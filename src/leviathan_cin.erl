-module(leviathan_cin).

-export([build_cins/1,
         prepare/1,
         prepare_in_cluster/1,
         destroy/1,
         destroy_in_cluster/1]).

-define(LM_EMPTY, ?LM([], [])).
-define(LM(Cins, Conts), #{cins => Cins, conts => Conts}).

-define(MATCH_LM(Cins, Conts), #{cins := Cins, conts := Conts}).
-define(MATCH_EMPTY_LM, ?MATCH_LM([], [])).

-include("leviathan.hrl").

%% -------------------------------------------------------------------------------
%% Types
%% -------------------------------------------------------------------------------

-type cont_id() :: {HostId :: string(), ContId :: string()}.

-type addressing() :: #{CenId :: string() =>
                                 {InterfaceName :: string(),
                                  IpAddress :: inet:ip4_address() | no_ip}}.

-type cin_map() :: #{cinID => string(),
                     contIDs => [cont_id()] | [],
                     ip_b => 0..255,
                     addressing => addressing()}.

-type cont_map() :: #{contID => cont_id(),
                      cinID => string(),
                      addressing => addressing()}.

-type cin_lm() :: #{cins => [cin_map()] | [],
                    conts => [cont_map()] | []}.

-export_type([cont_id/0, cin_map/0, cont_map/0, cin_lm/0]).

%% -------------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------------

-spec build_cins(#{CinId :: string() => CenIds :: [string()]}) -> cin_lm().

build_cins(CinToCensMap) ->
    maps:fold(fun build_cin/3, ?LM_EMPTY, CinToCensMap).


-spec prepare(CinIds :: [string()]) -> ok.

prepare(CinIds) ->
    do_prepare(leviathan_cin_store:get_levmap(CinIds)).

prepare_in_cluster(CinIds) ->
    [rpc:call(N, leviathan_cin, prepare, [CinIds]) || N <- nodes()],
    prepare(CinIds).


-spec destroy(CinIds :: [string()]) -> ok.

destroy(CinIds) ->
    do_destroy(leviathan_cin_store:get_levmap(CinIds)).

destroy_in_cluster(CinIds) ->
    destroy(CinIds),
    [rpc:call(N, leviathan_cin, destroy, [CinIds]) || N <- nodes()].
    

%% -----------------------------------------------------------------------------
%% Local Functions: building CIN LM
%% -----------------------------------------------------------------------------

build_cin(CinId, CenIds, ?MATCH_LM(CinMaps, ContMaps)) ->
    Cens = get_cens(CenIds),
    CinMap = make_cin_map(CinId, Cens),
    ?LM([CinMap | CinMaps], make_cont_maps(CinMap, Cens) ++ ContMaps).

%% -----------------------------------------------------------------------------
%% Local Functions: building CIN maps
%% -----------------------------------------------------------------------------

make_cin_map(CinId, Cens) ->
    IpB = next_cin_ip_b(),
    #{cinID => CinId,
      contIDs => get_cen_containers(Cens),
      ip_b => IpB,
      master_cin_node => node(),
      addressing => make_cin_addressing(IpB, Cens)}.

get_cens(CenIds) ->
    lists:map(fun(CenId) -> leviathan_dby:get_cen(CenId) end, CenIds).

get_cen_containers(Cens) ->
    lists:foldl(fun(#{contIDs := ContIds}, Acc) ->
                        ContIds ++ Acc
                end, [], Cens).

make_cin_addressing(IpB, Cens) ->
    Fn = fun(#{cenID := CenId, wire_type := WireType, bridges := Bridges}, CinCount) ->
                 Gateway = get_cen_gateway_interface(Bridges),
                 Ip = cin_ip_address(IpB, WireType, CinCount),
                 {make_cin_addressing_item(CenId, Gateway, Ip),
                  CinCount + 1}
         end,
    {CinAddressing, _} = lists:mapfoldl(Fn, 1, Cens),
    maps:from_list(CinAddressing).

make_cin_addressing_item(CenId, {HostId, BridgeInterface}, Ip) ->
    {CenId, #{hostid => HostId, interface => BridgeInterface, ip => Ip}}.

get_cen_gateway_interface([First | _Rest]) ->
    First.

%% -----------------------------------------------------------------------------
%% Local Functions: building Container maps
%% -----------------------------------------------------------------------------

make_cont_maps(#{cinID := CinId, contIDs := ContIds, ip_b := IpB},
               Cens) ->
    ContToWires = map_cont_to_wires(get_cen_wires(Cens)),
    Fn = mkfn_make_cont_map(CinId, IpB, ContToWires),
    element(1, lists:mapfoldl(Fn, 1, ContIds)).

mkfn_make_cont_map(CinId, IpB, ContToWires) ->
    fun({_HostId, BareContId} = ContId, ContCount) ->
            ContWires = maps:get(BareContId, ContToWires),
            {#{contID => ContId,
               cinID => CinId,
               addressing => make_cont_addressing(IpB, ContCount,
                                                  ContWires)},
             ContCount + length(ContWires)}
    end.

make_cont_addressing(IpB, InitContCount, ContWires) ->
    Fn = fun([
              #{endID := EndId, dest := #{alias := ContInterface}},
              #{dest := #{id := CenId}}
             ], ContCount) ->
                 Ip = cont_ip_address(IpB, ContCount),
                 {make_cont_addressing_item(CenId,
                                            EndId,
                                            ContInterface,
                                            Ip),
                  ContCount + 1}
         end,
    {ContAdddressing, _} = lists:mapfoldl(Fn, InitContCount, ContWires),
    maps:from_list(ContAdddressing).

make_cont_addressing_item(CenId, EndId, ContInterface, Ip) ->
    {CenId, #{endID => EndId, interface => ContInterface, ip => Ip}}.

get_cen_wires(Cens) ->
    lists:foldl(fun(Cen, Acc) ->
                        leviathan_dby:get_wires(Cen) ++ Acc
                end, [], Cens).

map_cont_to_wires(Wires) ->
    map_cont_to_wires(Wires, #{}).

map_cont_to_wires(
  [[
    #{side := in, dest := #{id := ContId}} = InEnd,
    #{side := out} = OutEnd
   ] | Rest], Acc) ->
    map_cont_to_wires(Rest, map_cont_to_wires(ContId, [InEnd, OutEnd], Acc));
map_cont_to_wires(
  [[
    #{side := out} = OutEnd,
    #{side := in, dest := #{id := ContId}} = InEnd
   ] | Rest], Acc) ->
    map_cont_to_wires(Rest, map_cont_to_wires(ContId, [InEnd, OutEnd], Acc));
map_cont_to_wires([], Acc) ->
    Acc.

map_cont_to_wires(ContId, Wire, Acc) ->
    Wires = maps:get(ContId, Acc, []),
    maps:put(ContId, [Wire | Wires], Acc).


%% -----------------------------------------------------------------------------
%% Local Functions:: preparing CINs
%% -----------------------------------------------------------------------------

do_prepare(?MATCH_LM(CinMaps, ContMaps)) ->
    set_cin_status(CinMaps, preparing),
    prepare_cins(CinMaps),
    prepare_conts(ContMaps),
    set_cin_status(CinMaps, ready).

prepare_cins(CinMaps) ->
    lists:foreach(fun(#{master_cin_node := MasterCinNode,
                        addressing := Addressing})
                        when MasterCinNode =:= node() ->
                          prepare_cin(Addressing);
                     (_) ->
                          ok
                  end, CinMaps).

prepare_cin(CinAddressing) ->
    case maps:keys(CinAddressing) of
        [CenId] ->
            #{interface := BridgeInterface,
              ip := Ip} = maps:get(CenId, CinAddressing),
            Cmd = leviathan_linux:set_bus_ip(BridgeInterface, Ip),
            leviathan_linux:eval(Cmd);
        _ ->
            erlang:error(cins_spanning_cens_not_implemented)
    end.

prepare_conts(ContMaps) ->
    HostIdToNode = leviathan_cen:hostid_to_node([node()]),
    Fn = fun(#{contID := {HostId, BareContId},
               addressing := Addressing}) ->
                 case maps:get(HostId, HostIdToNode, undefined) of
                     N when N =:= node() ->
                         prepare_cont(BareContId, Addressing);
                     _ ->
                         ok
                 end;
            %% The below clause should be removed when the CEN layer is
            %% adjusted to the new ContId format that is {HostId, ContId}
            (#{contID := BareContId,
               addressing := Addressing}) ->
                 prepare_cont(BareContId, Addressing)
         end,
    lists:foreach(Fn, ContMaps).

prepare_cont(ContId, ContAddressing) ->
    case maps:keys(ContAddressing) of
        [CenId] ->
            #{interface := ContInterface,
              ip := Ip} = maps:get(CenId, ContAddressing),
            CmdBundle = leviathan_linux:set_ip_address(ContId,
                                                       ContInterface,
                                                       Ip),
            leviathan_linux:eval(CmdBundle);
        _ ->
            erlang:error(cins_spanning_cens_not_implemented)
    end.

%% -----------------------------------------------------------------------------
%% Local Functions: destroying CINs
%% -----------------------------------------------------------------------------

do_destroy(?MATCH_LM(CinMaps, ContMaps)) ->
    set_cin_status(CinMaps, destroy),
    destroy_cins(CinMaps),
    destroy_conts(ContMaps),
    set_cin_status(CinMaps, pending).

destroy_cins(CinMaps) ->
    lists:foreach(fun(#{master_cin_node := MasterCinNode,
                        addressing := Addressing})
                        when MasterCinNode =:= node() ->
                          destroy_cin(Addressing);
                     (_) ->
                          ok
                  end, CinMaps).

destroy_cin(CinAddressing) ->
    case maps:keys(CinAddressing) of
        [CenId] ->
            #{interface := BridgeInterface,
              ip := Ip} = maps:get(CenId, CinAddressing),
            Cmd = leviathan_linux:delete_bus_ip(BridgeInterface, Ip),
            leviathan_linux:eval(Cmd);
        _ ->
            erlang:error(cins_spanning_cens_not_implemented)
    end.

destroy_conts(ContMaps) ->
    HostIdToNode = leviathan_cen:hostid_to_node([node()]),
    Fn = fun(#{contID := {HostId, BareContId},
               addressing := Addressing}) ->
                 case maps:get(HostId, HostIdToNode, undefined) of
                     N when N =:= node() ->
                         destroy_cont(BareContId, Addressing);
                     _ ->
                         ok
                 end;
            %% The below clause should be removed when the CEN layer is
            %% adjusted to the new ContId format that is {HostId, ContId}
            (#{contID := BareContId,
               addressing := Addressing}) ->
                 destroy_cont(BareContId, Addressing)
         end,
    lists:foreach(Fn, ContMaps).

destroy_cont(ContId, ContAddressing) ->
    case maps:keys(ContAddressing) of
        [CenId] ->
            #{interface := ContInterface,
              ip := Ip} = maps:get(CenId, ContAddressing),
            CmdBundle = leviathan_linux:delete_ip_address(ContId,
                                                          ContInterface,
                                                          Ip),
            leviathan_linux:eval(CmdBundle);
        _ ->
            erlang:error(cins_spanning_cens_not_implemented)
    end.
    

%% -----------------------------------------------------------------------------
%% Local Functions: addressing
%% -----------------------------------------------------------------------------

next_cin_ip_b() ->
    leviathan_common_store:next_count(cin_ip_b, 10).

%% @doc Generate an IP address of a CIN.
%%
%% Current implementation supports only one bridge with an IP address
%% in a CIN.
cin_ip_address(CenB, bus, 1) ->
    inet_parse:ntoa({10, CenB, 0, 1});
cin_ip_address(_, _, _) ->
    no_ip.

%% Generate an IP address in the form:
%% 10.NN.C1.C2
%% where NN is in the range 10-250 and C1.C2 is 0.10-255.240
%% NN is derived from the IpB, C1.C2 from ContCount
%% IpB is the "B" part of the address computed by next_cen_ip_b/0
cont_ip_address(IpB, ContCount) when ContCount =< 65511 ->
    C = ContCount + 9, %% offset
    <<C1:8, C2:8>> = <<C:16>>,
    inet_parse:ntoa({10, IpB, C1, C2});
cont_ip_address(IpB, UsedIps) when is_list(UsedIps) andalso length(UsedIps) =< 6551 ->
    cont_ip_address(IpB, UsedIps, (length(UsedIps) + 1) rem 65511).

cont_ip_address(IpB, UsedIps, N) ->
    Ip = cont_ip_address(IpB, N),
    case lists:member(Ip, UsedIps) of
        false ->
            Ip;
        true ->
            cont_ip_address(IpB, UsedIps, N+1)
    end.

set_cin_status(CinMaps, Status) ->
    lists:foreach(fun(#{cinID := CinId}) ->
                          leviathan_dby:set_cin_status(CinId, Status)
                  end, CinMaps).
