-module(leviathan_cin2).

-export([build_cins/1,
         prepare/1]).

-define(LM_EMPTY, ?LM([], [])).
-define(LM(Cins, Conts), #{cins => Cins, conts => Conts}).

-define(MATCH_LM(Cins, Conts), #{cins := Cins, conts := Conts}).
-define(MATCH_EMPTY_LM, ?MATCH_LM([], [])).

-include("leviathan.hrl").

%% -------------------------------------------------------------------------------
%% Types
%% -------------------------------------------------------------------------------

-type cont_id() :: [{HostId :: string(), ContId :: string()}].
-type cin_map() :: #{cinID => string(),
                     cenIDs => [string()],
                     contIDs => [cont_id()] ,
                     ip_b => 0..255,
                     ip => inet:ip4_address() | no_ip}.
-type cont_map() :: #{contID => cont_id(),
                      cinID => string(),
                      ip => inet:ip4_address()}.
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

%% -----------------------------------------------------------------------------
%% Local Functions: building cins
%% -----------------------------------------------------------------------------

build_cin(CinId, CenIds, ?MATCH_LM(Cins, Conts)) ->
    CinMap = cin_map(CinId, CenIds),
    ContsMap = cont_maps(CinMap),
    ?LM([CinMap | Cins], ContsMap ++ Conts).

cin_map(CinId, [CenId] = CenIds) ->
    #{contIDs := ContIds, wire_type := WireType} = leviathan_dby:get_cen(CenId),
    IpB = next_cin_ip_b(),
    #{cinID => CinId,
      cenIDs => CenIds,
      contIDs => ContIds,
      ip_b => IpB,
      ip => cin_ip_address(IpB, WireType)}.

cont_maps(#{cinID := CinId, cenIDs := CenIds, contIDs := ContIds, ip_b := IpB}) ->
    ContToInterface = map_cont_to_interface(get_cen_wires(CenIds)),
    Fn = fun(ContId, ContCount) ->
                 {#{contID => ContId,
                    cinID => CinId,
                    interface_alias => maps:get(ContId, ContToInterface),
                    ip => cont_ip_address(IpB, ContCount)},
                  ContCount + 1}
         end,
    element(1, lists:mapfoldl(Fn, 1, ContIds)).

%% TODO: extract mkfn_create_cont_map/2

get_cen_wires(CenIds) ->
    lists:foldl(fun(CenId, Acc) ->
                        leviathan_dby:get_wires(CenId) ++ Acc
                end, [], CenIds).

map_cont_to_interface(Wires) ->
    map_cont_to_interface(Wires, #{}).

map_cont_to_interface([[#{dest := #{type := cont} = Dest}, _] | Rest],
                      Acc) ->
    map_cont_to_interface(Rest, add_cont_to_interface_mapping(Dest, Acc));
map_cont_to_interface([[_, #{dest := Dest}] | Rest], Acc) ->
    map_cont_to_interface(Rest, add_cont_to_interface_mapping(Dest, Acc));
map_cont_to_interface([], Acc) ->
    Acc.

add_cont_to_interface_mapping(#{id := ContId, alias := Alias}, Mappings) ->
    maps:put(ContId, Alias, Mappings).


%% -----------------------------------------------------------------------------
%% Local Functions:: preparing CINs
%% -----------------------------------------------------------------------------

do_prepare(?MATCH_LM(CinMaps, ContMaps)) ->
    set_cin_status(CinMaps, preparing),
    prepare_cins(CinMaps),
    prepare_conts(ContMaps),
    set_cin_status(CinMaps, ready).

set_cin_status(CinMaps, Status) ->
    lists:foreach(fun(#{cinID := CinId}) ->
                          leviathan_dby:set_cin_status(CinId, Status)
                  end, CinMaps).

prepare_cins(CinMaps) ->
    lists:foreach(fun(#{cenIDs := [Id], ip := Ip}) ->
                          prepare_cin(Id, Ip)
                  end, CinMaps).

prepare_cin(_, no_ip) ->
    ok;
prepare_cin(CenId, Ip) ->
    Cmd = leviathan_linux:set_bus_ip(CenId, Ip),
    leviathan_linux:eval(Cmd),
    ok.

prepare_conts(ContMaps) ->
    lists:foreach(
      fun(#{contID := {_HostId, BareContId}, ip := Ip, interface_alias := Alias}) ->
              prepare_cont(BareContId, Alias, Ip)
      end, ContMaps).

prepare_cont(ContId, Alias, Ip) ->
    CmdBundle = leviathan_linux:set_ip_address(ContId, Alias, Ip),
    leviathan_linux:eval(CmdBundle),
    ok.

%% -----------------------------------------------------------------------------
%% Local Functions: addressing
%% -----------------------------------------------------------------------------

next_cin_ip_b() ->
    leviathan_common_store:next_count(cin_ip_b, 10).

cin_ip_address(CenB, bus) ->
    inet_parse:ntoa({10, CenB, 0, 1});
cin_ip_address(_, _) ->
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


