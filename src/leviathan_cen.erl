-module(leviathan_cen).

-compile(export_all).

-export([import_file/2,
         decode_file/1,
         decode_binary/1,
         remove_container_from_cen/3,
         add_container_to_cen/3,
         destroy_cen/1,
         new_cen/2]).

-ifdef(TEST).
-export([decode_jiffy/1]).
-endif.

-define(MAX_INTERFACE_ID, 1000).

-include("leviathan_logger.hrl").
-include_lib("kernel/include/inet.hrl").


%-------------------------------------------------------------------------------
% API
%-------------------------------------------------------------------------------

import_file(Host, Filename) ->
    LM = decode_file(Filename),
    ok = leviathan_cen_store:import_cens(Host, LM),
    ok = leviathan_dby:import_cens(Host, LM).

decode_file(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    decode_binary(Binary).

decode_binary(Binary) ->
    #{<<"cenList">> := Cens} = jiffy:decode(Binary, [return_maps]),
    decode_jiffy(Cens).

% Add a container to a CEN
add_container_to_cen(HostId, ContainerId, CenId) ->
    ?INFO("Add container to cen: Container(~s, ~s), Cen(~s)",
                                            [HostId, ContainerId, CenId]),
    lm_update_cens(HostId, CenId, ContainerId, fun lm_add_container/3).

% Remove a container from a CEN
remove_container_from_cen(HostId, ContainerId, CenId) ->
    ?INFO("Remove container from cen: Container(~s, ~s), Cen(~s)",
                                            [HostId, ContainerId, CenId]),
    lm_update_cens(HostId, CenId, ContainerId, fun lm_remove_container/3).

% Create new CEN
new_cen(HostId, CenId) ->
    ?INFO("Create cen: Cen(~s)", [CenId]),
    lm_add_cen(HostId, CenId).
    
% Destroy CEN
destroy_cen(CenId) ->
    % XXX not implemented
    ?INFO("Remove cen: Cen(~s)", [CenId]),
    ok.

% To test:
                                                % 1. load the cen.json file in this repo via leviathan_cen:import_file/2
%    or use curl and the REST interface (see leviathan_rest_lib).
%    The host name must be "host1"
% 2. test_cens/0 returns the cen IDs of the cens in the .json file, so
%    you can use that to save typing
% 3. (optional) inspect the levmap:
%       leviathan_cen_store:get_levmap(leviathan_cen:test_cens()).
% 4. test prepare:
%       leviathan_cen:test_local_prepare_lev(leviathan_cen:test_cens()).

                                                % return list of cens ids in the cen.json file
test_cens() ->
    ["cen1","cen2", "cen3", "cen4","cen5"].

% call main entry point to run prepare
test_local_prepare_lev(CenIds)->
    prepare_lev(leviathan_cen_store:get_levmap(CenIds)).

prepare_in_cluster(CenIds) ->
    [rpc:call(N, leviathan_cen, prepare, [CenIds]) || N <- nodes()],
    prepare(CenIds).

% prepare cens from a list of cen ids
prepare(CenIds) ->
    prepare_lev(leviathan_cen_store:get_levmap(CenIds)).

prepare_deltas(Deltas) ->
    ok = lists:foreach(fun prepare_instruction/1, Deltas).

prepare_instruction({add, cen, _}) ->
    % no-op
    ok;
prepare_instruction({add, cont, #{contID := ContId}}) ->
    prepare_cont(ContId);
prepare_instruction({add, wire, Wire}) ->
    prepare_wire(Wire);
prepare_instruction({add, cont_in_cen, _}) ->
    % no-op
    ok;
prepare_instruction({add, bridge, {CenId, IpAddr}}) ->
    prepare_bus(CenId);
prepare_instruction({destroy, cen, _}) ->
    % no-op
    ok;
prepare_instruction({destroy, cont, _}) ->
    % no-op
    ok;
prepare_instruction({destroy, wire, Wire}) ->
    destroy_wire(Wire);
prepare_instruction({destroy, cont_in_cen, _}) ->
    % no-op
    ok;
prepare_instruction({destroy, bridge, CenId}) ->
    destroy_bus(CenId);
prepare_instruction({set, wire_type, _}) ->
    % no-op
    ok.

%
% Top Level Processor
%

prepare_lev(#{censmap := CensMap, contsmap := ContsMap, wiremap := WireMap}) ->
    prepare_cens(CensMap),
    prepare_conts(ContsMap),
    prepare_wires(WireMap).

prepare_cens(#{cens := Cens}) ->
    %% make any necessary Ethernet buses
    %% if a Cen has more than 2 containers, we'll create a bus
    Fn = fun(#{wire_type := CenType} = CenMap) ->
                 case  CenType of
                     bus ->
                         prepare_cen(CenMap);
                     _ ->
                         ok %% don't create a bus
                 end
         end,
    lists:foreach(Fn, Cens).

prepare_cen(#{bridges := Bridges, hostid_to_node := HostIdToNode} = CenMap) ->
    Fn = fun({HostId, BrId}) ->
                 case maps:get(HostId, HostIdToNode) of
                     Node when Node =:= node() ->
                         prepare_bus(BrId),
                         prepare_tunnel_interfaces(HostId, CenMap),
                         prepare_tunnels(HostId, CenMap);
                     _ ->
                         ok
                 end
         end,
    lists:foreach(Fn, Bridges).

prepare_tunnel_interfaces(ThisHostId, #{tunnels := Tunnels,
                                        bridges := Bridges}) ->
    Bridge = proplists:get_value(ThisHostId, Bridges),
    Fn = fun({#{hostid := HostId, tap_no := TapNo}, _})
               when HostId =:= ThisHostId ->
                 prepare_tunnel_interface(TapNo, Bridge);
            ({_, #{hostid := HostId, tap_no := TapNo}})
               when HostId =:= ThisHostId ->
                 prepare_tunnel_interface(TapNo, Bridge);
            (_) ->
                 ok
         end,
    lists:foreach(Fn, Tunnels).

prepare_tunnel_interface(TapNo, Bridge) ->
    Intf = "tap" ++ integer_to_list(TapNo),
    CmdBundle = leviathan_linux:new_tap(Intf)
        ++ leviathan_linux:tap2bridge(Intf, Bridge),
    leviathan_linux:eval(CmdBundle),
    ok.


prepare_tunnels(HostId, #{master_hostid := MasterHostId,
                          tunnels := Tunnels})
  when HostId =:= MasterHostId ->
    Fn = fun({#{tap_no := TapNoA},
              #{hostid := HostB, tap_no := TapNoB}}) ->
                 TunnelUser = application:get_env(leviathan_lib, tunnel_user, undefined),
                 CmdBundle = leviathan_linux:new_tunnel(atom_to_list(TunnelUser),
                                                        integer_to_list(TapNoA),
                                                        integer_to_list(TapNoB),
                                                        HostB),
                 leviathan_linux:eval(CmdBundle),
                 ok
         end,
    lists:foreach(Fn, Tunnels);
prepare_tunnels(_, _) ->
    ok.
        


prepare_bus(CenId) ->
    CmdBundle = leviathan_linux:new_bus(CenId),
    leviathan_linux:eval(CmdBundle),
    ok.

prepare_conts(#{conts := Conts}) ->
    HostIdToNode = hostid_to_node([node()|nodes()]),
    lists:foreach(
      fun(#{contID := {HostId, ContId}}) ->
              case maps:get(HostId, HostIdToNode) of
                  Node when Node =:= node() ->
                      prepare_cont(ContId);
                  _ ->
                      %% the other host container
                      ok
              end
      end, Conts).

prepare_cont(ContId) ->
    CmdBundle = leviathan_linux:set_netns(ContId),
    leviathan_linux:eval(CmdBundle),
    ok.

prepare_wires(WireMap)->
    #{wires := Wires} = WireMap,
    lists:foreach(fun(Wire)->prepare_wire(Wire) end, Wires).

prepare_wire([#{dest := #{type := cont, id := {HostId, _ContId}}}, _] = Wire) ->
    HostIdToNode = hostid_to_node([node()]),
    case maps:get(HostId, HostIdToNode, undefined) of
        undefined ->
            ok;
        _ ->
            prepare_wire2(Wire)
    end;
prepare_wire([_, #{dest := #{type := cont, id := {HostId, _ContId}}}] = Wire) ->
    HostIdToNode = hostid_to_node([node()]),
    case maps:get(HostId, HostIdToNode, undefined) of
        undefined ->
            ok;
        _ ->
            prepare_wire2(Wire)
    end.
    
prepare_wire2(Wire = [#{endID := EndId1}, #{endID := EndId2}]) ->
    CmdBundle = leviathan_linux:new_peer(EndId1, EndId2),
    leviathan_linux:eval(CmdBundle),				      
    lists:foreach(fun(WireEnd) -> prepare_wire_end(WireEnd) end, Wire).

prepare_wire_end(#{endID := EndId, dest := #{type := cen, id := CenId}}) ->
    CmdBundle = leviathan_linux:peer2cen(CenId,EndId),
    leviathan_linux:eval(CmdBundle);				      
prepare_wire_end(#{endID := EndId,
                dest := #{type := cont, id := {_HostId, ContId}, alias := Alias}}) ->
    CmdBundle = leviathan_linux:peer2cont(ContId, EndId, Alias),
    leviathan_linux:eval(CmdBundle).

%% === DESTROY ===== %%%

% destroy cens from a list of cen ids
destroy(CenIds) ->
    destroy_lev(leviathan_cen_store:get_levmap(CenIds)).

destroy_in_cluster(CenIds) ->
    destroy(CenIds),
    [rpc:call(N, leviathan_cen, destroy, [CenIds]) || N <- nodes()].
    

%
% Top Level Processor
%
destroy_lev(#{censmap := CensMap, wiremap := WireMap}) ->
    destroy_cens(CensMap),
    destroy_wires(WireMap).

destroy_cens(#{cens := Cens}) ->
    %%     make any necessary Ethernet buses
    %%     if a Cen has more than 2 containers, we'll create a bus
    %%
    Fn = fun(#{wire_type := bus} = CenMap) ->
                 destroy_cen2(CenMap);
            (_) ->
                 ok
         end,
    lists:foreach(Fn, Cens).

destroy_cen2(#{bridges := Bridges, hostid_to_node := HostIdToNode} = CenMap) ->
    Fn = fun({HostId, BrId}) ->
                 case maps:get(HostId, HostIdToNode) of
                     Node when Node =:= node() ->
                         destroy_tunnels(HostId, CenMap),
                         destroy_tunnel_interfaces(HostId, CenMap),
                         destroy_bus(BrId);
                     _ ->
                         ok
                 end
         end,
    lists:foreach(Fn, Bridges).

destroy_tunnels(HostId, #{master_hostid := MasterHostId,
                          tunnels := Tunnels})
  when HostId =:= MasterHostId ->
    Fn = fun({
               #{tap_no := TapNoA},
               #{hostid := HostB, tap_no := TapNoB}
             }) ->
                 TunnelUser = application:get_env(leviathan_lib, tunnel_user, undefined),
                 CmdBundle = leviathan_linux:delete_tunnel(atom_to_list(TunnelUser),
                                                           integer_to_list(TapNoA),
                                                           integer_to_list(TapNoB),
                                                           HostB),
                 leviathan_linux:eval(CmdBundle),
                 ok
         end,
    lists:foreach(Fn, Tunnels);
destroy_tunnels(_, _) ->
    ok.

destroy_tunnel_interfaces(ThisHostId, #{tunnels := Tunnels}) ->
    Fn = fun({#{hostid := HostId, tap_no := TapNo}, _}) when HostId == ThisHostId ->
                 destroy_tunnel_interface(TapNo);
            ({_, #{hostid := HostId, tap_no := TapNo}}) when HostId == ThisHostId ->
                 destroy_tunnel_interface(TapNo);
            (_) ->
                 ok
         end,
    lists:foreach(Fn, Tunnels).

destroy_tunnel_interface(TapNo) ->
    destroy_tunnel_interface(TapNo, 4).

destroy_tunnel_interface(_, 0) ->
    ok;
destroy_tunnel_interface(TapNo, Retries) ->
    Intf = "tap" ++ integer_to_list(TapNo),
    CmdBundle = leviathan_linux:delete_tap(Intf),
    case leviathan_linux:eval(CmdBundle, output) of
        [] ->
            ok;
        _ ->
            timer:sleep(500),
            destroy_tunnel_interface(TapNo, Retries - 1)
    end.



destroy_bus(CenId) ->
    CmdBundle = leviathan_linux:delete_bus(CenId),
    leviathan_linux:eval(CmdBundle),
    ok.

destroy_wires(WireMap)->
    #{wires := Wires} = WireMap,
    lists:foreach(fun(Wire)->destroy_wire(Wire) end, Wires).

destroy_wire([#{dest := #{type := cont, id := {HostId, _ContId}}}, _] = Wire) ->
    HostIdToNode = hostid_to_node([node()]),
    case maps:get(HostId, HostIdToNode, undefined) of
        undefined ->
            ok;
        _ ->
            destroy_wire2(Wire)
    end;
destroy_wire([_, #{dest := #{type := cont, id := {HostId, _ContId}}}] = Wire) ->
    HostIdToNode = hostid_to_node([node()]),
    case maps:get(HostId, HostIdToNode, undefined) of
        undefined ->
            ok;
        _ ->
            destroy_wire2(Wire)
    end.

destroy_wire2(Wire) ->
    lists:foreach(fun(WireEnd) -> destroy_wire_end(WireEnd) end, Wire).

destroy_wire_end(#{endID := EndId, dest := #{type := cen}}) ->
    CmdBundle = leviathan_linux:delete_peer(EndId),
    leviathan_linux:eval(CmdBundle);				      
destroy_wire_end(#{dest := #{type := cont, id := {_HostId, ContId}, alias := Alias}}) ->
    CmdBundle = leviathan_linux:delete_cont_interface(ContId,Alias),
    leviathan_linux:eval(CmdBundle).

% -----------------------------------------------------------------------------
%
% Manipulate the Leviathan Map structure
%
% -----------------------------------------------------------------------------

-define(LM_VALUE(Map, Key, Value), #{Map := #{Key := Value}}).
-define(LM_CENS(Value), ?LM_VALUE(censmap, cens, Value)).
-define(LM_CONTS(Value), ?LM_VALUE(contsmap, conts, Value)).
-define(LM_WIRES(Value), ?LM_VALUE(wiremap, wires, Value)).

-define(LM_SET(Map, Key, Value), #{Map := #{Key => Value}}).
-define(LM_SET_CENS(Value), ?LM_SET(censmap, cens, Value)).
-define(LM_SET_CONTS(Value), ?LM_SET(contsmap, conts, Value)).
-define(LM_SET_WIRES(Value), ?LM_SET(wiremap, wires, Value)).

% Update a Cen with Fn.
lm_update_cens(HostId, CenId, ContId, Fn) ->
    LM0 = leviathan_cen_store:get_levmap([CenId]),
    LM1 = Fn(CenId, ContId, LM0),
    Deltas = lm_compare(LM0, LM1),
    ok = leviathan_cen_store:update_cens(HostId, Deltas),
    ok = leviathan_dby:update_cens(HostId, Deltas),
    ok = prepare_deltas(Deltas).

% Add container to LM
lm_add_container(CenId, ContId, LM0) ->
    LM1 = add_cen(CenId, LM0),
    LM2 = add_container_to_censmap(CenId, ContId, LM1),
    LM3 = add_container(ContId, LM2),
    LM4 = add_container_to_contsmap(ContId, CenId, LM3),
    lm_wire_cens(LM4).

% add cen
lm_add_cen(HostId, CenId) ->
    LM0 = leviathan_cen_store:get_levmap([CenId]),
    LM1 = add_cen(CenId, LM0),
    Deltas = lm_compare(LM0, LM1),
    ok = leviathan_cen_store:update_cens(HostId, Deltas),
    ok = leviathan_dby:update_cens(HostId, Deltas),
    ok = prepare_deltas(Deltas).

% add cen to CEN maps
add_cen(CenId, LM = ?LM_CENS(Cens)) ->
    case lists:any(cenid_is(CenId), Cens) of
        true ->
            LM;
        false ->
            LM?LM_SET_CENS([cen(CenId, bus, [], #{}) | Cens])
    end.

% returns filter function matching CenId
cenid_is(MatchCenId) ->
    fun(#{cenID := CenId}) ->
        MatchCenId == CenId
    end.

%add container to Cont maps
add_container(ContId, LM = ?LM_CONTS(Conts)) ->
    case lists:any(contid_is(ContId), Conts) of
        true ->
            LM;
        false ->
            LM?LM_SET_CONTS([new_cont_map(ContId)|Conts])
    end.

% returns filter function matching ContId
contid_is(MatchContId) ->
    fun(#{contID := ContId}) ->
        MatchContId == ContId
    end.

new_cont_map(ContId) ->
    #{contID => ContId, cens => [], reservedIdNums => []}.

% add container to CEN map
add_container_to_censmap(CenId, ContId, LM = ?LM_CENS(Cens0)) ->
    Cens1 = update_censmap(CenId, Cens0, mk_add_container_to_censmap_fun(ContId)),
    LM?LM_SET_CENS(Cens1).

mk_add_container_to_censmap_fun(ContId) ->
    fun(Cen = #{contIDs := ContIds0}) ->
            case lists:member(ContId, ContIds0) of
                true ->
                    [Cen];
                false ->
                    ContIds1 = ContIds0 ++ [ContId],
                    [Cen#{contIDs := ContIds1}]
            end
    end.

%% add container to Cont map
add_container_to_contsmap(ContId, CenId, LM = ?LM_CONTS(Conts0)) ->
    Conts1 = update_contsmap(ContId, Conts0, mk_add_container_to_contsmap_fn(CenId)),
    LM?LM_SET_CONTS(Conts1).

mk_add_container_to_contsmap_fn(CenId) ->
    fun(Cont = #{cens := Cens0, reservedIdNums := ReserverdIdNums0}) ->
            case lists:member(CenId, Cens0) of
                true ->
                    [Cont];
                false ->
                    ReserverdIdNums1 = ReserverdIdNums0
                        ++ [next_cont_id_num(ReserverdIdNums0)],
                    Cens1 = Cens0 ++ [CenId],
                    [Cont#{cens := Cens1, reservedIdNums := ReserverdIdNums1}]
            end
    end.

next_cont_id_num(ReservedIds) ->
    %% TODO: throw an exception when there're no IPs left
    next_cont_id_num(ReservedIds, length(ReservedIds)).

next_cont_id_num(ReservedIds, IdCandidate) ->
    %% TODO: throw an exception when there're no id numbers left
    case lists:member(IdCandidate, ReservedIds) of
        false ->
            IdCandidate;
        true ->
            next_cont_id_num(ReservedIds,
                             (IdCandidate+1) rem ?MAX_INTERFACE_ID)
    end.

% Remove container from CEN
lm_remove_container(CenId, ContId, LM) ->
    LM0 = remove_container_from_censmap(CenId, ContId, LM),
    LM1 = remove_container_from_contsmap(ContId, CenId, LM0),
    lm_wire_cens(LM1).

% remove container from cens maps
remove_container_from_censmap(CenId, ContId, LM = ?LM_CENS(Cens0)) ->
    Fn = fun(Cen = #{contIDs := ContIds}) ->
                    [Cen#{contIDs := lists:delete(ContId, ContIds)}]
            end,
    LM?LM_SET_CENS(update_censmap(CenId, Cens0, Fn)).

% remove container from Cont map
remove_container_from_contsmap(ContId, CenId, LM = ?LM_CONTS(Conts0)) ->
    Conts1 = update_contsmap(ContId, Conts0,
        fun(Cont = #{cens := Cens0, reservedIdNums := ReservedIds0}) ->
            {Cens1, ReservedIds1} =
                list_delete2(CenId, Cens0, ReservedIds0),
            if
                Cens1 == [] -> [];
                true -> [Cont#{cens := Cens1, reservedIdNums := ReservedIds1}]
            end
        end),
    LM?LM_SET_CONTS(Conts1).

% Rewire the CENs
lm_wire_cens(LM) ->
    ?LM_CONTS(Conts) = LM,
    ?LM_CENS(Cens) = LM,
    Wires = wire_cens(Cens, Conts),
    LM?LM_SET_WIRES(Wires).

%% Compare LMs
%% Returns a list of instructions, list of:
%% - {add, cen, CenMap}
%% - {add, cont, ContMap}
%% - {add, wire, Wire}
%% - {add, cont_in_cen, {ContMap, CenMap}}
%% - {add, bridge, {CenId, IpAddr}} XXX not used
%% - {destroy, cen, CenMap}
%% - {destroy, cont, ContMap}
%% - {destroy, wire, Wire}
%% - {destroy, cont_in_cen, {ContMap, CenMap}}
%% - {destroy, bridge, CenId} XXX not used
%% - {set, wire_type, {CenId, WireType}}
%% Regarding the add instructions there are 5 base cases:
%% 1) Add an empty Cen:
%%    {add, cen, CenMap}
%% 2) Add a new Cen and assign an existing Cont to it:
%%    {add, cen, CenMap}
%%    {add, cont_in_cen, {ContMap, CenMap}}
%%    {add, wire, Wire} (it is not added if Cen has only one Cont)
%% 3) Add a new Cen and new Cont in it:
%%    {add, cen, CenMap}
%%    {add, cont, ContMap}
%%    {add, cont_in_cen, {ContMap, CenMap}}
%%    {add, wire, Wire} (it is not added if Cen has only one Cont)
%% 4) Add a new Cont to an existing Cen
%%    {add, cont, ContMap}
%%    {add, cont_in_cen, {ContMap, CenMap}}
%%    {add, wire, Wire} (it is not added if Cen has only one Cont)
%% 5) Add an existing Cont to an existing Cen
%%    {add, cont_in_cen, {ContMap, CenMap}}
%%    {add, wire, Wire} (it is not added if Cen has only one Cont)
%% Regarding the destroy instructions there are 3 base cases:
%% 1) Destroy an empty Cen:
%%    {destroy, cen, Cen}
%% 2) Destroy a Cen with Cont:
%%    {destroy, cen, CenMap}
%%    {destroy, cont, ContMap}
%%    {destroy, cont_in_cen, {ContMap, CenMap}}
%%    {destroy, wire, Wires} (providing the Cen had more than one Cont)
%% 3) Destroy a Cont in Cen:
%%    {destroy, cont, ContMap}
%%    {destroy, cont_in_cen, {ContMap, CenMap}}
%%    {destroy, wire, Wire} (providing the Cen had more than one Cont)
lm_compare(Old, New) ->
    lists:flatten([
        compare_cens(Old, New),
        compare_cens_containers(Old, New),
        compare_conts(Old, New),
        compare_wires(Old, New)
    ]).

compare_cens(?LM_CENS(OldCens), ?LM_CENS(NewCens)) ->
    delta_instructions(cen, cens_map(OldCens), cens_map(NewCens)).

compare_cens_containers(OldLM = ?LM_CENS(OldCens), NewLM = ?LM_CENS(NewCens)) ->
    ?LM_CONTS(OldConts) = OldLM,
    ?LM_CONTS(NewConts) = NewLM,
    [OldCensMap, NewCensMap] = [cens_map(C) || C <- [OldCens, NewCens]],
    [OldContsMap, NewContsMap] = [conts_map(C) || C <- [OldConts, NewConts]],
    MkInstructionsFun =
        fun(_, [], _) ->
                [];
           (Op, ContIds, CenId) ->
                {CensMap, ContsMap} = case Op of
                                          destroy -> {OldCensMap, OldContsMap};
                                          add -> {NewCensMap, NewContsMap}
                                      end,
                CenMap = maps:get(CenId, CensMap),
                instructions(Op, cont_in_cen,
                             [{maps:get(CId, ContsMap), CenMap} || CId <- ContIds])
        end,
    [begin
         #{contIDs := OldList} = maps:get(CenId, OldCensMap, #{contIDs => []}),
         #{contIDs := NewList} = maps:get(CenId, NewCensMap, #{contIDs => []}),
         {ToRemove, ToAdd} = compare_lists(OldList, NewList),
         [
          MkInstructionsFun(destroy, ToRemove, CenId),
          MkInstructionsFun(add, ToAdd, CenId)
         ]
     end || CenId <- lists:usort(maps:keys(NewCensMap) ++ maps:keys(OldCensMap))].

cens_map(Cens) ->
    map_from_list(Cens, fun(#{cenID := CenId}) -> CenId end).

compare_conts(?LM_CONTS(OldConts), ?LM_CONTS(NewConts)) ->
    delta_instructions(cont, conts_map(OldConts), conts_map(NewConts)).

conts_map(Conts) ->
    map_from_list(Conts, fun(#{contID := ContId}) -> ContId end).

set_wiretype(_, Wiretype, Wiretype) ->
    [];
set_wiretype(CenId, _, NewWiretype) ->
    {set, wire_type, {CenId, NewWiretype}}.

compare_wires(?LM_WIRES(OldWires), ?LM_WIRES(NewWires)) ->
    delta_instructions(wire, wires_map(OldWires), wires_map(NewWires)).

wires_map(Wires) ->
    map_from_list(Wires,
        fun([#{endID := End1}, #{endID := End2}]) ->
            normalize_ends({End1, End2})
        end).

normalize_ends({End1, End2}) when End1 > End2 ->
    {End1, End2};
normalize_ends({End1, End2}) ->
    {End2, End1}.

delta_instructions(Item, OldMap, NewMap) ->
    {OldOnly, NewOnly} = compare_maps(OldMap, NewMap),
    [
        instructions(destroy, Item, OldOnly),
        instructions(add, Item, NewOnly)
    ].

compare_maps(OldMap, NewMap) ->
    OldOnlyMap = maps:without(maps:keys(NewMap), OldMap),
    NewOnlyMap = maps:without(maps:keys(OldMap), NewMap),
    {maps:values(OldOnlyMap), maps:values(NewOnlyMap)}.

compare_lists(OldList, NewList) ->
    OldSet = sets:from_list(OldList),
    NewSet = sets:from_list(NewList),
    OldOnlySet = sets:subtract(OldSet, NewSet),
    NewOnlySet = sets:subtract(NewSet, OldSet),
    {sets:to_list(OldOnlySet), sets:to_list(NewOnlySet)}.

instructions(Operation, Item, List) ->
    lists:map(fun(Element) -> {Operation, Item, Element} end, List).

node_cen_ids(Node, ?LM_CENS(Cens)) ->
    Fn = fun(#{cenID := CenId, hostid_to_node := HostIdToNode}, Acc) ->
                 case lists:member(Node, maps:values(HostIdToNode)) of
                     true ->
                         [CenId | Acc];
                     _ ->
                         Acc
                 end
         end,
    lists:foldl(Fn, [], Cens).

map_cen_id_to_host(?LM_CENS(Cens)) ->
    Fn = fun(#{cenID := CenId, hostid_to_node := HostIdToNode}, Acc) ->
                 maps:put(CenId, maps:keys(HostIdToNode), Acc)
         end,
    lists:foldl(Fn, #{}, Cens).

% helpers

map_from_list(List, KeyFn) ->
    lists:foldl(
        fun(Element, Acc) ->
            Key = KeyFn(Element),
            maps:put(Key, Element, Acc)
        end, #{}, List).

update_censmap(CenId, Cens, UpdateFn) ->
    update_map(cenID, CenId, Cens, UpdateFn).

update_contsmap(ContId, Conts, UpdateFn) ->
    update_map(contID, ContId, Conts, UpdateFn).

update_map(KeyField, Key, List, UpdateFn) ->
    update_map(KeyField, Key, List, UpdateFn, []).

update_map(_, _, [], _, Acc) ->
    Acc;
update_map(KeyField, Key, [Element | Rest], UpdateFn, Acc) ->
    case maps:get(KeyField, Element) of
        Key ->
            lists:reverse(Acc) ++ UpdateFn(Element) ++ Rest;
        _ ->
            update_map(KeyField, Key, Rest, UpdateFn, [Element | Acc])
    end.

% -----------------------------------------------------------------------------
%
% Decode JSON
%
% -----------------------------------------------------------------------------

%% process decoded json
decode_jiffy(CensJson) ->
    ?DEBUG("CensJson:~n~p~n",[CensJson]),
    Cens = cens_from_jiffy(CensJson),
    Conts = conts_from_jiffy(CensJson),
    #{
        censmap => #{cens => Cens},
        contsmap => #{conts => Conts},
        wiremap => #{wires => wire_cens(Cens, Conts)}
    }.

% cens
cens_from_jiffy(CensJson) ->
    HostIdToNode = hostid_to_node([node() | nodes()]),
    lists:foldl(
      fun(#{<<"cenID">> := Cen, <<"containerIDs">> := Conts}, Acc) ->
              [cen(binary_to_list(Cen), bus, Conts, HostIdToNode) | Acc]
      end, [], CensJson).
    

cen(Cen, WireType, Conts, HostIdToNode0) ->
    Fn = fun(Cont, Acc) ->
                 CenCont = {HostId, _} = cen_cont_id(Cont),
                 maps:is_key(HostId, HostIdToNode0) orelse
                     throw({unknown_host, HostId}),
                 {CenCont, sets:add_element(HostId, Acc)}
         end,
    {CenConts, HostIds} = lists:mapfoldl(Fn, sets:new(), Conts),
    HostIdToNode1 = maps:with(sets:to_list(HostIds), HostIdToNode0),
    [{MasterHostId, _} | _] = maps:to_list(HostIdToNode1),
    TapsCntBase =
        leviathan_common_store:next_count(taps_cnt_base, 100, 100),
    #{cenID => Cen,
      wire_type => WireType,
      contIDs => CenConts,
      bridges => [{HostId, Cen} || HostId <- maps:keys(HostIdToNode1)],
      master_hostid => MasterHostId,
      hostid_to_node => HostIdToNode1,
      taps_cnt_base => TapsCntBase,
      tunnels => cen_tunnels(MasterHostId, TapsCntBase, HostIdToNode1)}.

cen_tunnels(MasterHostId, TapsCntBase, HostIdToNode) ->
    MasterTunnelEndpointFn =
        fun(Cnt) ->
                tunnel_endpoint(Cnt,
                                MasterHostId,
                                maps:get(MasterHostId, HostIdToNode))
        end,
    Fn = fun(HostId, Node, {Cnt, Acc}) ->
                 {Cnt + 1,
                  [{MasterTunnelEndpointFn(Cnt),
                    tunnel_endpoint(Cnt, HostId, Node)}
                   | Acc]}
         end, 
    {_, Tunnels} = maps:fold(Fn,
                             {TapsCntBase + 1, []},
                             maps:without([MasterHostId], HostIdToNode)),
    Tunnels.

tunnel_endpoint(Cnt, HostId, Node) ->
    #{hostid => HostId,
      node => Node,
      tap_no => Cnt}.


conts_from_jiffy(CensJson) ->
    Pairs = cen_cont_pairs(CensJson),
    Index = lists:foldl(
        fun ({Cen, Cont}, Acc) ->
            maps_append(Cont, Cen, Acc)
        end, #{}, Pairs),
    maps:fold(
        fun(Cont, Cens, Acc) ->
            [cont(Cont, Cens) | Acc]
        end, [], Index).

cont(Cont, Cens) ->
    #{contID => cen_cont_id(Cont),
      cens => list_binary_to_list(Cens),
      reservedIdNums => cont_id_numbers(Cens)}.

cen_cont_id(ContFromJson) ->
    [HostId] = maps:values(ContFromJson),
    [ContId] = maps:keys(ContFromJson),
    {binary_to_list(HostId), binary_to_list(ContId)}.

cont_id_numbers(Cens) ->
    lists:seq(0, length(Cens) - 1).

cen_cont_pairs(CensJson) ->
    lists:foldl(
        fun(#{<<"cenID">> := CenId, <<"containerIDs">> := ContainerIds}, Acc) ->
            make_pair(CenId, ContainerIds, Acc);
           (_, _) ->
            throw(bad_json)
        end, [], CensJson).

make_pair(Const, List, Acc) ->
    lists:foldl(
        fun(Element, Acc0) ->
            [{Const, Element} | Acc0]
        end, Acc, List).

maps_append(Key, Value, Map) ->
    Old = maps:get(Key, Map, []),
    maps:put(Key, [Value | Old], Map).

maps_append_unique(Key, Value, Map) ->
    Old = maps:get(Key, Map, []),
    maps:put(Key, list_append_unique(Value, Old), Map).

%% wiring helpers

wire_cens(Cens, Conts) ->
    Fn =
        fun(Cen = #{cenID := CenId,
                    contIDs := ContIds}, Acc) when length(ContIds) > 1 ->
            IdNumbers = id_numbers(CenId, ContIds, Conts),
            wire_cen(Cen, lists:zip(ContIds, IdNumbers)) ++ Acc;
           (_, Acc) ->
            Acc
         end,
    lists:foldl(Fn, [], Cens).

id_numbers(CenId, ContIds, Conts) ->
    ContsMap = lists:foldl(
        fun(Cont = #{contID := ContId}, Acc) ->
            case id_number_for_cen(CenId, Cont) of
                not_found ->
                    Acc;
                Id ->
                    maps:put(ContId, Id, Acc)
            end
        end, #{}, Conts),
    lists:map(
        fun(ContId) ->
            maps:get(ContId, ContsMap)
        end, ContIds).

id_number_for_cen(CenId, #{cens := Cens, reservedIdNums := ReservedIds}) ->
    list_lookup2(CenId, Cens, ReservedIds).

% ContInfo :: [{contid, id}]
wire_cen(#{cenID := CenId}, ContsInfo) ->
    lists:foldl(mk_wire_cont_to_cen_fun(CenId), [], ContsInfo).

mk_wire_cont_to_cen_fun(CenId) ->
    fun({ContId, Id}, Acc) ->
            Wire = [mk_in_endpoint(CenId, ContId, Id),
                    mk_out_endpoint(CenId, ContId, Id)],
            [Wire| Acc]
    end.

mk_in_endpoint(CenId, ContId, IdNumber) ->
    #{endID => in_endpoint_name(ContId, IdNumber),
      side => in,
      dest => #{type => cont,
                id => ContId,
                alias => CenId}}.

mk_out_endpoint(CenId, ContId, IdNumber) ->
    #{endID => out_endpoint_name(ContId, IdNumber),
      side => out,
      dest => #{type => cen,
                id => CenId}
     }.

% find the key in the first list argument and return the corresponding
% value from the second list
list_lookup2(_, [], []) ->
    not_found;
list_lookup2(Key, [Key | _], [Value | _]) ->
    Value;
list_lookup2(Key, [_ | Keys], [_ | Values]) ->
    list_lookup2(Key, Keys, Values).

% delete the key in the first list argument and the corresponding element
% from the second list.
list_delete2(ToRemove, Keys, Values) ->
    list_delete2(ToRemove, Keys, Values, [], []).

list_delete2(_, [], [], NewKeys, NewValues) ->
    {lists:reverse(NewKeys), lists:reverse(NewValues)};
list_delete2(ToRemove, [ToRemove | Keys], [_ | Values], NewKeys, NewValues) ->
    list_delete2(ToRemove, Keys, Values, NewKeys, NewValues);
list_delete2(ToRemove, [OtherKey | Keys], [Value | Values], NewKeys, NewValues) ->
    list_delete2(ToRemove, Keys, Values, [OtherKey | NewKeys], [Value | NewValues]).

% name formatters
in_endpoint_name({_HostId, ContId}, N) ->
    endpoint_name(ContId, "i", N).

out_endpoint_name({_HostId, ContId}, N) ->
    endpoint_name(ContId, "o", N).

endpoint_name(ContId, Side, N) ->
    lists:flatten([ContId, $., integer_to_list(N), Side]).

list_binary_to_list(List) ->
    lists:map(fun binary_to_list/1, List).

list_append_unique(Element, List) ->
    case lists:member(Element, List) of
        false ->
            List ++ [Element];
        true ->
            List
    end.

hostid_to_node(Nodes) ->
    L = [begin
             [_, HostName] = string:tokens(atom_to_list(N), "@"),
             {ok, #hostent{h_addr_list = Ips}} =
                 inet:gethostbyname(HostName),
             [{inet:ntoa(Ip), N} || Ip <- Ips]
         end || N <- Nodes],
    maps:from_list(lists:flatten(L)).
