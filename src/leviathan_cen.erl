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

-include("leviathan_logger.hrl").


%-------------------------------------------------------------------------------
% API
%-------------------------------------------------------------------------------

import_file(Host, Filename) ->
    LM = decode_file(Filename),
    ok = leviathan_store:import_cens(Host, LM),
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
%       leviathan_store:get_levmap(leviathan_cen:test_cens()).
% 4. test prepare:
%       leviathan_cen:test_local_prepare_lev(leviathan_cen:test_cens()).

                                                % return list of cens ids in the cen.json file
test_cens() ->
    ["cen1","cen2", "cen3", "cen4","cen5"].

% call main entry point to run prepare
test_local_prepare_lev(CenIds)->
    prepare_lev(leviathan_store:get_levmap(CenIds)).

% prepare cens from a list of cen ids
prepare(CenIds) ->
    prepare_lev(leviathan_store:get_levmap(CenIds)).

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
    prepare_bus(CenId, IpAddr);
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
    % mark cens as preparing
    cens_status(CensMap, preparing),
    prepare_cens(CensMap),
    prepare_conts(ContsMap),
    prepare_wires(WireMap),
    % mark cens as ready
    cens_status(CensMap, ready).

cens_status(#{cens := Cens}, Status) ->
    lists:foreach(
        fun(#{cenID := CenId}) ->
            leviathan_dby:set_cen_status(CenId, Status)
        end, Cens).

prepare_cens(#{cens := Cens}) ->
    %%     make any necessary Ethernet buses
    %%     if a Cen has more than 2 containers, we'll create a bus
    %%
    lists:foreach(
        fun(CenMap)->
            #{cenID := CenId,
             wire_type := CenType,
             ip_address := IpAddress} = CenMap,
            case CenType of
                bus ->
                    prepare_bus(CenId, IpAddress);
                _ ->
                    ok %% don't create a bus
            end
        end, Cens).

prepare_bus(CenId, IPAddress) ->
    CmdBundle = leviathan_linux:new_bus(CenId, IPAddress),
    leviathan_linux:eval(CmdBundle),
    ok.

prepare_conts(#{conts := Conts}) ->
    lists:foreach(
        fun(#{contID := ContId}) ->
            prepare_cont(ContId)
        end, Conts).

prepare_cont(ContId) ->
    CmdBundle = leviathan_linux:set_netns(ContId),
    leviathan_linux:eval(CmdBundle),
    ok.

prepare_wires(WireMap)->
    #{wires := Wires} = WireMap,
    lists:foreach(fun(Wire)->prepare_wire(Wire) end, Wires).

prepare_wire(Wire = [#{endID := EndId1}, #{endID := EndId2}]) ->
    CmdBundle = leviathan_linux:new_peer(EndId1, EndId2),
    leviathan_linux:eval(CmdBundle),				      
    lists:foreach(fun(WireEnd) -> prepare_wire_end(WireEnd) end, Wire).

prepare_wire_end(#{endID := EndId, dest := #{type := cen, id := CenId}}) ->
    CmdBundle = leviathan_linux:peer2cen(CenId,EndId),
    leviathan_linux:eval(CmdBundle);				      
prepare_wire_end(#{endID := EndId,
                dest := #{type := cont, id := ContId, alias := Alias}} = WireEnd) ->
    CmdBundle = leviathan_linux:peer2cont(ContId, EndId, Alias),
    leviathan_linux:eval(CmdBundle),
    case WireEnd of
	#{endID := EndId,
	  dest := Dest} ->
	    leviathan_cin:prepare_wire_end(Dest);
	_ -> ok  %% No IP Address
    end.

%% === DESTROY ===== %%%

% destroy cens from a list of cen ids
destroy(CenIds) ->
    destroy_lev(leviathan_store:get_levmap(CenIds)).

%
% Top Level Processor
%
destroy_lev(#{censmap := CensMap, wiremap := WireMap}) ->
    cens_status(CensMap, destroy),
    destroy_cens(CensMap),
    destroy_wires(WireMap),
    cens_status(CensMap, pending).

destroy_cens(#{cens := Cens}) ->
    %%     make any necessary Ethernet buses
    %%     if a Cen has more than 2 containers, we'll create a bus
    %%
    lists:foreach(
        fun(CenMap)->
            #{wire_type := CenType} = CenMap,
                case CenType of
                    bus ->
                        #{cenID := CenId} = CenMap,
                        destroy_bus(CenId);
                    _ ->
                        ok %% don't create a bus
                end
            end, Cens).

destroy_bus(CenId) ->
    CmdBundle = leviathan_linux:delete_bus(CenId),
    leviathan_linux:eval(CmdBundle),
    ok.

destroy_wires(WireMap)->
    #{wires := Wires} = WireMap,
    lists:foreach(fun(Wire)->destroy_wire(Wire) end, Wires).

destroy_wire(Wire) ->
    lists:foreach(fun(WireEnd) -> destroy_wire_end(WireEnd) end, Wire).

destroy_wire_end(#{endID := EndId, dest := #{type := cen}}) ->
    CmdBundle = leviathan_linux:delete_peer(EndId),
    leviathan_linux:eval(CmdBundle);				      
destroy_wire_end(#{dest := #{type := cont, id := ContId, alias := Alias}}) ->
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
    LM0 = leviathan_store:get_levmap([CenId]),
    LM1 = Fn(CenId, ContId, LM0),
    Deltas = lm_compare(LM0, LM1),
    ok = leviathan_store:update_cens(HostId, Deltas),
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
    LM0 = leviathan_store:get_levmap([CenId]),
    LM1 = add_cen(CenId, LM0),
    Deltas = lm_compare(LM0, LM1),
    ok = leviathan_store:update_cens(HostId, Deltas),
    ok = leviathan_dby:update_cens(HostId, Deltas),
    ok = prepare_deltas(Deltas).

% add cen to CEN maps
add_cen(CenId, LM = ?LM_CENS(Cens)) ->
    case lists:any(cenid_is(CenId), Cens) of
        true ->
            LM;
        false ->
            IpB = leviathan_cin:next_cenb(),
            LM?LM_SET_CENS([cen(CenId, bus, [], IpB) | Cens])
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
    fun(Cen = #{contIDs := ContIds0,
                ipaddr_b := IpB,
                reservedIps := ReservedIps0}) ->
            case lists:member(ContId, ContIds0) of
                true ->
                    [Cen];
                false ->
                    ContIds1 = ContIds0 ++ [ContId],
                    ReservedIps1 = ReservedIps0
                        ++ [next_ip_in_cen(IpB, ReservedIps0)],
                    [Cen#{contIDs := ContIds1, reservedIps := ReservedIps1}]
            end
    end.

next_ip_in_cen(IpB, ReservedIps0) ->
    binary_to_list(leviathan_cin:ip_address(IpB, ReservedIps0)).

%% add container to Cont map
add_container_to_contsmap(ContId, CenId, LM = ?LM_CONTS(Conts0)) ->
    Conts1 = update_contsmap(ContId, Conts0, mk_add_container_to_contsmap_fun(CenId)),
    LM?LM_SET_CONTS(Conts1).

mk_add_container_to_contsmap_fun(CenId) ->
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
    next_cont_id_number(ReservedIds, length(ReservedIds)).

next_cont_id_number(ReservedIds, IdCandidate) ->
    %% TODO: throw an exception when there're no id numbers left
    case lists:member(IdCandidate, ReservedIds) of
        false ->
            IdCandidate;
        true ->
            next_cont_id_number(ReservedIds,
                                (IdCandidate+1) rem (_DummyInterfacesLimit = 1000))
    end.

% Remove container from CEN
lm_remove_container(CenId, ContId, LM) ->
    LM0 = remove_container_from_censmap(CenId, ContId, LM),
    LM1 = remove_container_from_contsmap(ContId, CenId, LM0),
    lm_wire_cens(LM1).

% remove container from cens maps
remove_container_from_censmap(CenId, ContId, LM = ?LM_CENS(Cens0)) ->
    Cens1 = update_censmap(CenId, Cens0,
        fun(Cen = #{contIDs := ContIds0, reservedIps := ReservedIps0}) ->
            {ContIds1, ReservedIps1} =
                            list_delete2(ContId, ContIds0, ReservedIps0),
            [Cen#{contIDs := ContIds1, reservedIps := ReservedIps1}]
        end),
    LM?LM_SET_CENS(Cens1).

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

% Compare LMs
% Returns a list of instructions, list of:
% - {add, cen, CenMap}
% - {add, cont, ContMap}
% - {add, wire, Wire}
% - {add, cont_in_cen, {ContMap, CenMap}}
% - {add, bridge, {CenId, IpAddr}} XXX not used
% - {destroy, cen, CenMap}
% - {destroy, cont, ContMap}
% - {destroy, wire, Wire}
% - {destroy, cont_in_cen, {ContMap, CenMap}}
% - {destroy, bridge, CenId} XXX not used
% - {set, wire_type, {CenId, WireType}}
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
    OldCensMap = cens_map(OldCens),
    NewCensMap = cens_map(NewCens),
    ?LM_CONTS(OldConts) = OldLM,
    ?LM_CONTS(NewConts) = NewLM,
    OldContsMap = conts_map(OldConts),
    NewContsMap = conts_map(NewConts),
    CommonKeys = maps:keys(maps:with(maps:keys(OldCensMap), NewCensMap)),
    MkInstructionsFun =
        fun(Op, ContIds, CenId, ContsMap, CensMap) ->
                CenMap = maps:get(CenId, CensMap),
                instructions(
                  Op,
                  cont_in_cen,
                  [{maps:get(ContId, ContsMap), CenMap} || ContId <- ContIds])
        end,
    lists:map(
      fun(CenId) ->
              #{contIDs := OldList} = maps:get(CenId, OldCensMap),
              #{contIDs := NewList} = maps:get(CenId, NewCensMap),
              {ToRemove, ToAdd} = compare_lists(OldList, NewList),
              [
               MkInstructionsFun(destroy, ToRemove, CenId, OldContsMap, OldCensMap),
               MkInstructionsFun(add, ToAdd, CenId, NewContsMap, NewCensMap)
              ]
      end, CommonKeys).

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
    Cens = lists:foldl(
        fun(#{<<"cenID">> := Cen, <<"containerIDs">> := Conts}, Acc) ->
            [cen(binary_to_list(Cen),
                 bus,
                 Conts,
                 leviathan_cin:next_cenb()) | Acc]
        end, [], CensJson),
    Cens.

cen(Cen, WireType, Conts, IpAddrB) ->
    #{cenID => Cen,
      wire_type => WireType,
      contIDs => list_binary_to_list(Conts),
      ipaddr_b => IpAddrB,
      ip_address => binary_to_list(leviathan_cin:bridge_ip_address(IpAddrB)),
      reservedIps => list_binary_to_list(container_ip_addrs(IpAddrB, Conts))}.

container_ip_addrs(IpAddrB, Conts) ->
    lists:map(
        fun(Count) ->
            leviathan_cin:ip_address(IpAddrB, Count)
        end, lists:seq(1, length(Conts))).

% conts
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
    #{contID => binary_to_list(Cont),
      cens => list_binary_to_list(Cens),
      reservedIdNums => cont_id_numbers(Cens)}.

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
wire_cen(Cen = #{cenID := CenId}, ContsInfo) ->
    IpAddrs = ipaddrs_for_conts(Cen, ContsInfo),
    lists:foldl(mk_wire_cont_to_cen_fun(CenId),
                [], lists:zip(ContsInfo, IpAddrs)).

ipaddrs_for_conts(#{reservedIps := ReservedIps, contIDs := ContIds},
                  ContsInfo) ->
    lists:map(
      fun({ContId, _}) ->
              list_lookup2(ContId, ContIds, ReservedIps)
      end, ContsInfo).

mk_wire_cont_to_cen_fun(CenId) ->
    fun({{ContId, Id}, Ip}, Acc) ->
            Wire = [mk_in_endpoint(CenId, ContId, Id, Ip),
                    mk_out_endpoint(CenId, ContId, Id)],
            [Wire| Acc]
    end.

mk_in_endpoint(CenId, ContId, IdNumber, Ip) ->
    #{endID => in_endpoint_name(ContId, IdNumber),
      side => in,
      dest => #{type => cont,
                id => ContId,
                alias => CenId,
                ip_address => Ip}
     }.

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
list_delete2(Key, Keys, Values) ->
    list_delete2(Key, Keys, Values, [], []).

list_delete2(_, [], [], NewKeys, NewValues) ->
    {lists:reverse(NewKeys), lists:reverse(NewValues)};
list_delete2(Key, [Key | Keys], [_ | Values], NewKeys, NewValues) ->
    list_delete2(Key, Keys, Values, NewKeys, NewValues);
list_delete2(_, [Key | Keys], [Value | Values], NewKeys, NewValues) ->
    list_delete2(Key, Keys, Values, [Key | NewKeys], [Value | NewValues]).

% name formatters
in_endpoint_name(ContId, N) ->
    endpoint_name(ContId, "i", N).

out_endpoint_name(ContId, N) ->
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
