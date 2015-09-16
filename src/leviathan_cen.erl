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
%       leviathan_cen:get_levmap(leviathan_cen:test_cens()).
% 4. test prepare:
%       leviathan_cen:test_local_prepare_lev(leviathan_cen:test_cens()).

% return list of cens ids in the cen.json file
test_cens() ->
    ["cen1","cen2", "cen3", "cen4","cen5"].

% call main entry point to run prepare
test_local_prepare_lev(CenIds)->
    prepare_lev(get_levmap(CenIds)).

% build the top level data structure for prepare; pulls data from dobby
get_levmap(CenIds) ->
    Cens = get_cens(CenIds),
    #{censmap => #{cens => Cens},
      contsmap => #{conts => get_conts(Cens)},
      wiremap => #{wires => get_wiremaps(Cens)}
    }.

get_cens(CenIds) ->
    lists:foldl(
        fun(CenId, Acc) ->
            case leviathan_dby:get_cen(CenId) of
                #{cenID := null} ->
                    Acc;
                Cen ->
                    [Cen | Acc]
            end
        end, [], CenIds).

% XXX host is hardcoded
get_conts(Cens) ->
    ContIds = contids_from_cens(Cens),
    [leviathan_dby:get_cont("host1", ContId) || ContId <- ContIds].

% XXX host is hardcoded
% leviathan_dby:get_wires/1 returns the list of wires per Cen. Flatten
% the list with lists:append/1 rather than with lists:flatten/1 because
% the wires themselves are lists.
get_wiremaps(Cens) ->
    lists:append([leviathan_dby:get_wires(Cen) || Cen <- Cens]).

% make a list of unique container ids by inspecting the cens
contids_from_cens(Cens) ->
    sets:to_list(lists:foldl(
        fun(#{contIDs := ContIds}, Acc) ->
            sets:union(Acc, sets:from_list(ContIds))
        end, sets:new(), Cens)).

% prepare cens from a list of cen ids
prepare(CenIds) ->
    prepare_lev(get_levmap(CenIds)).

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
    destroy_lev(get_levmap(CenIds)).

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
    LM0 = get_levmap([CenId]),
    LM1 = Fn(CenId, ContId, LM0),
    Deltas = lm_compare(LM0, LM1),
    ok = leviathan_dby:update_cens(HostId, Deltas),
    ok = prepare_deltas(Deltas).

% Add container to LM
lm_add_container(CenId, ContId, LM0) ->
    LM1 = add_cen(CenId, LM0),
    LM2 = add_container_to_censmap(CenId, ContId, LM1),
    LM3 = add_container(ContId, LM2),
    LM4 = add_container_to_contsmap(ContId, CenId, LM3),
    lm_wire_cens(LM4).

-record(leviathan_cen, {cen :: string(),
                        data :: #{
                          contIDs => [string()],
                          wire_type => atom(),
                          ipaddr => string()
                         }}).

%% connection betgween cen and container
-record(leviathan_cont, {cont :: string(),
                         cen :: string(),
                         data :: #{
                           idnumber => integer(),
                           ip_address => string()
                          }}).
        

% add cen
lm_add_cen(HostId, CenId) ->
    LM0 = get_levmap([CenId]),
    LM1 = add_cen(CenId, LM0),
    Deltas = lm_compare(LM0, LM1),
    ok = leviathan_dby:update_cens(HostId, Deltas),
    ok = prepare_deltas(Deltas).

% add cen to CEN maps
add_cen(CenId, LM = ?LM_CENS(Cens)) ->
    case lists:any(cenid_is(CenId), Cens) of
        true ->
            LM;
        false ->
            Ip = leviathan_dby:get_next_cin_ip(),
            ad_add_cen(CenId, Ip),
            LM?LM_SET_CENS([cen(CenId, null, [],Ip) | Cens])
            
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
    #{contID => ContId, cens => []}.

% add container to CEN map
add_container_to_censmap(CenId, ContId, LM = ?LM_CENS(Cens0)) ->
    Cens1 = update_censmap(CenId, Cens0,
        fun(Cen = #{contIDs := ContIds0}) ->
            ContIds1 = list_add_unique(ContId, ContIds0),
            [Cen#{contIDs := ContIds1, wire_type := wire_type(ContIds1)}]
        end),
    ad_add_container(CenId, ContId),
    LM?LM_SET_CENS(Cens1).

% add container to Cont map
add_container_to_contsmap(ContId, CenId, LM = ?LM_CONTS(Conts0)) ->
    Conts1 = update_contsmap(ContId, Conts0,
        fun(Cont) ->
            [maps_append_unique(cens, CenId, Cont)]
        end),
    LM?LM_SET_CONTS(Conts1).

% Remove container from CEN
lm_remove_container(CenId, ContId, LM) ->
    LM0 = remove_container_from_censmap(CenId, ContId, LM),
    LM1 = remove_container_from_contsmap(ContId, CenId, LM0),
    lm_wire_cens(LM1).

% remove container from cens maps
remove_container_from_censmap(CenId, ContId, LM = ?LM_CENS(Cens0)) ->
    Cens1 = update_censmap(CenId, Cens0,
        fun(Cen = #{contIDs := ContIds0}) ->
            ContIds1 = lists:delete(ContId, ContIds0),
            [Cen#{contIDs := ContIds1, wire_type := wire_type(ContIds0)}]
        end),
    LM?LM_SET_CENS(Cens1).

% remove container from Cont map
remove_container_from_contsmap(ContId, CenId, LM = ?LM_CONTS(Conts0)) ->
    Conts1 = update_contsmap(ContId, Conts0,
        fun(Cont = #{cens := Cens0}) ->
            case lists:delete(CenId, Cens0) of
                [] ->
                    [];
                Cens1 ->
                    [Cont#{cens := Cens1}]
            end
        end),
    LM?LM_SET_CONTS(Conts1).

% Rewire the CENs
lm_wire_cens(LM = ?LM_CENS(Cens)) ->
    Wires = wire_cens(Cens),
    LM?LM_SET_WIRES(Wires).

% Compare LMs
% Returns a list of instructions, list of:
% - {add, cen, CenMap}
% - {add, cont, ContMap}
% - {add, wire, Wire}
% - {add, cont_in_cen, {ContId, CenId}}
% - {add, bridge, {CenId, IpAddr}}
% - {destroy, cen, CenMap}
% - {destroy, cont, ContMap}
% - {destroy, wire, Wire}
% - {destroy, cont_in_cen, {ContId, CenId}}
% - {destroy, bridge, CenId}
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

compare_cens_containers(?LM_CENS(OldCens), ?LM_CENS(NewCens)) ->
    OldMap = cens_map(OldCens),
    NewMap = cens_map(NewCens),
    CommonKeys = maps:keys(maps:with(maps:keys(OldMap), NewMap)),
    lists:map(
        fun(CenId) ->
            #{contIDs := OldList} = maps:get(CenId, OldMap),
            #{contIDs := NewList} = maps:get(CenId, NewMap),
            Ipaddr = maps:get(ip_address, maps:get(CenId, NewMap), null),
            {ToRemove, ToAdd} = compare_lists(OldList, NewList),
            [
                instructions(destroy, cont_in_cen,
                    [{ContId, CenId}|| ContId <- ToRemove]),
                instructions(add, cont_in_cen,
                    [{ContId, CenId} || ContId <- ToAdd]),
                set_wiretype(CenId, wire_type(OldList),
                                    wire_type(NewList)),
                set_bridge(CenId, Ipaddr,
                                    wire_type(OldList), wire_type(NewList))
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

set_bridge(_, _, WireType, WireType) ->
    % nothing changed
    [];
set_bridge(CenId, Ipaddr, _, bus) ->
    % add bridge
    {add, bridge, {CenId, Ipaddr}};
set_bridge(CenId, _, bus, _) ->
    % remove bridge
    {destroy, bridge, CenId};
set_bridge(_, _, _, _) ->
    % ignore any other transition
    [].

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
    Wires = wire_cens(Cens),
    #{
        censmap => #{cens => Cens},
        contsmap => #{conts => Conts},
        wiremap => #{wires => Wires}
    }.

% cens
cens_from_jiffy(CensJson) ->
    Cens = lists:foldl(
        fun(#{<<"cenID">> := Cen, <<"containerIDs">> := Conts}, Acc) ->
            [cen(binary_to_list(Cen),
                 wire_type(Conts),
                 Conts,
                 leviathan_dby:get_next_cin_ip()) | Acc]
        end, [], CensJson),
    Cens.

wire_type(Conts) when length(Conts) < 2 ->
    bus;
    %null;
wire_type(Conts) when length(Conts)  == 2 ->
    bus;
    %wire
wire_type(Conts) when length(Conts)  > 2 ->
    bus.

% XXX ignore wiretype for now and always use bus
cen(Cen, _WireType, Conts, IpAddr) ->
     #{cenID => Cen,
%      wire_type => WireType,
       wire_type => bus,
       contIDs => list_binary_to_list(Conts),
       ip_address => binary_to_list(IpAddr)}.

% conts
conts_from_jiffy(CensJson) ->
    Pairs = cen_cont_pairs(CensJson),
    Index = lists:foldl(
        fun ({Cen, Cont}, Acc) ->
            maps_append(Cont, Cen, Acc)
        end, #{}, Pairs),
    maps:fold(
        fun(Cont, Cens, Acc) ->
            [#{contID => binary_to_list(Cont),
               cens => list_binary_to_list(Cens)} | Acc]
        end, [], Index).

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
    maps:put(Key, list_add_unique(Value, Old), Map).

wire_cens(Cens) ->
    #{wires := Wires} = lists:foldl(
        fun(#{cenID := CenId, contIDs := ContIds, ip_address := IpAddr}, Context) ->
            wire_cen(Context, cen_b(IpAddr), CenId, ContIds)
        end, #{cen_b => undefined, count => #{}, wires => []}, Cens),
    Wires.

% wiring helpers

wire_cen(Context, _, _, []) ->
    Context;
wire_cen(Context, _, _, [_]) ->
    Context;
%wire_cen(Context0, CenId, [ContId1, ContId2]) ->
%    % wire the containers directly if there are two containers in the CEN
%    Context1 = count_cont(Context0, CenId),
%    {Context2, ContId1InEndpoint} = next_in_endpoint(Context1, ContId1),
%    Cont1IpAddr = ip_addr(Context2, CenId),
%    Context3 = count_cont(Context2, CenId),
%    {Context4, ContId2InEndpoint} = next_in_endpoint(Context3, ContId2),
%    Cont2IpAddr = ip_addr(Context4, CenId),
%    maps_append(wires, [#{
%        endID => ContId1InEndpoint,
%        side => in,
%        dest => #{
%                    type => cont,
%                    id => ContId1,
%                    alias => CenId,
%                    ip_address => Cont1IpAddr
%                }
%     },
%     #{
%        endID => ContId2InEndpoint,
%        side => in,
%        dest => #{
%                    type => cont,
%                    id => ContId2,
%                    alias => CenId,
%                    ip_address => Cont2IpAddr
%                }
%     }
%    ], Context4);
wire_cen(Context, CenB, CenId, ContainerIds) ->
    lists:foldl(wire_cen_to_container(CenId, CenB), Context, ContainerIds).

wire_cen_to_container(CenId, CenB) ->
    fun(ContId, Context0) ->
            %% if ContId wired with CenId then skip
            Context1 = count_cont(Context0, CenId),
            {Context2, InEndpoint} = next_in_endpoint(Context1, ContId),
            {Context3, OutEndpoint} = next_out_endpoint(Context2, ContId),
            IpAddr = ip_addr(Context3, CenB, CenId),
            maps_append(wires, [#{
                                   endID => InEndpoint,
                                   side => in,
                                   dest => #{
                                     type => cont,
                                     id => ContId,
                                     alias => CenId,
                                     ip_address => IpAddr
                                    }
                                 },
                                #{
                                   endID => OutEndpoint,
                                   side => out,
                                   dest => #{
                                     type => cen,
                                     id => CenId
                                    }
                                 }
                               ], Context3)
    end.

% publish context helpers

% set B network for Cen
cen_b(IpAddr) ->
    {ok, {_, CenB, _, _}} = inet:parse_address(IpAddr),
    CenB.

% mark next container in cen
count_cont(Context, CenId) ->
    {Context1, _} = next_count(Context, {conts, CenId}, fun(_) -> ok end),
    Context1.

% get next inside port for a container
next_in_endpoint(Context, ContId) ->
    FormatFn = fun(N) -> in_endpoint_name(ContId, N) end,
    next_count(Context, {in_endpoint, ContId}, FormatFn).

% get next outside port for a container
next_out_endpoint(Context, ContId) ->
    FormatFn = fun(N) -> out_endpoint_name(ContId, N) end,
    next_count(Context, {out_endpoint, ContId}, FormatFn).

% helper
next_count(Context = #{count := CountMap}, Key, FormatFn) ->
    N = maps:get(Key, CountMap, 0),
    {maps:update(count, maps:put(Key, N + 1,  CountMap), Context),
     FormatFn(N)}.

% format ip addr
ip_addr(#{count := CountMap}, CenB, CenId) ->
    ContCount = maps:get({conts, CenId}, CountMap),
    binary_to_list(leviathan_cin:ip_address(CenB, ContCount)).

% name formatters
in_endpoint_name(ContId, N) ->
    endpoint_name(ContId, "i", N).

out_endpoint_name(ContId, N) ->
    endpoint_name(ContId, "o", N).

endpoint_name(ContId, Side, N) ->
    lists:flatten([ContId, $., integer_to_list(N), Side]).

list_binary_to_list(List) ->
    lists:map(fun binary_to_list/1, List).

list_add_unique(Element, List) ->
    case lists:member(Element, List) of
        false ->
            List ++ [Element];
        true ->
            List
    end.

%%% Authoritative data

%% add cen to authoritative data
ad_add_cen(CenId, Ip) ->
    Fn = fun() ->
                 CenData = #{contIDs => [], wirte_type => bus,
                             ipaddr => binary_to_list(Ip)},
                 Cen = #leviathan_cen{cen = CenId, data = CenData},
                 mnesia:write(Cen)
         end,
    {atomic, ok} = mnesia:transaction(Fn).


%% add container to authoritative data
ad_add_container(CenId, ContId) ->
    io:format("SIEMA ~p", [eloooooooo]),
    Fn = fun() ->
                 ContData = #{idnumber => undefined, ip_address => undefined},
                 Cont = #leviathan_cont{cont = ContId, cen = CenId,
                                        data = ContData},
                 mnesia:write(Cont)
         end,
    {atomic, ok} = mnesia:transaction(Fn).

