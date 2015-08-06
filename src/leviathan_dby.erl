-module(leviathan_dby).

-on_load(install_iso8601/0).

-export([import_file/2,
         import_binary/2]).

-export([get_cen/2,
         get_cont/2,
         get_wires/2]).

-define(PUBLISHER, atom_to_binary(?MODULE, utf8)).

% -----------------------------------------------------------------------------
%
% API
%
% -----------------------------------------------------------------------------

% json file

import_file(Host, Filename) ->
    {ok, Binary} = file:read_file(Filename),
    import_binary(Host, Binary).

import_binary(Host, Binary) ->
    #{<<"cenList">> := Cens} = jiffy:decode(Binary, [return_maps]),
    import_cens(Host, Cens).

% getters

-spec get_cen(string(), string()) -> #{}.
get_cen(Host, CenId) ->
    dby:search(fun linked_containers/4,
        #{cenID => null,
         wire_type => null,
         contIDs => []},
        dby_cen_id(Host, CenId), [{max_depth, 1}]).

-spec get_cont(string(), string()) -> #{}.
get_cont(Host, ContId) ->
    dby:search(fun linked_cens/4,
        #{contID => null,
         cens => []},
        dby_cont_id(Host, ContId), [{max_depth, 1}]).

get_wires(_, #{wire_type := null}) ->
    [];
get_wires(Host, #{cenID := CenId, wire_type := bus}) ->
    dby:search(fun wires/4, [],
            dby_cen_id(Host, CenId), [breadth, {max_depth, 4}, {loop, link}]);
get_wires(Host, #{cenID := CenId, wire_type := wire}) ->
    dby:search(fun wires/4, [],
            dby_cen_id(Host, CenId), [depth, {max_depth, 4}, {loop, link}]).

% -----------------------------------------------------------------------------
%
% Internal functions
%
% -----------------------------------------------------------------------------

install_iso8601() ->
    {module, _} = dby:install(iso8601),
    ok.

% format for dobby

dby_id(List) ->
    dby_id(List, []).

dby_id([E], Acc) ->
    iolist_to_binary([Acc, E]);
dby_id([E | Rest], Acc) ->
    dby_id(Rest, [Acc, E, ">"]).

dby_cen_id(Host, CenId) ->
    dby_id([<<"lev_cen">>, Host, CenId]).

dby_bridge_id(Host, BridgeId) ->
    dby_id([<<"lev_bridge">>, Host, BridgeId]).

dby_cont_id(Host, ContId) ->
    dby_id([<<"lev_cont">>, Host, ContId]).

dby_endpoint_id(Host, Endpoint) ->
    dby_id([<<"lev_endpoint">>, Host, Endpoint]).

dby_cen(Host, CenId, Metadata) when is_binary(CenId) ->
    {dby_cen_id(Host, CenId), [{<<"cenID">>, CenId},
                               {<<"type">>, <<"cen">>}] ++ Metadata}.

dby_bridge(Host, BridgeId, Metadata) when is_binary(BridgeId) ->
    {dby_bridge_id(Host, BridgeId), [{<<"bridgeID">>, BridgeId},
                               {<<"type">>, <<"bridge">>}] ++ Metadata}.

dby_cont(Host, ContId, Metadata) when is_binary(ContId) ->
    {dby_cont_id(Host, ContId), [{<<"contID">>, ContId},
                                {<<"type">>, <<"container">>}] ++ Metadata}.

dby_endpoint(Host, EndID, Side, Metadata) when is_binary(EndID) ->
    {dby_endpoint_id(Host, EndID), [{<<"type">>, <<"endpoint">>},
                                      endpoint_side_md(Side),
                                     {<<"endID">>, EndID}] ++ Metadata}.

dby_cen_to_container(Host, CenId, ContId) ->
    dby_link(dby_cen_id(Host, CenId),
             dby_cont_id(Host, ContId), <<"part_of">>).

dby_endpoint_to_container(Host, EndpointId, ContId) ->
    dby_link(dby_endpoint_id(Host, EndpointId),
             dby_cont_id(Host, ContId), <<"bound_to">>).

dby_endpoint_to_bridge(Host, EndpointId, BridgeId) ->
    dby_link(dby_endpoint_id(Host, EndpointId),
                dby_bridge_id(Host, BridgeId), <<"bound_to">>).

dby_endpoint_to_endpoint(Host, EndpointId1, EndpointId2, Type) ->
    dby_link(dby_endpoint_id(Host, EndpointId1),
             dby_endpoint_id(Host, EndpointId2), Type).

% helper
dby_link(E1, E2, Type) ->
    {E1, E2, [{<<"type">>, Type}]}.

%% process decoded json

import_cens(Host, CensJson) ->
    Context0 = #{
        topublish => [],
        count => #{}
    },
    Context1 = container_from_cens_json(Context0, Host, CensJson),
    #{topublish := ToPublish} = cens_from_cens_json(Context1, Host, CensJson),
    dby:publish(?PUBLISHER, lists:flatten(ToPublish), [persistent]).

% prepare to publish the list of containers
container_from_cens_json(Context, Host, CensJson) ->
    ContSet = lists:foldl(
        fun(#{<<"containerIDs">> := ContainerIds}, Acc) ->
            sets:union(Acc, sets:from_list(ContainerIds));
           (_, _) ->
            throw(bad_json)
        end, sets:new(), CensJson),
    ToPublish = sets:fold(
        fun(ContId, Acc) ->
            [Acc, dby_cont(Host, ContId, [status_md(pending)])]
        end, [], ContSet),
    topublish(Context, ToPublish).

% prepare to publish cens and the wiring
cens_from_cens_json(Context0, Host, CensJson) ->
    lists:foldl(
        fun(#{<<"cenID">> := CenId, <<"containerIDs">> := ContIds}, Context) ->
            wire_cen(Context, Host, CenId, ContIds);
           (_, _) ->
            throw(bad_json)
        end, Context0, CensJson).

% wiring helpers

wire_cen(Context, Host, CenId, []) ->
    % no wire the CEN has no containers
    topublish(Context, [
        dby_cen(Host, CenId, [wire_type_md(null), status_md(pending)])
    ]);
wire_cen(Context, Host, CenId, [ContId]) ->
    % no wire if the CEN has zero or one containers
    topublish(Context, [
        dby_cen(Host, CenId, [wire_type_md(null), status_md(pending)]),
        dby_cen_to_container(Host, CenId, ContId)
    ]);
wire_cen(Context0, Host, CenId, [ContId1, ContId2]) ->
    % wire the containers directly if there are two containers in the CEN
    {Context1, ContId1InEndpoint} = next_in_endpoint(Context0, ContId1),
    {Context2, ContId2InEndpoint} = next_in_endpoint(Context1, ContId2),
    {Context3, Cont1Eth} = next_eth(Context2, ContId1),
    {Context4, Cont2Eth} = next_eth(Context3, ContId2),
    topublish(Context4, [
        dby_cen(Host, CenId, [wire_type_md(wire), status_md(pending)]),
        dby_cen_to_container(Host, CenId, ContId1),
        dby_cen_to_container(Host, CenId, ContId2),
        dby_endpoint(Host, ContId1InEndpoint, inside,
                                [alias_md(Cont1Eth), status_md(pending)]),
        dby_endpoint(Host, ContId2InEndpoint, inside,
                                [alias_md(Cont2Eth), status_md(pending)]),
        dby_endpoint_to_container(Host, ContId1InEndpoint, ContId1),
        dby_endpoint_to_container(Host, ContId2InEndpoint, ContId2),
        dby_endpoint_to_endpoint(Host, ContId1InEndpoint,
                                       ContId2InEndpoint, <<"connected_to">>)
    ]);
wire_cen(Context, Host, CenId, ContainerIds) ->
    Context1 = topublish(Context,
        [
            dby_cen(Host, CenId, [wire_type_md(bus), status_md(pending)]),
            dby_bridge(Host, CenId, [status_md(pending)])
        ]),
    lists:foldl(wire_cen_to_container(Host, CenId), Context1, ContainerIds).

wire_cen_to_container(Host, CenId) ->
    fun(ContId, Context0) ->
        {Context1, InEndpoint} = next_in_endpoint(Context0, ContId),
        {Context2, OutEndpoint} = next_out_endpoint(Context1, ContId),
        {Context3, Eth} = next_eth(Context2, ContId),
        topublish(Context3,
            [
                dby_cen_to_container(Host, CenId, ContId),
                dby_endpoint(Host, InEndpoint, inside,
                                        [alias_md(Eth), status_md(pending)]),
                dby_endpoint_to_container(Host, InEndpoint, ContId),
                dby_endpoint(Host, OutEndpoint, outside, [status_md(pending)]),
                dby_endpoint_to_endpoint(Host, InEndpoint,
                                               OutEndpoint, <<"veth_peer">>),
                dby_endpoint_to_bridge(Host, OutEndpoint, CenId)
            ])
    end.

% publish context helpers

% add to the list to publish to the end of the list.
topublish(Context = #{topublish := ToPublish}, AddToPublish) ->
    Context#{topublish := [ToPublish, AddToPublish]}.

% get next eth port for a container
next_eth(Context, ContId) ->
    next_count(Context, {eth, ContId}, fun eth_name/1).

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

% name formatters
eth_name(N) ->
    Nbinary = integer_to_binary(N),
    <<"eth", Nbinary/binary>>.

in_endpoint_name(ContId, N) ->
    endpoint_name(ContId, <<"i">>, N).

out_endpoint_name(ContId, N) ->
    endpoint_name(ContId, <<"o">>, N).

endpoint_name(ContId, Side, N) ->
    Nbinary = integer_to_binary(N),
    <<ContId/binary, $., Nbinary/binary, Side/binary>>.

% metadata helpers
alias_md(Alias) ->
    {<<"alias">>, Alias}.

status_md(pending) ->
    {<<"status">>, <<"pending">>}.

wire_type_md(null) ->
    {<<"wire_type">>, null};
wire_type_md(wire) ->
    {<<"wire_type">>, <<"wire">>};
wire_type_md(bus) ->
    {<<"wire_type">>, <<"bus">>}.

endpoint_side_md(inside) ->
    {<<"side">>, <<"in">>};
endpoint_side_md(outside) ->
    {<<"side">>, <<"out">>}.

md_wire_type(null) ->
    null;
md_wire_type(<<"wire">>) ->
    wire;
md_wire_type(<<"bus">>) ->
    bus.

% search

-define(MDVALUE(Key, Var), Key := #{value := Var}).

-define(MDTYPE(Type), ?MDVALUE(<<"type">>, Type)).

-define(MATCH_CONTAINER(ContId), #{?MDTYPE(<<"container">>),
                                   ?MDVALUE(<<"contID">>, ContId)}).

-define(MATCH_BRIDGE(BridgeId), #{?MDTYPE(<<"bridge">>),
                                  ?MDVALUE(<<"bridgeID">>, BridgeId)}).

-define(MATCH_CEN(CenId, WireType), #{?MDTYPE(<<"cen">>),
                                       ?MDVALUE(<<"cenID">>, CenId),
                                       ?MDVALUE(<<"wire_type">>, WireType)}).

-define(MATCH_IN_ENDPOINT(EndId, Alias), #{?MDTYPE(<<"endpoint">>),
                                          ?MDVALUE(<<"side">>, <<"in">>),
                                          ?MDVALUE(<<"endID">>, EndId),
                                          ?MDVALUE(<<"alias">>, Alias)}).

-define(MATCH_OUT_ENDPOINT(EndId), #{?MDTYPE(<<"endpoint">>),
                                     ?MDVALUE(<<"side">>, <<"out">>),
                                     ?MDVALUE(<<"endID">>, EndId)}).

% dby:search function to return list of containers linked to an identifier.
linked_containers(_, ?MATCH_CEN(CenId, WireType), [], Acc) ->
    {continue, Acc#{cenID := CenId,
                    wire_type := md_wire_type(WireType)}};
linked_containers(_, ?MATCH_CONTAINER(ContId), _, Acc) ->
    {continue, map_prepend(Acc, contIDs, binary_to_list(ContId))};
linked_containers(_, _, _, Acc) ->
    {continue, Acc}.

% dby:search function to return list of cens linked to an identifier.
linked_cens(_, ?MATCH_CONTAINER(ContId), [], Acc) ->
    {continue, Acc#{contID := ContId}};
linked_cens(_, ?MATCH_CEN(CenId, _), _, Acc) ->
    {continue, map_prepend(Acc, cens, binary_to_list(CenId))};
linked_cens(_, _, _, Acc) ->
    {continue, Acc}.

% dby:search function to return the list of wires
% looks for:
% if cen wire_type is bus:
%   bridge <-> endpoint (outside) <-> endpoint (inside) <-> cont
% if cen wire_type is wire:
%   cont <-> endpoint (inside) <-> endpoint (inside) <-> cont
wires(_, ?MATCH_CEN(_, WireType), [], Acc) ->
    case md_wire_type(WireType) of
        null ->
            {stop, Acc};
        bus ->
            {continue, fun wires_bus/4, Acc};
        wire ->
            {continue, fun wires_wire/4, Acc}
    end;
wires(_, _, _, Acc) ->
    {continue, Acc}.

%   bridge <-> endpoint (outside) <-> endpoint (inside) <-> cont
wires_bus(_, ?MATCH_BRIDGE(BridgeId),
                [{_, ?MATCH_OUT_ENDPOINT(OutEndId), _},
                 {_, ?MATCH_IN_ENDPOINT(InEndId, Alias), _},
                 {_, ?MATCH_CONTAINER(ContId), _} | _], Acc) ->
    Wire = [
        #{endID => binary_to_list(InEndId),
          dest => #{type => cont,
                    id => binary_to_list(ContId),
                    alias => binary_to_list(Alias)}},
        #{endID => binary_to_list(OutEndId),
          dest => #{type => cen,
                    id => binary_to_list(BridgeId)}}],
    {continue, [Wire | Acc]};
wires_bus(_, _, _, Acc) ->
    {continue, Acc}.

%   cont <-> endpoint (inside) <-> endpoint (inside) <-> cont
wires_wire(_, ?MATCH_CONTAINER(ContId1),
                [{_, ?MATCH_IN_ENDPOINT(EndId1, Alias1), _},
                 {_, ?MATCH_IN_ENDPOINT(EndId2, Alias2), _},
                 {_, ?MATCH_CONTAINER(ContId2), _} | _], Acc) ->
    Wire = [
        #{endID => binary_to_list(EndId1),
          dest => #{type => cont,
                    id => binary_to_list(ContId1),
                    alias => binary_to_list(Alias1)}},
        #{endID => binary_to_list(EndId2),
          dest => #{type => cont,
                    id => binary_to_list(ContId2),
                    alias => binary_to_list(Alias2)}}],
    {continue, [Wire | Acc]};
wires_wire(_, _, _, Acc) ->
    {continue, Acc}.

% map helpers

map_prepend(Map, Key, Value) ->
    {ok, OldList} = maps:find(Key, Map),
    maps:update(Key, [Value | OldList], Map).
