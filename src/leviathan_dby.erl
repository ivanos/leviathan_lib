-module(leviathan_dby).

-on_load(install_iso8601/0).

-export([import_file/2,
         import_binary/2]).

-export([get_cen/2,
         get_cont/2]).

-define(PUBLISHER, atom_to_binary(?MODULE, utf8)).
-define(MDVALUE(Key, Value), #{Key := #{value := Value}}).

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
    DobbyCenId = dby_cen_id(Host, CenId),
    ContIds = dby:search(fun linked_containers/4, [], DobbyCenId, [{max_depth, 1}]),
    #{cenID => CenId, contIDs => ContIds}.

-spec get_cont(string(), string()) -> #{}.
get_cont(Host, ContId) ->
    DobbyContId = dby_cont_id(Host, ContId),
    CenInfo = dby:search(fun linked_cens/4, [], DobbyContId, [{max_depth, 1}]),
    #{contID => ContId, cens => CenInfo}.

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

dby_cont_id(Host, ContId) ->
    dby_id([<<"lev_cont">>, Host, ContId]).

dby_endpoint_id(Host, Endpoint) ->
    dby_id([<<"lev_endpoint">>, Host, Endpoint]).

dby_cen(Host, CenId, WireType) when is_binary(CenId) ->
    {dby_cen_id(Host, CenId), [{<<"CenID">>, CenId},
                               {<<"type">>, <<"cen">>},
                               {<<"wire_type">>, WireType}]}.

dby_cont(Host, ContId) when is_binary(ContId) ->
    {dby_cont_id(Host, ContId), [{<<"contID">>, ContId},
                                {<<"type">>, <<"container">>}]}.

dby_endpoint(Host, EndID, Alias) when is_binary(EndID) ->
    {dby_endpoint_id(Host, EndID), [{<<"type">>, <<"part_of">>},
                                   {<<"alias">>, Alias}]}.

dby_cen_to_container(Host, CenId, ContId) ->
    next_to_link(dby_cen_id(Host, CenId), dby_cont_id(Host, ContId)).

dby_endpoint_to_container(Host, EndpointId, ContId) ->
    next_to_link(dby_endpoint_id(Host, EndpointId), dby_cont_id(Host, ContId)).

dby_endpoint_to_cen(Host, EndpointId, CenId) ->
    next_to_link(dby_endpoint_id(Host, EndpointId), dby_cen_id(Host, CenId)).

dby_endpoint_to_endpoint(Host, EndpointId1, EndpointId2) ->
    next_to_link(dby_endpoint_id(Host, EndpointId1), dby_endpoint_id(Host, EndpointId2)).

% helper
next_to_link(E1, E2) ->
    {E1, E2, [{<<"type">>, <<"next_to">>}]}.

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
            [Acc, dby_cont(Host, ContId)]
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
        dby_cen(Host, CenId, null)
    ]);
wire_cen(Context, Host, CenId, [ContId]) ->
    % no wire if the CEN has zero or one containers
    topublish(Context, [
        dby_cen(Host, CenId, null),
        dby_cen_to_container(Host, CenId, ContId)
    ]);
wire_cen(Context0, Host, CenId, [ContId1, ContId2]) ->
    % wire the containers directly if there are two containers in the CEN
    {Context1, ContId1Endpoint} = next_in_endpoint(Context0, ContId1),
    {Context2, ContId2Endpoint} = next_in_endpoint(Context1, ContId2),
    {Context3, Cont1Eth} = next_eth(Context2, ContId1),
    {Context4, Cont2Eth} = next_eth(Context3, ContId2),
    topublish(Context4, [
        dby_cen(Host, CenId, <<"wire">>),
        dby_cen_to_container(Host, CenId, ContId1),
        dby_cen_to_container(Host, CenId, ContId2),
        dby_endpoint(Host, ContId1Endpoint, Cont1Eth),
        dby_endpoint(Host, ContId2Endpoint, Cont2Eth),
        dby_endpoint_to_container(Host, ContId1Endpoint, ContId1),
        dby_endpoint_to_container(Host, ContId2Endpoint, ContId2),
        dby_endpoint_to_endpoint(Host, ContId1Endpoint, ContId2Endpoint)
    ]);
wire_cen(Context, Host, CenId, ContainerIds) ->
    Context1 = topublish(Context, [dby_cen(Host, CenId, <<"bus">>)]),
    lists:foldl(wire_cen_to_container(Host, CenId), Context1, ContainerIds).

wire_cen_to_container(Host, CenId) ->
    fun(ContId, Context0) ->
        {Context1, InEndpoint} = next_in_endpoint(Context0, ContId),
        {Context2, OutEndpoint} = next_out_endpoint(Context1, ContId),
        {Context3, Eth} = next_eth(Context2, ContId),
        topublish(Context3,
            [
                dby_cen_to_container(Host, CenId, ContId),
                dby_endpoint(Host, InEndpoint, Eth),
                dby_endpoint_to_container(Host, InEndpoint, ContId),
                dby_endpoint(Host, OutEndpoint, null),
                dby_endpoint_to_cen(Host, OutEndpoint, CenId)
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

% search

-define(MATCH_CONTAINER, ?MDVALUE(<<"type">>, <<"container">>)).
-define(MATCH_CEN, ?MDVALUE(<<"type">>, <<"cen">>)).

% dby:search function to return list of containers linked to an identifier.
linked_containers(Container, ?MATCH_CONTAINER, _, Acc) ->
    {continue, [binary_to_list(Container) | Acc]};
linked_containers(_, _, _, Acc) ->
    {continue, Acc}.

% dby:search function to return list of cens linked to an identifier.
% XXX should peerId be a separate identifier?
linked_cens(_, ?MATCH_CEN, [{_, _, ?MDVALUE(<<"cenID">>, CenId)} | _], Acc) ->
    {continue, [CenId | Acc]};
linked_cens(_, _, _, Acc) ->
    {continue, Acc}.
