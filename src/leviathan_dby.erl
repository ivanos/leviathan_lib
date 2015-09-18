-module(leviathan_dby).

-ifndef(TEST).
-on_load(install_iso8601/0).
-endif.

-export([import_cens/2,
         update_cens/2,
         import_switch/2]).

-export([dby_cen_id/1,
         get_cen/1,
         get_cont/2,
         get_wires/1,
         set_cen_status/2,
         get_next_cin_ip/0]).

-define(PUBLISHER, atom_to_binary(?MODULE, utf8)).
-define(REGISTRY, <<"lev_registry">>).
-define(CIN_COUNT, <<"cin_count">>).

-include("leviathan_logger.hrl").
-include_lib("leviathan_lib/include/leviathan_lib.hrl").

% -----------------------------------------------------------------------------
%
% API
%
% -----------------------------------------------------------------------------

% import CENs

import_cens(Host, LM) ->
    ToPublish = [container_from_lm(Host, LM),
                 cens_from_lm(Host, LM),
                 wires_from_lm(Host, LM)],
    ok = dby:publish(?PUBLISHER, lists:flatten(ToPublish), [persistent]).

% import switch

import_switch(Host, Switch) ->
    ToPublish = [dby_switch(Host, Switch),
                 switch_ports(Host, Switch),
                 %% One flow table should be enough for everyone.
                 switch_tables(Host, Switch, 1)],
    ok = dby:publish(?PUBLISHER, lists:flatten(ToPublish), [persistent]).

% getters

-spec get_bridge(string()) -> #{}.
get_bridge(BridgeId) ->
    dby:search(fun bridge/4,
	       #{bridgeID => null,
		 ip_address => null}, 
	       dby_bridge_id("host1",BridgeId), [{max_depth, 0}]).

-spec get_cen(string()) -> #{}.
get_cen(CenId) ->
    #{ip_address := IPAddress} = get_bridge(CenId),
    dby:search(fun linked_containers/4,
        #{cenID => null,
         wire_type => null,
	 ip_address => IPAddress,
         contIDs => []},
        dby_cen_id(CenId), [{max_depth, 1}]).

-spec get_cont(string(), string()) -> #{}.
get_cont(Host, ContId) ->
    dby:search(fun linked_cens/4,
        #{contID => null,
         cens => []},
        dby_cont_id(Host, ContId), [{max_depth, 1}]).

get_wires(Cen) ->
    #{wires := Wires, ipaddrmap := IpAddrMap} = wire_search(Cen),
    lists:map(
        fun([WireEnd1, WireEnd2]) ->
            % if neeeded, add IP address to wire ends
            [ipaddr_for_wireend(WireEnd1, IpAddrMap),
             ipaddr_for_wireend(WireEnd2, IpAddrMap)]
        end, Wires).

% update

update_cens(Host, Instructions) ->
    ToPublish = lists:map(
        fun(Instruction) -> update_instruction(Host, Instruction) end,
        Instructions),
    ok = dby:publish(?PUBLISHER, lists:flatten(ToPublish), [persistent]).

% status
set_cen_status(CenId, Status) ->
    set_status(dby_cen_id(CenId), Status).

% compute the next cin ip address
% XXX doesn't reuse addresses of delete CINs
get_next_cin_ip() ->
    CinCount = case dby:identifier(?REGISTRY) of
        [] ->
            % no registry
            1;
        #{?CIN_COUNT := #{value := V}} ->
            V
    end,
    % increment count
    ok = dby:publish(?PUBLISHER, {?REGISTRY, [{?CIN_COUNT, CinCount + 1}]}, [persistent]),
    leviathan_cin:cen_ip_address(CinCount).

% formatters

dby_cen_id(CenId) ->
    dby_id([<<"lev_cen">>, CenId]).

% -----------------------------------------------------------------------------
%
% Internal functions
%
% -----------------------------------------------------------------------------
    
install_iso8601() ->
    {module, _} = dby:install(iso8601),
    ok.

% incremental dobby update

update_instruction(Host, {add, cen, Cen}) ->
    pub_cen(Host, Cen);
update_instruction(Host, {add, cont, Cont}) ->
    pub_cont(Host, Cont);
update_instruction(Host, {add, wire, Wire}) ->
    pub_wire(Host, Wire);
update_instruction(Host, {add, cont_in_cen, {ContId, CenId}}) ->
    pub_cont_in_cen(Host, ContId, CenId);
update_instruction(Host, {add, bridge, {Cen, IpAddr}}) ->
    pub_bridge(Host, Cen, IpAddr);
update_instruction(Host, {destroy, cen, Cen}) ->
    pub_destroy_cen(Host, Cen);
update_instruction(Host, {destroy, cont, Cont}) ->
    pub_destroy_cont(Host, Cont);
update_instruction(Host, {destroy, wire, Wire}) ->
    pub_destroy_wire(Host, Wire);
update_instruction(Host, {destroy, cont_in_cen, {ContId, CenId}}) ->
    pub_destroy_cont_in_cen(Host, ContId, CenId);
update_instruction(Host, {destroy, bridge, Cen}) ->
    pub_destroy_bridge(Host, Cen);
update_instruction(_, {set, wire_type, {CenId, WireType}}) ->
    pub_set_wire_type(CenId, WireType).

set_status(DbyId, Status) ->
    dby:publish(?PUBLISHER, {DbyId, [status_md(Status)]}, [persistent]).

% format for dobby

dby_id(List) ->
    dby_id(List, []).

dby_id([E], Acc) ->
    iolist_to_binary([Acc, E]);
dby_id([E | Rest], Acc) ->
    dby_id(Rest, [Acc, E, ">"]).

dby_bridge_id(Host, BridgeId) ->
    dby_id([<<"lev_bridge">>, Host, BridgeId]).

dby_cont_id(Host, ContId) ->
    dby_id([<<"lev_cont">>, Host, ContId]).

dby_endpoint_id(Host, Endpoint) ->
    dby_id([<<"lev_endpoint">>, Host, Endpoint]).

dby_ipaddr_id(IpAddr) ->
    dby_id([<<"lev_ip">>, IpAddr]).

dby_switch_id(Host, ContId) ->
    dby_id([<<"lev_switch">>, Host, ContId]).

dby_of_port_id(SwitchId, N) ->
    <<SwitchId/binary, "/OFP", (integer_to_binary(N))/binary>>.

dby_of_flow_table_id(SwitchId, N) ->
    <<SwitchId/binary, "-table-", (integer_to_binary(N))/binary>>.

dby_cen(CenId, Metadata) when is_binary(CenId) ->
    {dby_cen_id(CenId), [{<<"cenID">>, CenId},
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

dby_ipaddr(IpAddr) when is_binary(IpAddr) ->
    {dby_ipaddr_id(IpAddr), [{<<"type">>, <<"ipaddr">>},
                             {<<"ipaddr">>, IpAddr}]}.

dby_switch(Host, #{<<"contID">> := ContId, <<"datapath_id">> := DatapathId}) ->
    {dby_switch_id(Host, ContId),
     [{<<"contID">>, ContId},
      {<<"type">>, <<"of_switch">>},
      {<<"datapath_id">>, DatapathId}]}.

dby_of_port(SwitchId, N) ->
    {dby_of_port_id(SwitchId, N),
     [{<<"type">>, <<"of_port">>}]}.

dby_of_flow_table(SwitchId, N) ->
    {dby_of_flow_table_id(SwitchId, N),
     [{<<"type">>, <<"of_flow_table">>},
      {<<"table_no">>, N}]}.

dby_endpoint_to_ipaddr(Host, EndpointId, IpAddr) ->
    dby_link(dby_endpoint_id(Host, EndpointId), dby_ipaddr_id(IpAddr),
                                                            <<"bound_to">>).

dby_cen_to_container(Host, CenId, ContId) ->
    dby_link(dby_cen_id(CenId),
             dby_cont_id(Host, ContId), <<"part_of">>).

dby_endpoint_to_container(Host, EndpointId, ContId) ->
    dby_link(dby_endpoint_id(Host, EndpointId),
             dby_cont_id(Host, ContId), <<"bound_to">>).

dby_endpoint_to_bridge(Host, EndpointId, BridgeId) ->
    dby_link(dby_endpoint_id(Host, EndpointId),
                dby_bridge_id(Host, BridgeId), <<"bound_to">>).

dby_bridge_to_cen(Host, BridgeId, CenId) ->
    dby_link(dby_bridge_id(Host, BridgeId), dby_cen_id(CenId),
                                                        <<"policy_engine">>).

dby_endpoint_to_endpoint(Host, EndpointId1, EndpointId2, Type) ->
    dby_link(dby_endpoint_id(Host, EndpointId1),
             dby_endpoint_id(Host, EndpointId2), Type).

% helper
dby_link(E1, E2, Type) ->
    {E1, E2, [{<<"type">>, Type}]}.

% prepare to publish the list of containers
container_from_lm(Host, #{contsmap := #{conts := Conts}}) ->
    lists:map(fun(Cont) -> pub_cont(Host, Cont) end, Conts).

% prepare to publish one container
pub_cont(Host, #{contID := ContId}) ->
    [dby_cont(Host, list_to_binary(ContId), [status_md(pending)])].

% prepare to delete one container
pub_destroy_cont(Host, #{contID := ContId}) ->
    [{dby_cont_id(Host, list_to_binary(ContId)), delete}].

% prepare to publish cens
cens_from_lm(Host, #{censmap := #{cens := Cens}}) ->
    lists:map(fun(Cen) -> pub_cen(Host, Cen) end, Cens).

% prepare to publish one cen
pub_cen(Host, #{cenID := CenId,
          wire_type := bus,
          contIDs := ContIds,
          ip_address := BridgeIpAddr}) ->
    [
        link_cen_to_containers(Host, CenId, ContIds, bus),
        dby_bridge(Host, list_to_binary(CenId),
            [status_md(pending),
             cen_ip_addr_md(list_to_binary(BridgeIpAddr))]),
        dby_bridge_to_cen(Host, list_to_binary(CenId), list_to_binary(CenId))
    ];
pub_cen(Host, #{cenID := CenId,
          wire_type := WireType,
          contIDs := ContIds}) ->
    link_cen_to_containers(Host, CenId, ContIds, WireType).

% prepare to delete one cen
pub_destroy_cen(Host, #{cenID := CenId, wire_type := bus}) ->
    [{dby_cen_id(CenId), delete},
     {dby_bridge_id(Host, CenId), delete}];
pub_destroy_cen(_Host, #{cenID := CenId}) ->
    [{dby_cen_id(CenId), delete}].

% link CEN to containers
link_cen_to_containers(Host, CenId, ContIds, WireType) ->
    [
        dby_cen(list_to_binary(CenId), [wire_type_md(WireType), status_md(pending)]),
        lists:map(
            fun(ContId) ->
                dby_cen_to_container(Host, list_to_binary(CenId), list_to_binary(ContId))
            end, ContIds)
    ].

% prepare to publish wires
wires_from_lm(Host, #{wiremap := #{wires := Wires}}) ->
    lists:foldl(
        fun(Wire, Acc) ->
            [pub_wire(Host, Wire) | Acc]
        end, [], Wires).

switch_ports(Host, #{<<"contID">> := ContID, <<"interfaces">> := InterfacesBin}) ->
    SwitchId = dby_switch_id(Host, ContID),
    NumberedInterfaces = lists:zip(lists:seq(1, length(InterfacesBin)), InterfacesBin),
    [
     [dby_of_port(SwitchId, N),
      dby_link(dby_of_port_id(SwitchId, N), SwitchId, <<"part_of">>),
      dby_link(dby_of_port_id(SwitchId, N), dby_bridge_id(Host, Interface), <<"bound_to">>)]
     || {N, Interface} <- NumberedInterfaces].

switch_tables(Host, #{<<"contID">> := ContID}, HowMany) ->
    SwitchId = dby_switch_id(Host, ContID),
    [
     [dby_of_flow_table(SwitchId, N),
      dby_link(dby_of_flow_table_id(SwitchId, N), SwitchId, <<"of_resource">>)]
     || N <- lists:seq(0, HowMany - 1)].

% prepare to publish one wire
pub_wire(Host, [Endpoint1 = #{endID := EndId1},
                Endpoint2 = #{endID := EndId2}]) ->
    [
        endpoint(Host, Endpoint1),
        endpoint(Host, Endpoint2),
        dby_endpoint_to_endpoint(Host,
            list_to_binary(EndId1),
            list_to_binary(EndId2),
            endpoint_to_endpoint_type(Endpoint1, Endpoint2))
    ].

% prepare to delete one wire
pub_destroy_wire(Host, [#{endID := EndId1}, #{endID := EndId2}]) ->
    [
        {dby_endpoint_id(Host, list_to_binary(EndId1)), delete},
        {dby_endpoint_id(Host, list_to_binary(EndId2)), delete}
        % XXX delete IP addresses?
    ].

endpoint(Host, #{endID := EndId,
                 side := Side,
                 dest := #{type := cont,
                           id := ContId,
                           alias := Eth,
                           ip_address := IpAddr}}) ->
    [
        dby_endpoint(Host, list_to_binary(EndId), Side, [alias_md(list_to_binary(Eth)), status_md(pending)]),
        dby_ipaddr(list_to_binary(IpAddr)),
        dby_endpoint_to_ipaddr(Host, list_to_binary(EndId), list_to_binary(IpAddr)),
        dby_endpoint_to_container(Host, list_to_binary(EndId), list_to_binary(ContId))
    ];
endpoint(Host, #{endID := EndId,
                 side := Side,
                 dest := #{type := cen,
                           id := CenId}}) ->
    [
        dby_endpoint(Host, list_to_binary(EndId), Side, [status_md(pending)]),
        dby_endpoint_to_bridge(Host, list_to_binary(EndId), list_to_binary(CenId))
    ].

endpoint_to_endpoint_type(#{dest := #{type := cont, id := ContId1}},
                          #{dest := #{type := cont, id := ContId2}})
                                                when ContId1 /= ContId2 ->
    <<"connected_to">>;
endpoint_to_endpoint_type(_,_) ->
    <<"veth_peer">>.

% prepare to add container to cen
pub_cont_in_cen(Host, ContId, CenId) ->
    [dby_cen_to_container(Host, list_to_binary(CenId),
                                list_to_binary(ContId))].

% prepare to destroy container to cen
pub_destroy_cont_in_cen(Host, ContId, CenId) ->
    [{dby_cen_id(list_to_binary(CenId)),
      dby_cont_id(Host, list_to_binary(ContId)),
      delete}].

% set wiretype in cen
pub_set_wire_type(CenId, WireType) ->
    [{dby_cen_id(list_to_binary(CenId)), [wire_type_md(WireType)]}].

% publish bridge
pub_bridge(Host, CenId, BridgeIpAddr) ->
    [
        dby_bridge(Host, list_to_binary(CenId),
            [status_md(pending),
             cen_ip_addr_md(list_to_binary(BridgeIpAddr))]),
        dby_bridge_to_cen(Host, list_to_binary(CenId), list_to_binary(CenId))
    ].

% publish destroy bridge
pub_destroy_bridge(Host, BridgeId) ->
    [
        {dby_bridge_id(Host, BridgeId), delete}
    ].


status_md(pending) ->
    {<<"status">>, <<"pending">>};
status_md(preparing) ->
    {<<"status">>, <<"preparing">>};
status_md(ready) ->
    {<<"status">>, <<"ready">>};
status_md(destroy) ->
    {<<"status">>, <<"destroy">>}.

cen_ip_addr_md(IPAddress) ->
    {<<"ipaddr">>, IPAddress}.

wire_type_md(null) ->
    {<<"wire_type">>, null};
wire_type_md(wire) ->
    {<<"wire_type">>, <<"wire">>};
wire_type_md(bus) ->
    {<<"wire_type">>, <<"bus">>}.

alias_md(Alias) ->
    {<<"alias">>, Alias}.

endpoint_side_md(in) ->
    {<<"side">>, <<"in">>};
endpoint_side_md(out) ->
    {<<"side">>, <<"out">>}.

md_wire_type(null) ->
    null;
md_wire_type(<<"wire">>) ->
    wire;
md_wire_type(<<"bus">>) ->
    bus.

% search

bridge(_,?MATCH_BRIDGE(BridgeId, IPAddress),[], Acc)-> 
    {continue, Acc#{bridgeID := binary_to_list(BridgeId),
                    ip_address := binary_to_list(IPAddress)}};
bridge(_, _, _, Acc) ->
    {continue, Acc}.
    

% dby:search function to return list of containers linked to an identifier.
linked_containers(_, ?MATCH_CEN(CenId, WireType), [], Acc) ->
    {continue, Acc#{cenID := binary_to_list(CenId),
                    wire_type := md_wire_type(WireType)}};
linked_containers(_, ?MATCH_CONTAINER(ContId), _, Acc) ->
    {continue, map_prepend(Acc, contIDs, binary_to_list(ContId))};
linked_containers(_, _, _, Acc) ->
    {continue, Acc}.

% dby:search function to return list of cens linked to an identifier.
linked_cens(_, ?MATCH_CONTAINER(ContId), [], Acc) ->
    {continue, Acc#{contID := binary_to_list(ContId)}};
linked_cens(_, ?MATCH_CEN(CenId, _), _, Acc) ->
    {continue, map_prepend(Acc, cens, binary_to_list(CenId))};
linked_cens(_, _, _, Acc) ->
    {continue, Acc}.

wire_search(#{wire_type := null}) ->
    #{wires => [], ipaddrmap => #{}};
wire_search(#{cenID := CenId, wire_type := bus}) ->
    % bus data model in dobby benefits from searching breadth first.
    % This makes it easy to find the paths that form the wires.
    dby:search(fun wires/4, #{wires => [], ipaddrmap => #{}},
            dby_cen_id(CenId), [breadth, {max_depth, 5}, {loop, link}]);
wire_search(#{cenID := CenId, wire_type := wire}) ->
    % wire data model in dobby beneifts from searching depth first because
    % the containers are both linked to the starting point. A breadth
    % first search never finds the complete path for the wire because
    % it partially traverses the wire from both directions.
    dby:search(fun wires/4, #{wires => [], ipaddrmap => #{}},
            dby_cen_id(CenId), [depth, {max_depth, 4}, {loop, link}]).

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
wires_bus(_, ?MATCH_BRIDGE(_, _), [_], Acc) ->
    % don't follow links from Bridge
    % we want to approach the bridge from the other direction of the loop
    {skip, Acc};
wires_bus(_, ?MATCH_BRIDGE(BridgeId, _),
                [{_, ?MATCH_OUT_ENDPOINT(OutEndId), _},
                 {_, ?MATCH_IN_ENDPOINT(InEndId, Alias), _},
                 {_, ?MATCH_CONTAINER(ContId), _} | _],
                                                 Acc = #{wires := Wires}) ->
    Wire = [
        #{endID => binary_to_list(InEndId),
          dest => #{type => cont,
                    id => binary_to_list(ContId),
                    alias => binary_to_list(Alias)}},
        #{endID => binary_to_list(OutEndId),
          dest => #{type => cen,
                    id => binary_to_list(BridgeId)}}],
    {continue, Acc#{wires := [Wire | Wires]}};
wires_bus(_, ?MATCH_IPADDR(IpAddr),
                [{_, ?MATCH_ENDPOINT(EndId), _} | _],
                                        Acc = #{ipaddrmap := IpAddrMap}) ->
    {continue, Acc#{ipaddrmap := put_ipaddr(EndId, IpAddr, IpAddrMap)}};
wires_bus(_, _, _, Acc) ->
    {continue, Acc}.

%   cont <-> endpoint (inside) <-> endpoint (inside) <-> cont
wires_wire(_, ?MATCH_CONTAINER(ContId1),
                [{_, ?MATCH_IN_ENDPOINT(EndId1, Alias1), _},
                 {_, ?MATCH_IN_ENDPOINT(EndId2, Alias2), _},
                 {_, ?MATCH_CONTAINER(ContId2), _} | _],
                                                 Acc = #{wires := Wires}) ->
    Wire = [
        #{endID => binary_to_list(EndId1),
          dest => #{type => cont,
                    id => binary_to_list(ContId1),
                    alias => binary_to_list(Alias1)}},
        #{endID => binary_to_list(EndId2),
          dest => #{type => cont,
                    id => binary_to_list(ContId2),
                    alias => binary_to_list(Alias2)}}],
    {continue, Acc#{wires := [Wire | Wires]}};
wires_wire(_, ?MATCH_IPADDR(IpAddr),
                [{_, ?MATCH_ENDPOINT(EndId), _} | _],
                                        Acc = #{ipaddrmap := IpAddrMap}) ->
    {continue, Acc#{ipaddrmap := put_ipaddr(EndId, IpAddr, IpAddrMap)}};
wires_wire(_, _, _, Acc) ->
    {continue, Acc}.

ipaddr_for_wireend(WireEnd = #{endID := EndId, dest := Dest}, IpAddrMap) ->
    case maps:get(EndId, IpAddrMap, not_found) of
        not_found ->
            WireEnd;
        IpAddr ->
            WireEnd#{dest := Dest#{ip_address => IpAddr}}
    end.

put_ipaddr(EndId, IpAddr, IpAddrMap) ->
    maps:put(binary_to_list(EndId), binary_to_list(IpAddr), IpAddrMap).

% map helpers

map_prepend(Map, Key, Value) ->
    {ok, OldList} = maps:find(Key, Map),
    maps:update(Key, [Value | OldList], Map).
