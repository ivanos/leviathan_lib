-module(leviathan_test_utils).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

check_wires(Cens, Wires) ->
    % map CEN to ipaddr
    IpAddrByCen = map_cen_to_ipaddr(Cens),
    
    % list of CEN endpionts
    CenEndpointIpAddrs = cen_endpoints_from_wires(Wires),

    % validate
    cen_ipaddrs_in_cen(CenEndpointIpAddrs, IpAddrByCen).

map_cen_to_ipaddr(Cens) ->
    lists:foldl(
        fun(#{cenID := CenId, ip_address := IpAddr}, Acc) ->
            maps:put(CenId, IpAddr, Acc)
        end, #{}, Cens).

cen_endpoints_from_wires(Wires) ->
    lists:flatten(lists:foldl(
        fun(Wire, Acc) ->
            [container_endpoints(Wire), Acc]
        end, [], Wires)).

container_endpoints([Endpoint1, Endpoint2]) ->
    [cen_ipaddr(Endpoint1), cen_ipaddr(Endpoint2)].

cen_ipaddr(#{dest := #{type := cont, alias := CenId, ip_address := IpAddr}}) ->
    [{CenId, IpAddr}];
cen_ipaddr(_) ->
    [].

cen_ipaddrs_in_cen([], _) ->
    true;
cen_ipaddrs_in_cen([{CenId, IpAddr} | Rest], IpAddrByCen) ->
    {ok, {CenA, CenB, _, _}} =
                inet:parse_address(CenIpaddr = maps:get(CenId, IpAddrByCen)),
    {ok, {ContA, ContB, _, _}} = inet:parse_address(IpAddr),
    ?assertEqual({CenA, CenB}, {ContA, ContB}),
    cen_ipaddrs_in_cen(Rest, IpAddrByCen).
