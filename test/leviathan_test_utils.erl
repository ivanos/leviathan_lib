-module(leviathan_test_utils).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

check_wires(Wires) ->
    same_cen_endpoints(Wires).

same_cen_endpoints([]) ->
    ok;
same_cen_endpoints([Wire | Wires]) ->
    same_cen_endpoints_wire(Wire),
    same_cen_endpoints(Wires).

same_cen_endpoints_wire([CenEnd  = #{dest := #{type := cen}},
                         ContEnd = #{dest := #{type := cont}}]) ->
    same_cen_endpoints_wire(CenEnd, ContEnd);
same_cen_endpoints_wire([ContEnd = #{dest := #{type := cont}},
                         CenEnd  = #{dest := #{type := cen}}]) ->
    same_cen_endpoints_wire(CenEnd, ContEnd).

same_cen_endpoints_wire(#{dest := #{id := CenId}},
                        #{dest := #{alias := Alias}}) ->
    ?assertEqual(CenId, Alias).
