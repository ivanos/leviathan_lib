-module(leviathan_cen_test).

-include_lib("eunit/include/eunit.hrl").

-define(assertEqualLists(A,B), ?assertEqual(lists:sort(A), lists:sort(B))).

leviathan_cen_test_() ->
    [
        {"decode_jiffy 0 containers", fun decode_jiffy1/0}
       ,{"decode_jiffy 1 container", fun decode_jiffy2/0}
       ,{"decode_jiffy 2 containers", fun decode_jiffy3/0}
       ,{"decode_jiffy 3 containers", fun decode_jiffy4/0}
    ].

decode_jiffy1() ->
    Json = [json_cen(<<"cen1">>, [])],
    Cen1 = cen_map(<<"cen1">>, [], null),
    #{censmap := #{cens := Cens},
      contsmap := #{conts := Conts},
      wiremap := #{wires := Wires}} = leviathan_cen:decode_jiffy(Json),
    ?assertEqualLists([Cen1], Cens),
    ?assertEqualLists([], Conts),
    ?assertEqualLists([], Wires).

decode_jiffy2() ->
    Json = [json_cen(<<"cen1">>, [<<"c1">>])],
    Cen1 = cen_map(<<"cen1">>, [<<"c1">>], null),
    Cont1 = cont_map(<<"c1">>, [<<"cen1">>]),
    #{censmap := #{cens := Cens},
      contsmap := #{conts := Conts},
      wiremap := #{wires := Wires}} = leviathan_cen:decode_jiffy(Json),
    ?assertEqualLists([Cen1], Cens),
    ?assertEqualLists([Cont1], Conts),
    ?assertEqualLists([], Wires).

decode_jiffy3() ->
    Json = [json_cen(<<"cen1">>, [<<"c1">>, <<"c2">>])],
    Cen1 = cen_map(<<"cen1">>, [<<"c1">>, <<"c2">>], wire),
    Cont1 = cont_map(<<"c1">>, [<<"cen1">>]),
    Cont2 = cont_map(<<"c2">>, [<<"cen1">>]),
    Wire = [
        endpoint(<<"c1">>, <<"c1.0i">>, in, <<"cen1">>, <<"10.7.0.10">>),
        endpoint(<<"c2">>, <<"c2.0i">>, in, <<"cen1">>, <<"10.7.0.11">>)
    ],
    #{censmap := #{cens := Cens},
      contsmap := #{conts := Conts},
      wiremap := #{wires := Wires}} = leviathan_cen:decode_jiffy(Json),
    ?assertEqualLists([Cen1], Cens),
    ?assertEqualLists([Cont1, Cont2], Conts),
    ?assertEqualLists([Wire], Wires).

decode_jiffy4() ->
    Json = [json_cen(<<"cen1">>, [<<"c1">>, <<"c2">>, <<"c3">>])],
    Cen1 = cen_map(<<"cen1">>, [<<"c1">>, <<"c2">>, <<"c3">>], bus, <<"10.7.0.1">>),
    Cont1 = cont_map(<<"c1">>, [<<"cen1">>]),
    Cont2 = cont_map(<<"c2">>, [<<"cen1">>]),
    Cont3 = cont_map(<<"c3">>, [<<"cen1">>]),
    Wire1 = [
        endpoint(<<"c1">>, <<"c1.0i">>, in, <<"cen1">>, <<"10.7.0.10">>),
        endpoint(<<"cen1">>, <<"c1.0o">>, out)
    ],
    Wire2 = [
        endpoint(<<"c2">>, <<"c2.0i">>, in, <<"cen1">>, <<"10.7.0.11">>),
        endpoint(<<"cen1">>, <<"c2.0o">>, out)
    ],
    Wire3 = [
        endpoint(<<"c3">>, <<"c3.0i">>, in, <<"cen1">>, <<"10.7.0.12">>),
        endpoint(<<"cen1">>, <<"c3.0o">>, out)
    ],
    #{censmap := #{cens := Cens},
      contsmap := #{conts := Conts},
      wiremap := #{wires := Wires}} = leviathan_cen:decode_jiffy(Json),
    ?assertEqualLists([Cen1], Cens),
    ?assertEqualLists([Cont1, Cont2, Cont3], Conts),
    ?assertEqualLists([Wire1, Wire2, Wire3], Wires).

%-------------------------------------------------------------------------------
% helpers
%-------------------------------------------------------------------------------

json_cen(CenId, ContIds) ->
    #{<<"cenID">> => CenId, <<"containerIDs">> => ContIds}.

cen_map(CenId, ContIds, WireType) ->
    #{cenID => CenId,
      wire_type => WireType,
      contIDs => ContIds}.

cen_map(CenId, ContIds, WireType, IpAddr) ->
    (cen_map(CenId, ContIds, WireType))#{ipaddr => IpAddr}.

cont_map(ContId, CenIds) ->
    #{contID => ContId,
      cens => CenIds}.

endpoint(ContId, EndId, Side, Alias, IpAddr) ->
    #{endID => EndId,
      side => Side,
      dest => #{alias => Alias,
                id => ContId,
                ip_address => IpAddr,
                type => cont}}.

endpoint(CenId, EndId, Side) ->
    #{endID => EndId,
      side => Side,
      dest => #{id => CenId,
                type => cen}}.

tr() ->
    dbg:start(),
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tpl(leviathan_cen, []).
