-module(leviathan_cen_test).

-include_lib("eunit/include/eunit.hrl").

-define(assertEqualLists(A,B), ?assertEqual(lists:sort(A), lists:sort(B))).

leviathan_cen_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     {foreach,
       fun each_setup/0,
       [
% tests that use wire/null cen wire type. wire not used for now.
%       {"decode_jiffy 0 containers", fun decode_jiffy1/0}
%      ,{"decode_jiffy 1 container", fun decode_jiffy2/0}
%      ,{"decode_jiffy 2 containers", fun decode_jiffy3/0}
        {"decode_jiffy 3 containers", fun decode_jiffy4/0}
%      ,{"lm_add_container test 0", fun lm_add_container0/0}
%      ,{"lm_add_container test 1", fun lm_add_container1/0}
       ,{"lm_add_container test 2", fun lm_add_container2/0}
       ,{"lm_add_container test 3", fun lm_add_container3/0}
%      ,{"lm_remove_container test 0", fun lm_remove_container0/0}
%      ,{"lm_remove_container test 1", fun lm_remove_container1/0}
%      ,{"lm_remove_container test 2", fun lm_remove_container2/0}
%      ,{"lm_compare 0", fun lm_compare0/0}
%      ,{"lm_compare 1", fun lm_compare1/0}
%      ,{"lm_compare 2", fun lm_compare2/0}
%      ,{"lm_compare 3", fun lm_compare3/0}
%      ,{"lm_compare 4", fun lm_compare4/0}
       ]}}.

setup() ->
    ok = meck:new(leviathan_dby).

cleanup(ok) ->
    ok = meck:unload(leviathan_dby).

each_setup() ->
    ok = meck:expect(leviathan_dby, get_next_cin_ip, 0,
                                        meck:seq(["10.7.0.1","10.8.0.1"])),
    ok = meck:reset(leviathan_dby).

decode_jiffy1() ->
    Json = [json_cen(<<"cen1">>, [])],
    Cen1 = cen_map("cen1", [], null, "10.7.0.1"),
    {Cens, Conts, Wires} = decompose_lm(leviathan_cen:decode_jiffy(Json)),
    ?assertEqualLists([Cen1], Cens),
    ?assertEqualLists([], Conts),
    ?assertEqualLists([], Wires).

decode_jiffy2() ->
    Json = [json_cen(<<"cen1">>, [<<"c1">>])],
    Cen1 = cen_map("cen1", ["c1"], null, "10.7.0.1"),
    Cont1 = cont_map("c1", ["cen1"]),
    {Cens, Conts, Wires} = decompose_lm(leviathan_cen:decode_jiffy(Json)),
    ?assertEqualLists([Cen1], Cens),
    ?assertEqualLists([Cont1], Conts),
    ?assertEqualLists([], Wires).

decode_jiffy3() ->
    Json = [json_cen(<<"cen1">>, [<<"c1">>, <<"c2">>])],
    Cen1 = cen_map("cen1", ["c1", "c2"], wire, "10.7.0.1"),
    Cont1 = cont_map("c1", ["cen1"]),
    Cont2 = cont_map("c2", ["cen1"]),
    Wire = [
        endpoint("c1", "c1.0i", in, "cen1", "10.7.0.10"),
        endpoint("c2", "c2.0i", in, "cen1", "10.7.0.11")
    ],
    {Cens, Conts, Wires} = decompose_lm(leviathan_cen:decode_jiffy(Json)),
    ?assertEqualLists([Cen1], Cens),
    ?assertEqualLists([Cont1, Cont2], Conts),
    ?assertEqualLists([Wire], Wires).

decode_jiffy4() ->
    Json = [json_cen(<<"cen1">>, [<<"c1">>, <<"c2">>, <<"c3">>])],
    Cen1 = cen_map("cen1", ["c1", "c2", "c3"], bus, "10.7.0.1"),
    Cont1 = cont_map("c1", ["cen1"]),
    Cont2 = cont_map("c2", ["cen1"]),
    Cont3 = cont_map("c3", ["cen1"]),
    Wire1 = [
        endpoint("c1", "c1.0i", in, "cen1", "10.7.0.10"),
        endpoint("cen1", "c1.0o", out)
    ],
    Wire2 = [
        endpoint("c2", "c2.0i", in, "cen1", "10.7.0.11"),
        endpoint("cen1", "c2.0o", out)
    ],
    Wire3 = [
        endpoint("c3", "c3.0i", in, "cen1", "10.7.0.12"),
        endpoint("cen1", "c3.0o", out)
    ],
    {Cens, Conts, Wires} = decompose_lm(leviathan_cen:decode_jiffy(Json)),
    ?assertEqualLists([Cen1], Cens),
    ?assertEqualLists([Cont1, Cont2, Cont3], Conts),
    ?assertEqualLists([Wire1, Wire2, Wire3], Wires).

lm_add_container0() ->
    LM = leviathan_cen:lm_add_container("cen1", "c1", new_lm()),
    {Cens, Conts, Wires} = decompose_lm(LM),
    ?assertEqual([cen_map("cen1", ["c1"], null, "10.7.0.1")], Cens),
    ?assertEqual([cont_map("c1", ["cen1"])], Conts),
    ?assertEqual([], Wires).

lm_add_container1() ->
    LM0 = leviathan_cen:lm_add_container("cen1", "c1", new_lm()),
    LM1 = leviathan_cen:lm_add_container("cen1", "c2", LM0),
    {Cens, Conts, Wires} = decompose_lm(LM1),
    ?assertEqual([cen_map("cen1", ["c2", "c1"], wire, "10.7.0.1")], Cens),
    ?assertEqualLists(Conts, [cont_map("c1", ["cen1"]),
                              cont_map("c2", ["cen1"])]),
    ?assertEqual([
        [
            endpoint("c2", "c2.0i", in, "cen1", "10.7.0.10"),
            endpoint("c1", "c1.0i", in, "cen1", "10.7.0.11")
        ]
    ], Wires).

lm_add_container2() ->
    LM0 = leviathan_cen:lm_add_container("cen1", "c1", new_lm()),
    LM1 = leviathan_cen:lm_add_container("cen1", "c2", LM0),
    LM2 = leviathan_cen:lm_add_container("cen1", "c3", LM1),
    {Cens, Conts, Wires} = decompose_lm(LM2),
    ?assertEqual([cen_map("cen1", ["c1", "c2", "c3"], bus, "10.7.0.1")], Cens),
    ?assertEqualLists([cont_map("c1", ["cen1"]),
                       cont_map("c2", ["cen1"]),
                       cont_map("c3", ["cen1"])],
                      Conts),
    ?assertEqual(3, length(Wires)),
    assert_bus_wires(Wires).

lm_add_container3() ->
    LM0 = leviathan_cen:lm_add_container("cen1", "c1", new_lm()),
    LM1 = leviathan_cen:lm_add_container("cen1", "c2", LM0),
    LM2 = leviathan_cen:lm_add_container("cen1", "c3", LM1),
    LM3 = leviathan_cen:lm_add_container("cen2", "c1", LM2),
    LM4 = leviathan_cen:lm_add_container("cen2", "c2", LM3),
    LM5 = leviathan_cen:lm_add_container("cen2", "c3", LM4),
    LM6 = leviathan_cen:lm_add_container("cen2", "c4", LM5),
    {Cens, Conts, Wires} = decompose_lm(LM6),
    ?assertEqualLists([cen_map("cen1", ["c1", "c2", "c3"], bus, "10.7.0.1"),
                       cen_map("cen2", ["c1", "c2", "c3", "c4"], bus, "10.8.0.1")],
                      Cens),
    ?assertEqualLists([cont_map("c1", ["cen1", "cen2"]),
                       cont_map("c2", ["cen1", "cen2"]),
                       cont_map("c3", ["cen1", "cen2"]),
                       cont_map("c4", ["cen2"])],
                      Conts),
    ?assertEqual(7, length(Wires)),
    leviathan_test_utils:check_wires(Cens, Wires),
    assert_bus_wires(Wires).

lm_remove_container0() ->
    LM = leviathan_cen:lm_remove_container("cen1", "c1", new_lm()),
    ?assertEqual(new_lm(), LM).

lm_remove_container1() ->
    Json = [json_cen(<<"cen1">>, [<<"c1">>])],
    LM0 = leviathan_cen:decode_jiffy(Json),
    LM1 = leviathan_cen:lm_remove_container("cen1", "c1", LM0),
    {Cens, Conts, Wires} = decompose_lm(LM1),
    ?assertEqual([cen_map("cen1", [], null, "10.7.0.1")], Cens),
    ?assertEqual([], Conts),
    ?assertEqual([], Wires).

lm_remove_container2() ->
    Json = [json_cen(<<"cen1">>, [<<"c1">>, <<"c2">>])],
    LM0 = leviathan_cen:decode_jiffy(Json),
    LM1 = leviathan_cen:lm_remove_container("cen1", "c1", LM0),
    {Cens, Conts, Wires} = decompose_lm(LM1),
    ?assertEqual([cen_map("cen1", ["c2"], null, "10.7.0.1")], Cens),
    ?assertEqual([cont_map("c2", ["cen1"])], Conts),
    ?assertEqual([], Wires).

lm_compare0() ->
    ?assertEqual([], leviathan_cen:lm_compare(new_lm(), new_lm())).

lm_compare1() ->
    Json = [json_cen(<<"cen1">>, [<<"c1">>])],
    LM0 = leviathan_cen:decode_jiffy(Json),
    Instructions = [
        {add, cen, cen_map("cen1", ["c1"], null, "10.7.0.1")},
        {add, cont, cont_map("c1", ["cen1"])}
    ],
    ?assertEqual(Instructions, leviathan_cen:lm_compare(new_lm(), LM0)).

lm_compare2() ->
    Json = [json_cen(<<"cen1">>, [<<"c1">>])],
    LM0 = leviathan_cen:decode_jiffy(Json),
    Instructions = [
        {destroy, cen, cen_map("cen1", ["c1"], null, "10.7.0.1")},
        {destroy, cont, cont_map("c1", ["cen1"])}
    ],
    ?assertEqual(Instructions, leviathan_cen:lm_compare(LM0, new_lm())).

lm_compare3() ->
    OldLM = leviathan_cen:decode_jiffy([json_cen(<<"cen1">>, [<<"c1">>])]),
    NewLM = leviathan_cen:decode_jiffy([json_cen(<<"cen1">>,
                                                    [<<"c2">>, <<"c1">>])]),
    Instructions = [
        {add, cont_in_cen, {"c2", "cen1"}},
        {set, wire_type, {"cen1", wire}},
        {add, cont, cont_map("c2", ["cen1"])},
        {add, wire, [
            endpoint("c2", "c2.0i", in, "cen1", "10.7.0.10"),
            endpoint("c1", "c1.0i", in, "cen1", "10.7.0.11")
        ]}
    ],
    ?assertEqual(Instructions, leviathan_cen:lm_compare(OldLM, NewLM)).

lm_compare4() ->
    OldLM = leviathan_cen:decode_jiffy([json_cen(<<"cen1">>,
                                                    [<<"c2">>, <<"c1">>])]),
    NewLM = leviathan_cen:decode_jiffy([json_cen(<<"cen1">>, [<<"c1">>])]),
    Instructions = [
        {destroy, cont_in_cen, {"c2", "cen1"}},
        {set, wire_type, {"cen1", null}},
        {destroy, cont, cont_map("c2", ["cen1"])},
        {destroy, wire, [
            endpoint("c2", "c2.0i", in, "cen1", "10.7.0.10"),
            endpoint("c1", "c1.0i", in, "cen1", "10.7.0.11")
        ]}
    ],
    ?assertEqual(Instructions, leviathan_cen:lm_compare(OldLM, NewLM)).

%-------------------------------------------------------------------------------
% helpers
%-------------------------------------------------------------------------------

new_lm() ->
    #{
        censmap => #{cens => []},
        contsmap => #{conts => []},
        wiremap => #{wires => []}
    }.

decompose_lm(#{censmap := #{cens := Cens},
               contsmap := #{conts := Conts},
               wiremap := #{wires := Wires}}) ->
    {Cens, Conts, Wires}.

json_cen(CenId, ContIds) ->
    #{<<"cenID">> => CenId, <<"containerIDs">> => ContIds}.

cen_map(CenId, ContIds, WireType, IpAddr) ->
    #{cenID => CenId,
      wire_type => WireType,
      contIDs => ContIds,
      ip_address => IpAddr}.

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

assert_bus_wires(Wires) ->
    ?assert(length(Wires) >= 3),
    [assert_one_end_is_cen(Wire) || Wire <- Wires].

assert_one_end_is_cen([Endpoint1, Endpoint2]) ->
    ?assert(is_cen_endpoint(Endpoint1) orelse is_cen_endpoint(Endpoint2)).

is_cen_endpoint(#{dest := #{type := cen}}) ->
    true;
is_cen_endpoint(_) ->
    false.

tr() ->
    dbg:start(),
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tpl(leviathan_cen, []).
