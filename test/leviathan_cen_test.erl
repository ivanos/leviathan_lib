-module(leviathan_cen_test).

-include_lib("eunit/include/eunit.hrl").

-define(assertEqualLists(A,B), ?assertEqual(lists:sort(A), lists:sort(B))).

leviathan_cen_import_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     {foreach,
       fun each_setup/0,
       [
        {"decode_jiffy 0 containers", fun decode_jiffy1/0}
       ,{"decode_jiffy 1 container", fun decode_jiffy2/0}
       ,{"decode_jiffy 2 containers", fun decode_jiffy3/0}
       ,{"decode_jiffy 3 containers", fun decode_jiffy4/0}
       ,{"lm_add_container test 0", fun lm_add_container0/0}
       ,{"lm_add_container test 1", fun lm_add_container1/0}
       ,{"lm_add_container test 2", fun lm_add_container2/0}
       ,{"lm_add_container test 3", fun lm_add_container3/0}
       ,{"lm_add_container test 4", fun lm_add_container4/0}
       ,{"lm_remove_container test 0", fun lm_remove_container0/0}
       ,{"lm_remove_container test 1", fun lm_remove_container1/0}
       ,{"lm_remove_container test 2", fun lm_remove_container2/0}
       ,{"lm_remove_container test 3", fun lm_remove_container3/0}
       ,{"lm_compare 0", fun lm_compare0/0}
       ,{"lm_compare 1", fun lm_compare1/0}
       ,{"lm_compare 2", fun lm_compare2/0}
       ,{"lm_compare 3", fun lm_compare3/0}
       ,{"lm_compare 4", fun lm_compare4/0}
       ,{"lm_compare 5", fun lm_compare5/0}
       ,{"lm_compare 6", fun lm_compare6/0}
       ,{"lm_add_cen 0", fun lm_add_cen0/0}
       ,{"lm_add_container_to_new_cen", fun lm_add_container_to_new_cen/0}
       ,{"get levmap from store 0", fun get_levmap0/0}
       ,{"get levmap from store 1", fun get_levmap1/0}
       ,{"get levmap from store 2", fun get_levmap2/0}
       ,{"get levmap from store 3", fun get_levmap3/0}
       ,{"get levmap from store 4", fun get_levmap4/0}
       ]}}.

setup() ->
    ok = application:load(erl_mnesia),
    ok = application:set_env(erl_mnesia, options, [persistent]),
    Apps = application:ensure_all_started(erl_mnesia),
    ok = mnesia:start(),
    ok = leviathan_mnesia:start(),
    Apps.

cleanup({ok, Apps}) ->
    lists:foreach(fun application:stop/1, Apps),
    ok = application:unload(erl_mnesia).

each_setup() ->
    ok = leviathan_mnesia:clear().

decode_jiffy1() ->
    Json = [json_cen(<<"cen1">>, [])],
    Cen1 = cen_map("cen1", [], bus, 10, "10.10.0.1"),
    {Cens, Conts, Wires} = decompose_lm(leviathan_cen:decode_jiffy(Json)),
    ?assertEqualLists([Cen1], Cens),
    ?assertEqualLists([], Conts),
    ?assertEqualLists([], Wires).

decode_jiffy2() ->
    Json = [json_cen(<<"cen1">>, [<<"c1">>])],
    Cen1 = cen_map("cen1", ["c1"], bus, 10, "10.10.0.1", ["10.10.0.10"]),
    Cont1 = cont_map("c1", ["cen1"], [0]),
    {Cens, Conts, Wires} = decompose_lm(leviathan_cen:decode_jiffy(Json)),
    ?assertEqualLists([Cen1], Cens),
    ?assertEqualLists([Cont1], Conts),
    ?assertEqualLists([], Wires).

decode_jiffy3() ->
    Json = [json_cen(<<"cen1">>, [<<"c1">>, <<"c2">>])],
    Cen1 = cen_map("cen1", ["c1", "c2"], bus, 10, "10.10.0.1", ["10.10.0.10",
                                                                "10.10.0.11"]),
    Cont1 = cont_map("c1", ["cen1"], [0]),
    Cont2 = cont_map("c2", ["cen1"], [0]),
    Wire1 = [
        endpoint("c1", "c1.0i", in, "cen1", "10.10.0.10"),
        endpoint("cen1", "c1.0o", out)
    ],
    Wire2 = [
        endpoint("c2", "c2.0i", in, "cen1", "10.10.0.11"),
        endpoint("cen1", "c2.0o", out)
    ],
    {Cens, Conts, Wires} = decompose_lm(leviathan_cen:decode_jiffy(Json)),
    ?assertEqualLists([Cen1], Cens),
    ?assertEqualLists([Cont1, Cont2], Conts),
    ?assertEqualLists([Wire1, Wire2], Wires).

decode_jiffy4() ->
    Json = [json_cen(<<"cen1">>, [<<"c1">>, <<"c2">>, <<"c3">>])],
    Cen1 = cen_map("cen1", ["c1", "c2", "c3"], bus, 10, "10.10.0.1",
                                                            ["10.10.0.10",
                                                             "10.10.0.11",
                                                             "10.10.0.12"]),
    Cont1 = cont_map("c1", ["cen1"], [0]),
    Cont2 = cont_map("c2", ["cen1"], [0]),
    Cont3 = cont_map("c3", ["cen1"], [0]),
    Wire1 = [
        endpoint("c1", "c1.0i", in, "cen1", "10.10.0.10"),
        endpoint("cen1", "c1.0o", out)
    ],
    Wire2 = [
        endpoint("c2", "c2.0i", in, "cen1", "10.10.0.11"),
        endpoint("cen1", "c2.0o", out)
    ],
    Wire3 = [
        endpoint("c3", "c3.0i", in, "cen1", "10.10.0.12"),
        endpoint("cen1", "c3.0o", out)
    ],
    {Cens, Conts, Wires} = decompose_lm(leviathan_cen:decode_jiffy(Json)),
    ?assertEqualLists([Cen1], Cens),
    ?assertEqualLists([Cont1, Cont2, Cont3], Conts),
    ?assertEqualLists([Wire1, Wire2, Wire3], Wires).

lm_add_container0() ->
    LM = leviathan_cen:lm_add_container("cen1", "c1", new_lm()),
    {Cens, Conts, Wires} = decompose_lm(LM),
    ?assertEqual([cen_map("cen1", ["c1"], bus, 10, "10.10.0.1", ["10.10.0.10"])],
                  Cens),
    ?assertEqual([cont_map("c1", ["cen1"], [0])], Conts),
    ?assertEqual([], Wires).

lm_add_container1() ->
    LM0 = leviathan_cen:lm_add_container("cen1", "c1", new_lm()),
    LM1 = leviathan_cen:lm_add_container("cen1", "c2", LM0),
    {Cens, Conts, Wires} = decompose_lm(LM1),
    ?assertEqual([cen_map("cen1", ["c1", "c2"], bus, 10, "10.10.0.1",
                          ["10.10.0.10", "10.10.0.11"])],
                 Cens),
    ?assertEqualLists(Conts, [cont_map("c1", ["cen1"], [0]),
                              cont_map("c2", ["cen1"], [0])]),
    ?assertEqualLists([
                       [
                        endpoint("c1", "c1.0i", in, "cen1", "10.10.0.10"),
                        endpoint("cen1", "c1.0o", out)
                       ],
                       [
                        endpoint("c2", "c2.0i", in, "cen1", "10.10.0.11"),
                        endpoint("cen1", "c2.0o", out)
                       ]
                      ], Wires).

lm_add_container2() ->
    {Cens, Conts, Wires} = foldcalls(new_lm(), [
        fun(Acc) -> leviathan_cen:lm_add_container("cen1", "c1", Acc) end,
        fun(Acc) -> leviathan_cen:lm_add_container("cen1", "c2", Acc) end,
        fun(Acc) -> leviathan_cen:lm_add_container("cen1", "c3", Acc) end,
        fun decompose_lm/1
    ]),
    ?assertEqual([
                  cen_map("cen1", ["c1", "c2", "c3"], bus, 10, "10.10.0.1",
                          [inet:ntoa({10, 10, 0, X}) || X <- lists:seq(10, 12)])
                 ],
                 Cens),
    ?assertEqualLists([cont_map("c1", ["cen1"], [0]),
                       cont_map("c2", ["cen1"], [0]),
                       cont_map("c3", ["cen1"], [0])],
                      Conts),
    ?assertEqual(3, length(Wires)),
    assert_bus_wires(Wires).

lm_add_container3() ->
    {Cens, Conts, Wires} = foldcalls(
                             new_lm(),
                             [
                              %% cen1
                              fun(Acc) -> leviathan_cen:lm_add_container("cen1", "c1", Acc) end,
                              fun(Acc) -> leviathan_cen:lm_add_container("cen1", "c2", Acc) end,
                              fun(Acc) -> leviathan_cen:lm_add_container("cen1", "c3", Acc) end,
                              %% cen2
                              fun(Acc) -> leviathan_cen:lm_add_container("cen2", "c1", Acc) end,
                              fun(Acc) -> leviathan_cen:lm_add_container("cen2", "c2", Acc) end,
                              fun(Acc) -> leviathan_cen:lm_add_container("cen2", "c3", Acc) end,
                              fun(Acc) -> leviathan_cen:lm_add_container("cen2", "c4", Acc) end,
                              fun decompose_lm/1
                             ]),
    ?assertEqualLists([
                       cen_map("cen1", ["c1", "c2", "c3"], bus, 10, "10.10.0.1",
                               [inet:ntoa({10, 10, 0, X}) || X <- lists:seq(10, 12)]
                              ),
                       cen_map("cen2", ["c1", "c2", "c3", "c4"], bus, 11, "10.11.0.1",
                               [inet:ntoa({10, 11, 0, X}) || X <- lists:seq(10, 13)]
                              )
                      ],
                      Cens),
    ?assertEqualLists([
                       cont_map("c1", ["cen1", "cen2"], [0, 1]),
                       cont_map("c2", ["cen1", "cen2"], [0, 1]),
                       cont_map("c3", ["cen1", "cen2"], [0, 1]),
                       cont_map("c4", ["cen2"], [0])
                      ],
                      Conts),
    ?assertEqual(7, length(Wires)),
    leviathan_test_utils:check_wires(Cens, Wires),
    assert_bus_wires(Wires).

lm_add_container4() ->
    {Cens, _, Wires} = decompose_lm(
        foldcalls(new_lm(), [
            fun(Acc) -> leviathan_cen:add_cen("cen1", Acc) end,
            fun(Acc) -> leviathan_cen:add_cen("cen2", Acc) end,
            fun(Acc) -> leviathan_cen:lm_add_container("cen1", "cA", Acc) end,
            fun(Acc) -> leviathan_cen:lm_add_container("cen2", "cA", Acc) end,
            fun(Acc) -> leviathan_cen:lm_add_container("cen1", "cB", Acc) end,
            fun(Acc) -> leviathan_cen:lm_add_container("cen2", "cB", Acc) end
        ])),
    leviathan_test_utils:check_wires(Cens, Wires).

lm_remove_container0() ->
    LM = leviathan_cen:lm_remove_container("cen1", "c1", new_lm()),
    ?assertEqual(new_lm(), LM).

lm_remove_container1() ->
    Json = [json_cen(<<"cen1">>, [<<"c1">>])],
    LM0 = leviathan_cen:decode_jiffy(Json),
    LM1 = leviathan_cen:lm_remove_container("cen1", "c1", LM0),
    {Cens, Conts, Wires} = decompose_lm(LM1),
    ?assertEqual([cen_map("cen1", [], bus, 10, "10.10.0.1", [])], Cens),
    ?assertEqual([], Conts),
    ?assertEqual([], Wires).

lm_remove_container2() ->
    Json = [json_cen(<<"cen1">>, [<<"c1">>, <<"c2">>])],
    LM0 = leviathan_cen:decode_jiffy(Json),
    LM1 = leviathan_cen:lm_remove_container("cen1", "c1", LM0),
    {Cens, Conts, Wires} = decompose_lm(LM1),
    ?assertEqual([cen_map("cen1", ["c2"], bus, 10, "10.10.0.1", ["10.10.0.11"])], Cens),
    ?assertEqual([cont_map("c2", ["cen1"], [0])], Conts),
    ?assertEqual([], Wires).

lm_remove_container3() ->
    Json = [json_cen(<<"cen1">>, [<<"c1">>, <<"c2">>]),
            json_cen(<<"cen2">>, [<<"c1">>, <<"c3">>])],
    LM0 = leviathan_cen:decode_jiffy(Json),
    LM1 = leviathan_cen:lm_remove_container("cen1", "c1", LM0),
    {Cens, Conts, Wires} = decompose_lm(LM1),
    ?assertEqual([cen_map("cen2", ["c1","c3"], bus, 11, "10.11.0.1", ["10.11.0.10", "10.11.0.11"]),
                  cen_map("cen1", ["c2"], bus, 10, "10.10.0.1", ["10.10.0.11"])], Cens),
    ?assertEqual([cont_map("c3", ["cen2"], [0]),
                  cont_map("c2", ["cen1"], [0]),
                  cont_map("c1", ["cen2"], [1])], Conts),
    ?assertEqual([
        [
            endpoint("c3", "c3.0i", in, "cen2", "10.11.0.11"),
            endpoint("cen2", "c3.0o", out)
        ],
        [
            endpoint("c1", "c1.1i", in, "cen2", "10.11.0.10"),
            endpoint("cen2", "c1.1o", out)
        ]
    ], Wires).

lm_compare0() ->
    ?assertEqual([], leviathan_cen:lm_compare(new_lm(), new_lm())).

lm_compare1() ->
    Json = [json_cen(<<"cen1">>, [<<"c1">>])],
    LM0 = leviathan_cen:decode_jiffy(Json),
    Instructions = [
        {add, cen, cen_map("cen1", ["c1"], bus, 10, "10.10.0.1", ["10.10.0.10"])},
        {add, cont, cont_map("c1", ["cen1"], [0])}
    ],
    ?assertEqual(Instructions, leviathan_cen:lm_compare(new_lm(), LM0)).

lm_compare2() ->
    Json = [json_cen(<<"cen1">>, [<<"c1">>])],
    LM0 = leviathan_cen:decode_jiffy(Json),
    Instructions = [
        {destroy, cen, cen_map("cen1", ["c1"], bus, 10, "10.10.0.1", ["10.10.0.10"])},
        {destroy, cont, cont_map("c1", ["cen1"], [0])}
    ],
    ?assertEqual(Instructions, leviathan_cen:lm_compare(LM0, new_lm())).

lm_compare3() ->
    OldLM = leviathan_cen:decode_jiffy([json_cen(<<"cen1">>, [<<"c1">>])]),
    NewLM = leviathan_cen:decode_jiffy([json_cen(<<"cen1">>,
                                                    [<<"c2">>, <<"c1">>])]),
    Instructions = [
        {add, cont_in_cen, {"c2", "cen1"}},
        {add, cont, cont_map("c2", ["cen1"], [0])},
        {add, wire, [
            endpoint("c1", "c1.0i", in, "cen1", "10.11.0.11"),
            endpoint("cen1", "c1.0o", out)
        ]},
        {add, wire, [
            endpoint("c2", "c2.0i", in, "cen1", "10.11.0.10"),
            endpoint("cen1", "c2.0o", out)
        ]}
    ],
    ?assertEqual(Instructions, leviathan_cen:lm_compare(OldLM, NewLM)).

lm_compare4() ->
    OldLM = leviathan_cen:decode_jiffy([json_cen(<<"cen1">>,
                                                    [<<"c2">>, <<"c1">>])]),
    NewLM = leviathan_cen:decode_jiffy([json_cen(<<"cen1">>, [<<"c1">>])]),
    Instructions = [
        {destroy, cont_in_cen, {"c2", "cen1"}},
        {destroy, cont, cont_map("c2", ["cen1"], [0])},
        {destroy, wire, [
            endpoint("c1", "c1.0i", in, "cen1", "10.10.0.11"),
            endpoint("cen1", "c1.0o", out)
        ]},
        {destroy, wire, [
            endpoint("c2", "c2.0i", in, "cen1", "10.10.0.10"),
            endpoint("cen1", "c2.0o", out)
        ]}
    ],
    ?assertEqual(Instructions, leviathan_cen:lm_compare(OldLM, NewLM)).

lm_compare5() ->
    LM0 = foldcalls(new_lm(), [
                               fun(Acc) -> leviathan_cen:add_cen("cen1", Acc) end,
                               fun(Acc) -> leviathan_cen:add_cen("cen2", Acc) end,
                               fun(Acc) -> leviathan_cen:lm_add_container("cen1", "cA", Acc) end,
                               fun(Acc) -> leviathan_cen:lm_add_container("cen2", "cA", Acc) end,
                               fun(Acc) -> leviathan_cen:lm_add_container("cen1", "cB", Acc) end
                              ]),
    LM1 = leviathan_cen:lm_add_container("cen2", "cB", LM0),
    Instructions = [
                    {add,cont_in_cen,{"cB","cen2"}},
                    {add, wire, [
                                 endpoint("cB", "cB.1i", in, "cen2", "10.11.0.11"),
                                 endpoint("cen2", "cB.1o", out)
                                ]},
                    {add, wire, [
                                 endpoint("cA", "cA.1i", in, "cen2", "10.11.0.10"),
                                 endpoint("cen2", "cA.1o", out)
                                ]}],
    ?assertEqualLists(Instructions, leviathan_cen:lm_compare(LM0, LM1)).

lm_compare6() ->
    LM0 = foldcalls(new_lm(), 
                    [
                     fun(Acc) -> leviathan_cen:add_cen("cen1", Acc) end,
                     fun(Acc) -> leviathan_cen:add_cen("cen2", Acc) end,
                     fun(Acc) -> leviathan_cen:lm_add_container("cen1", "c1", Acc) end,
                     fun(Acc) -> leviathan_cen:lm_add_container("cen1", "c2", Acc) end,
                     fun(Acc) -> leviathan_cen:lm_add_container("cen1", "c3", Acc) end
                    ]),
    LM1 = foldcalls(LM0, 
                    [
                     fun(Acc) -> leviathan_cen:lm_add_container("cen2", "c3", Acc) end,
                     fun(Acc) -> leviathan_cen:lm_add_container("cen2", "c4", Acc) end
                    ]),
    Instructions = 
        [
         {add,cont_in_cen,{"c4","cen2"}},
         {add,cont_in_cen,{"c3","cen2"}},
         {add,cont,cont_map("c4", ["cen2"], [0])},
         {add, wire, [
                      endpoint("c3", "c3.1i", in, "cen2", "10.11.0.10"),
                      endpoint("cen2", "c3.1o", out)
                     ]},
         {add, wire, [
                      endpoint("c4", "c4.0i", in, "cen2", "10.11.0.11"),
                      endpoint("cen2", "c4.0o", out)
                     ]}],
    ?assertEqual(lists:sort(Instructions),
                 lists:sort(leviathan_cen:lm_compare(LM0, LM1))).

lm_add_container_to_new_cen() ->
    LM0 = foldcalls(new_lm(), 
                    [
                     fun(Acc) -> leviathan_cen:add_cen("cen1", Acc) end,
                     fun(Acc) -> leviathan_cen:lm_add_container("cen1", "c1", Acc) end,
                     fun(Acc) -> leviathan_cen:lm_add_container("cen1", "c2", Acc) end
                    ]),
    LM1 = foldcalls(LM0, 
                    [
                     fun(Acc) -> leviathan_cen:add_cen("cen2", Acc) end,
                     fun(Acc) -> leviathan_cen:lm_add_container("cen2", "c2", Acc) end,
                     fun(Acc) -> leviathan_cen:lm_add_container("cen2", "c3", Acc) end
                    ]),
    #{wiremap := #{wires := LM0Wires}} = LM0,
    #{wiremap := #{wires := LM1Wires}} = LM1,
    lists:foreach(fun(Wire) ->
                          ?assert(lists:member(Wire, LM1Wires))
                  end, LM0Wires).

lm_add_cen0() ->
    OldLM = new_lm(),
    NewLM = leviathan_cen:add_cen("cen1", OldLM),
    Instructions = [{add, cen, #{cenID => "cen1",
                                 contIDs => [],
                                 ip_address => "10.10.0.1",
                                 ipaddr_b => 10,
                                 reservedIps => [],
                                 wire_type => bus}}],
    ?assertEqual(Instructions, leviathan_cen:lm_compare(OldLM, NewLM)).

get_levmap0() ->
    %% GIVEN
    Cen = cen_map(CenIdToCheck = "cen1", ["c1"], bus, 10, "10.10.0.1", ["10.10.0.10"]),
    Cont = cont_map("c1", ["cen1"], [0]),
    Wires = [],
    ExpectedLM = compose_lm([Cen], [Cont], Wires),
    leviathan_store:import_cens(<<"host">>, ExpectedLM),

    %% WHEN
    ActualLM = leviathan_store:get_levmap([CenIdToCheck]),

    %% THEN
    ?assertEqual(ExpectedLM, ActualLM).

get_levmap1() ->
    %% GIVEN
    Cen1 = cen_map(CenIdToCheck = "cen1", ["c1", "c2"], bus, 10, "10.10.0.1",
                   ["10.10.0.10", "10.10.0.11"]),
    Cont1 = cont_map("c1", ["cen1"], [0]),
    Cont2 = cont_map("c2", ["cen1"], [0]),
    Wire1 = [
             endpoint("c1", "c1.0i", in, "cen1", "10.10.0.10"),
             endpoint("cen1", "c1.0o", out)
            ],
    Wire2 = [
             endpoint("c2", "c2.0i", in, "cen1", "10.10.0.11"),
             endpoint("cen1", "c2.0o", out)
            ],
    ExpectedLM = compose_lm([Cen1], [Cont1, Cont2], [Wire1, Wire2]),
    leviathan_store:import_cens(<<"host">>, ExpectedLM),

    %% WHEN
    ActualLM = leviathan_store:get_levmap([CenIdToCheck]),

    %% THEN
    {ActualCens, ActualConts, ActualWires} = decompose_lm(ActualLM),
    ?assertEqualLists([Cen1], ActualCens),
    ?assertEqualLists([Cont1, Cont2], ActualConts),
    ?assertEqualLists([Wire1, Wire2], ActualWires).

get_levmap2() ->
    %% GIVEN
    Cen1 = cen_map(CenIdToCheck = "cen1", ["c1", "c2"], bus, 10, "10.10.0.1",
                   ["10.10.0.10", "10.10.0.11"]),
    Cen2 = cen_map("cen2", ["c2"], bus, 11, "10.11.0.1", ["10.11.0.10"]),
    Cont1 = cont_map("c1", ["cen1"], [0]),
    Cont2 = cont_map("c2", ["cen1", "cen2"], [0, 1]),
    Wire1 = [
             endpoint("c1", "c1.0i", in, "cen1", "10.10.0.10"),
             endpoint("cen1", "c1.0o", out)
            ],
    Wire2 = [
             endpoint("c2", "c2.0i", in, "cen1", "10.10.0.11"),
             endpoint("cen1", "c2.0o", out)
            ],
    ExpectedLM = compose_lm([Cen1, Cen2], [Cont1, Cont2], [Wire1, Wire2]),
    leviathan_store:import_cens(<<"host">>, ExpectedLM),

    %% WHEN
    ActualLM = leviathan_store:get_levmap([CenIdToCheck]),

    %% THEN
    {ActualCens, ActualConts, ActualWires} = decompose_lm(ActualLM),
    ?assertEqualLists([Cen1], ActualCens),
    ?assertEqualLists([Cont1, Cont2], ActualConts),
    ?assertEqualLists([Wire1, Wire2], ActualWires).

get_levmap3() ->
    %% GIVEN
    Cen1 = cen_map("cen1", ["c1", "c2"], bus, 10, "10.10.0.1",
                   ["10.10.0.10", "10.10.0.11"]),
    Cen2 = cen_map(CenIdToCheck = "cen2", ["c3", "c4"], bus, 11, "10.11.0.1",
                   ["10.11.0.10", "10.11.0.11"]),
    Cont1 = cont_map("c1", ["cen1"], [0]),
    Cont2 = cont_map("c2", ["cen1"], [0]),
    Cont3 = cont_map("c3", ["cen2"], [0]),
    Cont4 = cont_map("c4", ["cen2"], [0]),
    Wire1 = [
             endpoint("c1", "c1.0i", in, "cen1", "10.10.0.10"),
             endpoint("cen1", "c1.0o", out)
            ],
    Wire2 = [
             endpoint("c2", "c2.0i", in, "cen1", "10.10.0.11"),
             endpoint("cen1", "c2.0o", out)
            ],
    Wire3 = [
             endpoint("c3", "c3.0i", in, "cen2", "10.11.0.10"),
             endpoint("cen2", "c3.0o", out)
            ],
    Wire4 = [
             endpoint("c4", "c4.0i", in, "cen2", "10.11.0.11"),
             endpoint("cen2", "c4.0o", out)
            ],
    ExpectedLM = compose_lm([Cen1, Cen2],
                            [Cont1, Cont2, Cont3, Cont4],
                            [Wire1, Wire2, Wire3, Wire4]),
    leviathan_store:import_cens(<<"host">>, ExpectedLM),

    %% WHEN
    ActualLM = leviathan_store:get_levmap([CenIdToCheck]),

    %% THEN
    {ActualCens, ActualConts, ActualWires} = decompose_lm(ActualLM),
    ?assertEqualLists([Cen2], ActualCens),
    ?assertEqualLists([Cont3, Cont4], ActualConts),
    ?assertEqualLists([Wire3, Wire4], ActualWires).

get_levmap4() ->
    %% GIVEN
    Cen1 = cen_map("cen1", ["c1", "c2"], bus, 10, "10.10.0.1",
                   ["10.10.0.10", "10.10.0.11"]),
    Cen2 = cen_map(CenIdToCheck = "cen2", ["c2", "c3"], bus, 11, "10.11.0.1",
                   ["10.11.0.10", "10.11.0.11"]),
    Cont1 = cont_map("c1", ["cen1"], [0]),
    Cont2 = cont_map("c2", ["cen1", "cen2"], [0, 1]),
    Cont3 = cont_map("c3", ["cen2"], [0]),
    Wire1 = [
             endpoint("c1", "c1.0i", in, "cen1", "10.10.0.10"),
             endpoint("cen1", "c1.0o", out)
            ],
    Wire2 = [
             endpoint("c2", "c2.0i", in, "cen1", "10.10.0.11"),
             endpoint("cen1", "c2.0o", out)
            ],
    Wire3 = [
             endpoint("c2", "c2.1i", in, "cen2", "10.11.0.10"),
             endpoint("cen2", "c2.1o", out)
            ],
    Wire4 = [
             endpoint("c3", "c3.0i", in, "cen2", "10.11.0.11"),
             endpoint("cen2", "c3.0o", out)
            ],
    ExpectedLM = compose_lm([Cen1, Cen2],
                            [Cont1, Cont2, Cont3],
                            [Wire1, Wire2, Wire3, Wire4]),
    leviathan_store:import_cens(<<"host">>, ExpectedLM),

    %% WHEN
    ActualLM = leviathan_store:get_levmap([CenIdToCheck]),

    %% THEN
    {ActualCens, ActualConts, ActualWires} = decompose_lm(ActualLM),
    ?assertEqualLists([Cen2], ActualCens),
    ?assertEqualLists([Cont2, Cont3], ActualConts),
    ?assertEqualLists([Wire3, Wire4], ActualWires).

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

compose_lm(Cens, Conts, Wires) ->
    #{censmap => #{cens => Cens},
      contsmap => #{conts => Conts},
      wiremap => #{wires => Wires}}.

json_cen(CenIdToCheck, ContIds) ->
    #{<<"cenID">> => CenIdToCheck, <<"containerIDs">> => ContIds}.

cen_map(CenIdToCheck, ContIds, WireType, IpAddrB, IpAddr) ->
    cen_map(CenIdToCheck, ContIds, WireType, IpAddrB, IpAddr, []).

cen_map(CenIdToCheck, ContIds, WireType, IpAddrB, IpAddr, ReservedIp) ->
    #{cenID => CenIdToCheck,
      wire_type => WireType,
      contIDs => ContIds,
      ipaddr_b => IpAddrB,
      ip_address => IpAddr,
      reservedIps => ReservedIp}.

cont_map(ContId, CenIds, ReservedIds) ->
    #{contID => ContId,
      cens => CenIds,
      reservedIdNums => ReservedIds}.

endpoint(ContId, EndId, Side, Alias, IpAddr) ->
    #{endID => EndId,
      side => Side,
      dest => #{alias => Alias,
                id => ContId,
                ip_address => IpAddr,
                type => cont}}.

endpoint(CenIdToCheck, EndId, Side) ->
    #{endID => EndId,
      side => Side,
      dest => #{id => CenIdToCheck,
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

foldcalls(Starting, Fns) ->
    lists:foldl(fun(Fn, Acc) -> Fn(Acc) end, Starting, Fns).

tr() ->
    dbg:start(),
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tpl(leviathan_cen, []).

troff() ->
    dbg:ctpl(leviathan_cen).
