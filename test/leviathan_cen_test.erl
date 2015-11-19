-module(leviathan_cen_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

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
       ,{"lm_remove_container test 4", fun lm_remove_container4/0}
       ,{"lm_compare_when_adding 0", fun lm_compare_when_adding0/0}
       ,{"lm_compare_when_adding 1", fun lm_compare_when_adding1/0}
       ,{"lm_compare_when_adding 2", fun lm_compare_when_adding2/0}
       ,{"lm_compare_when_adding 3", fun lm_compare_when_adding3/0}
       ,{"lm_compare_when_adding 4", fun lm_compare_when_adding4/0}
       ,{"lm_compare_when_adding 5", fun lm_compare_when_adding5/0}
       ,{"lm_compare_when_adding 6", fun lm_compare_when_adding6/0}
       ,{"lm_compare_when_destroying 1", fun lm_compare_when_destroying1/0}
       ,{"lm_compare_when_destroying 2", fun lm_compare_when_destroying2/0}
       ,{"lm_compare_when_destroying 3", fun lm_compare_when_destroying3/0}
       ,{"lm_add_container_to_new_cen", fun lm_add_container_to_new_cen/0}
       ,{"get levmap from store 0", fun get_levmap0/0}
       ,{"get levmap from store 1", fun get_levmap1/0}
       ,{"get levmap from store 2", fun get_levmap2/0}
       ,{"get levmap from store 3", fun get_levmap3/0}
       ,{"get levmap from store 4", fun get_levmap4/0}
       ,{"update cens in store 0", fun update_cens0/0}
       ,{"update cens in store 1", fun update_cens1/0}
       ,{"update cens in store 2", fun update_cens2/0}
       ,{"update cens in store 3", fun update_cens3/0}
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
    Cen1 = cen_map("cen1", [], bus),
    {Cens, Conts, Wires} = decompose_lm(leviathan_cen:decode_jiffy(Json)),
    ?assertEqualLists([Cen1], Cens),
    ?assertEqualLists([], Conts),
    ?assertEqualLists([], Wires).

decode_jiffy2() ->
    Json = [json_cen(<<"cen1">>, [<<"c1">>])],
    Cen1 = cen_map("cen1", ["c1"], bus),
    Cont1 = cont_map("c1", ["cen1"], [0]),
    {Cens, Conts, Wires} = decompose_lm(leviathan_cen:decode_jiffy(Json)),
    ?assertEqualLists([Cen1], Cens),
    ?assertEqualLists([Cont1], Conts),
    ?assertEqualLists([], Wires).

decode_jiffy3() ->
    Json = [json_cen(<<"cen1">>, [<<"c1">>, <<"c2">>])],
    Cen1 = cen_map("cen1", ["c1", "c2"], bus),
    Cont1 = cont_map("c1", ["cen1"], [0]),
    Cont2 = cont_map("c2", ["cen1"], [0]),
    Wire1 = [
             endpoint("c1", "c1.0i", in, "cen1"),
             endpoint("cen1", "c1.0o", out)
    ],
    Wire2 = [
             endpoint("c2", "c2.0i", in, "cen1"),
             endpoint("cen1", "c2.0o", out)
    ],
    {Cens, Conts, Wires} = decompose_lm(leviathan_cen:decode_jiffy(Json)),
    ?assertEqualLists([Cen1], Cens),
    ?assertEqualLists([Cont1, Cont2], Conts),
    ?assertEqualLists([Wire1, Wire2], Wires).

decode_jiffy4() ->
    Json = [json_cen(<<"cen1">>, [<<"c1">>, <<"c2">>, <<"c3">>])],
    Cen1 = cen_map("cen1", ["c1", "c2", "c3"], bus),
    Cont1 = cont_map("c1", ["cen1"], [0]),
    Cont2 = cont_map("c2", ["cen1"], [0]),
    Cont3 = cont_map("c3", ["cen1"], [0]),
    Wire1 = [
             endpoint("c1", "c1.0i", in, "cen1"),
             endpoint("cen1", "c1.0o", out)
    ],
    Wire2 = [
             endpoint("c2", "c2.0i", in, "cen1"),
             endpoint("cen1", "c2.0o", out)
    ],
    Wire3 = [
             endpoint("c3", "c3.0i", in, "cen1"),
             endpoint("cen1", "c3.0o", out)
    ],
    {Cens, Conts, Wires} = decompose_lm(leviathan_cen:decode_jiffy(Json)),
    ?assertEqualLists([Cen1], Cens),
    ?assertEqualLists([Cont1, Cont2, Cont3], Conts),
    ?assertEqualLists([Wire1, Wire2, Wire3], Wires).

lm_add_container0() ->
    LM = leviathan_cen:lm_add_container("cen1", "c1", new_lm()),
    ExpectedLM = compose_lm(
                   [cen_map("cen1", ["c1"], bus)],
                   [cont_map("c1", ["cen1"], [0])],
                   []),
    assert_equal_levmaps(ExpectedLM, LM).

lm_add_container1() ->
    LM0 = leviathan_cen:lm_add_container("cen1", "c1", new_lm()),
    LM1 = leviathan_cen:lm_add_container("cen1", "c2", LM0),
    ExpectedLM = compose_lm(
                   [cen_map("cen1", ["c1", "c2"], bus)],
                   [cont_map("c1", ["cen1"], [0]),
                    cont_map("c2", ["cen1"], [0])],
                   [
                    [
                     endpoint("c1", "c1.0i", in, "cen1"),

                     endpoint("cen1", "c1.0o", out)
                    ],
                    [
                     endpoint("c2", "c2.0i", in, "cen1"),
                     endpoint("cen1", "c2.0o", out)
                    ]
                   ]),
    assert_equal_levmaps(ExpectedLM, LM1).

lm_add_container2() ->
    {Cens, Conts, Wires} = foldcalls(new_lm(), [
        fun(Acc) -> leviathan_cen:lm_add_container("cen1", "c1", Acc) end,
        fun(Acc) -> leviathan_cen:lm_add_container("cen1", "c2", Acc) end,
        fun(Acc) -> leviathan_cen:lm_add_container("cen1", "c3", Acc) end,
        fun decompose_lm/1
    ]),
    ?assertEqual([cen_map("cen1", ["c1", "c2", "c3"], bus)], Cens),
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
                       cen_map("cen1", ["c1", "c2", "c3"], bus),
                       cen_map("cen2", ["c1", "c2", "c3", "c4"], bus)
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
    leviathan_test_utils:check_wires(Wires),
    assert_bus_wires(Wires).

lm_add_container4() ->
    {_, _, Wires} = decompose_lm(
        foldcalls(new_lm(), [
            fun(Acc) -> leviathan_cen:add_cen("cen1", Acc) end,
            fun(Acc) -> leviathan_cen:add_cen("cen2", Acc) end,
            fun(Acc) -> leviathan_cen:lm_add_container("cen1", "cA", Acc) end,
            fun(Acc) -> leviathan_cen:lm_add_container("cen2", "cA", Acc) end,
            fun(Acc) -> leviathan_cen:lm_add_container("cen1", "cB", Acc) end,
            fun(Acc) -> leviathan_cen:lm_add_container("cen2", "cB", Acc) end
        ])),
    leviathan_test_utils:check_wires(Wires).

lm_remove_container0() ->
    LM = leviathan_cen:lm_remove_container("cen1", "c1", new_lm()),
    ?assertEqual(new_lm(), LM).

lm_remove_container1() ->
    LM0 = leviathan_cen:lm_add_container("cen1", "c1", new_lm()),
    LM1 = leviathan_cen:lm_remove_container("cen1", "c1", LM0),
    Cens = [cen_map("cen1", [], bus)],
    assert_equal_levmaps(compose_lm(Cens, [], []), LM1).

lm_remove_container2() ->
    LM0 = foldcalls(new_lm(),
                    [
                     fun(Acc) -> leviathan_cen:lm_add_container("cen1", "c1", Acc) end,
                     fun(Acc) -> leviathan_cen:lm_add_container("cen1", "c2", Acc) end
                    ]),
    LM1 = leviathan_cen:lm_remove_container("cen1", "c1", LM0),
    ExpectedLM = compose_lm([cen_map("cen1", ["c2"], bus)],
                            [cont_map("c2", ["cen1"], [0])],
                            []),
    assert_equal_levmaps(ExpectedLM, LM1).

lm_remove_container3() ->
    LM0 = foldcalls(new_lm(),
                    [
                     fun(Acc) -> leviathan_cen:lm_add_container("cen1", "c1", Acc) end,
                     fun(Acc) -> leviathan_cen:lm_add_container("cen1", "c2", Acc) end
                    ]),
    LM1 = leviathan_cen:lm_remove_container("cen1", "c1", LM0),
    ExpectedLM = compose_lm([cen_map("cen1", ["c2"], bus)],
                            [cont_map("c2", ["cen1"], [0])],
                            []),
    assert_equal_levmaps(ExpectedLM, LM1).

lm_remove_container4() ->
    LM0 = foldcalls(new_lm(),
                    [
                     fun(Acc) -> leviathan_cen:lm_add_container("cen1", "c1", Acc) end,
                     fun(Acc) -> leviathan_cen:lm_add_container("cen1", "c2", Acc) end,
                     fun(Acc) -> leviathan_cen:lm_add_container("cen2", "c1", Acc) end,
                     fun(Acc) -> leviathan_cen:lm_add_container("cen2", "c3", Acc) end
                    ]),
    LM1 = leviathan_cen:lm_remove_container("cen1", "c1", LM0),
    ExpectedLM = compose_lm(
                   [cen_map("cen2", ["c1","c3"], bus),
                    cen_map("cen1", ["c2"], bus)],
                   [cont_map("c3", ["cen2"], [0]),
                    cont_map("c2", ["cen1"], [0]),
                    cont_map("c1", ["cen2"], [1])], 
                   [
                    [
                     endpoint("c3", "c3.0i", in, "cen2"),
                     endpoint("cen2", "c3.0o", out)
                    ],
                    [
                     endpoint("c1", "c1.1i", in, "cen2"),
                     endpoint("cen2", "c1.1o", out)
                    ]
                   ]),
    assert_equal_levmaps(ExpectedLM, LM1).

%% Compare two empty Leviathan Maps.
lm_compare_when_adding0() ->
    ?assertEqual([], leviathan_cen:lm_compare(new_lm(), new_lm())).

%% Add a new Cen with a new Container.
lm_compare_when_adding1() ->
    LM0 = leviathan_cen:lm_add_container("cen1", "c1", new_lm()),
    Cen = cen_map("cen1", ["c1"], bus),
    Cont = cont_map("c1", ["cen1"], [0]),
    Instructions =
        [
         {add, cen, Cen},
         {add, cont, Cont},
         {add, cont_in_cen, {Cont, Cen}}
        ],
    assert_equal_instructions(Instructions, leviathan_cen:lm_compare(new_lm(), LM0)).

%% Add a new Container to an existing Cen.
lm_compare_when_adding2() ->
    OldLM = foldcalls(new_lm(),
                      [
                       fun(Acc) -> leviathan_cen:add_cen("cen1", Acc) end,
                       fun(Acc) -> leviathan_cen:lm_add_container("cen1", "c1", Acc) end
                      ]),
    NewLM = leviathan_cen:lm_add_container("cen1", "c2", OldLM),
    Instructions =
        [
         {add, cont_in_cen, {cont_map("c2", ["cen1"], [0]),
                             cen_map("cen1", ["c1", "c2"], bus)
                            }},
         {add, cont, cont_map("c2", ["cen1"], [0])},
         {add, wire, [
                      endpoint("c1", "c1.0i", in, "cen1"),
                      endpoint("cen1", "c1.0o", out)
                     ]},
         {add, wire, [
                      endpoint("c2", "c2.0i", in, "cen1"),
                      endpoint("cen1", "c2.0o", out)
                     ]}
        ],
    assert_equal_instructions(Instructions, leviathan_cen:lm_compare(OldLM, NewLM)).

%% Add an existing Container to an existing Cen.
lm_compare_when_adding3() ->
    LM0 = foldcalls(new_lm(), [
                               fun(Acc) -> leviathan_cen:add_cen("cen1", Acc) end,
                               fun(Acc) -> leviathan_cen:add_cen("cen2", Acc) end,
                               fun(Acc) -> leviathan_cen:lm_add_container("cen1", "cA", Acc) end,
                               fun(Acc) -> leviathan_cen:lm_add_container("cen2", "cA", Acc) end,
                               fun(Acc) -> leviathan_cen:lm_add_container("cen1", "cB", Acc) end
                              ]),
    LM1 = leviathan_cen:lm_add_container("cen2", "cB", LM0),
    Instructions = [
                    {add, cont_in_cen, {cont_map("cB", ["cen1", "cen2"], [0, 1]),
                                        cen_map("cen2", ["cA", "cB"], bus)
                                       }},
                    {add, wire, [
                                 endpoint("cB", "cB.1i", in, "cen2"),
                                 endpoint("cen2", "cB.1o", out)
                                ]},
                    {add, wire, [
                                 endpoint("cA", "cA.1i", in, "cen2"),
                                 endpoint("cen2", "cA.1o", out)
                                ]}],
    assert_equal_instructions(Instructions, leviathan_cen:lm_compare(LM0, LM1)).

%% Add exsisting containers to an eixsting empty Cen.
lm_compare_when_adding4() ->
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
         {add, cont_in_cen, {cont_map("c4", ["cen2"], [0]),
                             cen_map("cen2", ["c3", "c4"], bus)
                            }},
         {add, cont_in_cen, {cont_map("c3", ["cen1", "cen2"], [0, 1]),
                             cen_map("cen2", ["c3", "c4"], bus)
                            }},
         {add,cont, cont_map("c4", ["cen2"], [0])},
         {add, wire, [
                      endpoint("c3", "c3.1i", in, "cen2"),
                      endpoint("cen2", "c3.1o", out)
                     ]},
         {add, wire, [
                      endpoint("c4", "c4.0i", in, "cen2"),
                      endpoint("cen2", "c4.0o", out)
                     ]}],
    AInstructions = leviathan_cen:lm_compare(LM0, LM1),
    assert_equal_instructions(Instructions, AInstructions).

%% Add an existing container to a new Cen.
lm_compare_when_adding5() ->
    LM0 = foldcalls(new_lm(), 
                    [
                     fun(Acc) -> leviathan_cen:add_cen("cen1", Acc) end,
                     fun(Acc) -> leviathan_cen:lm_add_container("cen1", "c1", Acc) end
                    ]),
    LM1 = leviathan_cen:lm_add_container("cen2", "c1", LM0),
    Cen = cen_map("cen2", ["c1"], bus),
    Instructions = 
        [
         {add, cont_in_cen, {cont_map("c1", ["cen1", "cen2"], [0, 1]),
                             Cen
                            }},
         {add, cen, Cen}
        ],
    assert_equal_instructions(Instructions, leviathan_cen:lm_compare(LM0, LM1)).

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

%% Add an empty Cen.
lm_compare_when_adding6() ->
    OldLM = new_lm(),
    NewLM = leviathan_cen:add_cen("cen1", OldLM),
    Instructions = [{add, cen, #{cenID => "cen1",
                                 contIDs => [],
                                 wire_type => bus}}],
    assert_equal_instructions(Instructions, leviathan_cen:lm_compare(OldLM, NewLM)).

%% Remove an empty Cen
lm_compare_when_destroying1() ->
    NewLM = new_lm(),
    OldLM = leviathan_cen:add_cen("cen1", NewLM),
    Instructions = [{destroy, cen, cen_map("cen1", [], bus)}],
    assert_equal_instructions(Instructions, leviathan_cen:lm_compare(OldLM, NewLM)).

%% Remove a Cen with a Cont
lm_compare_when_destroying2() ->
    LM0 = leviathan_cen:lm_add_container("cen1", "c1", new_lm()),
    Cen = cen_map("cen1", ["c1"], bus),
    Cont = cont_map("c1", ["cen1"], [0]),
    Instructions =
        [
         {destroy, cen, Cen},
         {destroy, cont, Cont},
         {destroy, cont_in_cen,  {Cont, Cen}}
        ],
    assert_equal_instructions(Instructions, leviathan_cen:lm_compare(LM0, new_lm())).

%% Remove a Container from a Cen.
lm_compare_when_destroying3() ->
    NewLM = foldcalls(new_lm(),
                      [
                       fun(Acc) -> leviathan_cen:add_cen("cen1", Acc) end,
                       fun(Acc) -> leviathan_cen:lm_add_container("cen1", "c1", Acc) end
                      ]),
    OldLM = leviathan_cen:lm_add_container("cen1", "c2", NewLM),
    Instructions =
        [
         {destroy, cont_in_cen, {cont_map("c2", ["cen1"], [0]),
                                 cen_map("cen1", ["c1", "c2"], bus)
                                }},
         {destroy, cont, cont_map("c2", ["cen1"], [0])},
         {destroy, wire, [
                          endpoint("c1", "c1.0i", in, "cen1"),
                          endpoint("cen1", "c1.0o", out)
                         ]},
         {destroy, wire, [
                          endpoint("c2", "c2.0i", in, "cen1"),
                          endpoint("cen1", "c2.0o", out)
                         ]}
        ],
    assert_equal_instructions(Instructions, leviathan_cen:lm_compare(OldLM, NewLM)).

get_levmap0() ->
    %% GIVEN
    Cen = cen_map(CenIdToCheck = "cen1", ["c1"], bus),
    Cont = cont_map("c1", ["cen1"], [0]),
    Wires = [],
    ExpectedLM = compose_lm([Cen], [Cont], Wires),
    leviathan_cen_store:import_cens(<<"host">>, ExpectedLM),

    %% WHEN
    ActualLM = leviathan_cen_store:get_levmap([CenIdToCheck]),

    %% THEN
    assert_equal_levmaps(ExpectedLM, ActualLM).

get_levmap1() ->
    %% GIVEN
    Cen1 = cen_map(CenIdToCheck = "cen1", ["c1", "c2"], bus),
    Cont1 = cont_map("c1", ["cen1"], [0]),
    Cont2 = cont_map("c2", ["cen1"], [0]),
    Wire1 = [
             endpoint("c1", "c1.0i", in, "cen1"),
             endpoint("cen1", "c1.0o", out)
            ],
    Wire2 = [
             endpoint("c2", "c2.0i", in, "cen1"),
             endpoint("cen1", "c2.0o", out)
            ],
    ExpectedLM = compose_lm([Cen1], [Cont1, Cont2], [Wire1, Wire2]),
    leviathan_cen_store:import_cens(<<"host">>, ExpectedLM),

    %% WHEN
    ActualLM = leviathan_cen_store:get_levmap([CenIdToCheck]),

    %% THEN
    assert_equal_levmaps(ExpectedLM, ActualLM).

get_levmap2() ->
    %% GIVEN
    Cen1 = cen_map(CenIdToCheck = "cen1", ["c1", "c2"], bus),
    Cen2 = cen_map("cen2", ["c2"], bus),
    Cont1 = cont_map("c1", ["cen1"], [0]),
    Cont2 = cont_map("c2", ["cen1", "cen2"], [0, 1]),
    Wire1 = [
             endpoint("c1", "c1.0i", in, "cen1"),
             endpoint("cen1", "c1.0o", out)
            ],
    Wire2 = [
             endpoint("c2", "c2.0i", in, "cen1"),
             endpoint("cen1", "c2.0o", out)
            ],
    leviathan_cen_store:import_cens(<<"host">>, compose_lm([Cen1, Cen2],
                                                       [Cont1, Cont2],
                                                       [Wire1, Wire2])),

    %% WHEN
    ActualLM = leviathan_cen_store:get_levmap([CenIdToCheck]),

    %% THEN
    ExpectedLM = compose_lm([Cen1], [Cont1, Cont2], [Wire1, Wire2]),
    assert_equal_levmaps(ExpectedLM, ActualLM).

get_levmap3() ->
    %% GIVEN
    Cen1 = cen_map("cen1", ["c1", "c2"], bus),
    Cen2 = cen_map(CenIdToCheck = "cen2", ["c3", "c4"], bus),
    Cont1 = cont_map("c1", ["cen1"], [0]),
    Cont2 = cont_map("c2", ["cen1"], [0]),
    Cont3 = cont_map("c3", ["cen2"], [0]),
    Cont4 = cont_map("c4", ["cen2"], [0]),
    Wire1 = [
             endpoint("c1", "c1.0i", in, "cen1"),
             endpoint("cen1", "c1.0o", out)
            ],
    Wire2 = [
             endpoint("c2", "c2.0i", in, "cen1"),
             endpoint("cen1", "c2.0o", out)
            ],
    Wire3 = [
             endpoint("c3", "c3.0i", in, "cen2"),
             endpoint("cen2", "c3.0o", out)
            ],
    Wire4 = [
             endpoint("c4", "c4.0i", in, "cen2"),
             endpoint("cen2", "c4.0o", out)
            ],
    leviathan_cen_store:import_cens(<<"host">>,
                                compose_lm([Cen1, Cen2],
                                           [Cont1, Cont2, Cont3, Cont4],
                                           [Wire1, Wire2, Wire3, Wire4])),
    
    %% WHEN
    ActualLM = leviathan_cen_store:get_levmap([CenIdToCheck]),

    %% THEN
    ExpectedLM = compose_lm([Cen2], [Cont3, Cont4], [Wire3, Wire4]),
    assert_equal_levmaps(ExpectedLM, ActualLM).

get_levmap4() ->
    %% GIVEN
    Cen1 = cen_map("cen1", ["c1", "c2"], bus),
    Cen2 = cen_map(CenIdToCheck = "cen2", ["c2", "c3"], bus),
    Cont1 = cont_map("c1", ["cen1"], [0]),
    Cont2 = cont_map("c2", ["cen1", "cen2"], [0, 1]),
    Cont3 = cont_map("c3", ["cen2"], [0]),
    Wire1 = [
             endpoint("c1", "c1.0i", in, "cen1"),
             endpoint("cen1", "c1.0o", out)
            ],
    Wire2 = [
             endpoint("c2", "c2.0i", in, "cen1"),
             endpoint("cen1", "c2.0o", out)
            ],
    Wire3 = [
             endpoint("c2", "c2.1i", in, "cen2"),
             endpoint("cen2", "c2.1o", out)
            ],
    Wire4 = [
             endpoint("c3", "c3.0i", in, "cen2"),
             endpoint("cen2", "c3.0o", out)
            ],
    leviathan_cen_store:import_cens(<<"host">>,
                                compose_lm([Cen1, Cen2],
                                           [Cont1, Cont2, Cont3],
                                           [Wire1, Wire2, Wire3, Wire4])),

    %% WHEN
    ActualLM = leviathan_cen_store:get_levmap([CenIdToCheck]),

    %% THEN
    ExpectedLM = compose_lm([Cen2], [Cont2, Cont3], [Wire3, Wire4]),
    assert_equal_levmaps(ExpectedLM, ActualLM).

update_cens0() ->
    %% GIVEN
    Cen = cen_map(CenIdToCheck = "cen1", ["c1"], bus),
    Cont = cont_map("c1", ["cen1"], [0]),
    ExpectedLM = compose_lm([Cen], [Cont], _Wires = []),
    Delta = leviathan_cen:lm_compare(new_lm(), ExpectedLM),
        
    %% WHEN
    ok = leviathan_cen_store:update_cens(<<"host">>, Delta),

    %% THEN
    ActualLM = leviathan_cen_store:get_levmap([CenIdToCheck]),
    assert_equal_levmaps(ExpectedLM, ActualLM).

update_cens1() ->
    %% GIVEN
    Cen1 = cen_map(CenIdToCheck = "cen1", ["c1", "c2"], bus),
    Cont1 = cont_map("c1", ["cen1"], [0]),
    Cont2 = cont_map("c2", ["cen1"], [0]),
    Wire1 = [
             endpoint("c1", "c1.0i", in, "cen1"),
             endpoint("cen1", "c1.0o", out)
            ],
    Wire2 = [
             endpoint("c2", "c2.0i", in, "cen1"),
             endpoint("cen1", "c2.0o", out)
            ],
    ExpectedLM = compose_lm([Cen1], [Cont1, Cont2], [Wire1, Wire2]),
    Delta = leviathan_cen:lm_compare(new_lm(), ExpectedLM),
    
    %% WHEN
    ok = leviathan_cen_store:update_cens(<<"host">>, Delta),
    
    %% THEN
    ActualLM = leviathan_cen_store:get_levmap([CenIdToCheck]),
    assert_equal_levmaps(ExpectedLM, ActualLM).

update_cens2() ->
    %% GIVEN
    LM0 = foldcalls(new_lm(),
                    [
                     fun(Acc) -> leviathan_cen:lm_add_container("cen1", "c1", Acc) end,
                     fun(Acc) -> leviathan_cen:lm_add_container("cen1", "c2", Acc) end,
                     fun(Acc) -> leviathan_cen:add_cen("cen2", Acc) end
                    ]),
    ok = leviathan_cen_store:import_cens(<<"host">>, LM0),
    LM1 = leviathan_cen:lm_add_container("cen2", "c2", LM0),
    Cen1 = cen_map("cen1", ["c1", "c2"], bus),
    Cen2 = cen_map("cen2", ["c2"], bus),
    Cont1 = cont_map("c1", ["cen1"], [0]),
    Cont2 = cont_map("c2", ["cen1", "cen2"], [0, 1]),
    Wire1 = [
             endpoint("c1", "c1.0i", in, "cen1"),
             endpoint("cen1", "c1.0o", out)
            ],
    Wire2 = [
             endpoint("c2", "c2.0i", in, "cen1"),
             endpoint("cen1", "c2.0o", out)
            ],
    ExpectedLM = compose_lm([Cen1, Cen2], [Cont1, Cont2], [Wire1, Wire2]),
    Delta = leviathan_cen:lm_compare(LM0, LM1),

    %% WHEN
    ok = leviathan_cen_store:update_cens(<<"host">>, Delta),

    %% THEN
    ActualLM = leviathan_cen_store:get_levmap(["cen1", "cen2"]),
    assert_equal_levmaps(ExpectedLM, ActualLM).

update_cens3() ->
    %% GIVEN
    LM0 = foldcalls(new_lm(),
                    [
                     fun(Acc) -> leviathan_cen:lm_add_container("cen1", "c1", Acc) end,
                     fun(Acc) -> leviathan_cen:lm_add_container("cen1", "c2", Acc) end,
                     fun(Acc) -> leviathan_cen:add_cen("cen2", Acc) end
                    ]),
    ok = leviathan_cen_store:import_cens(<<"host">>, LM0),
    LM1 = foldcalls(LM0,
                    [
                     fun(Acc) -> leviathan_cen:lm_add_container("cen2", "c2", Acc) end,
                     fun(Acc) -> leviathan_cen:lm_add_container("cen2", "c3", Acc) end
                    ]),
    Cen1 = cen_map("cen1", ["c1", "c2"], bus),
    Cen2 = cen_map("cen2", ["c3", "c2"], bus),
    Cont1 = cont_map("c1", ["cen1"], [0]),
    Cont2 = cont_map("c2", ["cen1", "cen2"], [0, 1]),
    Cont3 = cont_map("c3", ["cen2"], [0]),
    Wire1 = [
             endpoint("c1", "c1.0i", in, "cen1"),
             endpoint("cen1", "c1.0o", out)
            ],
    Wire2 = [
             endpoint("c2", "c2.0i", in, "cen1"),
             endpoint("cen1", "c2.0o", out)
            ],
    Wire3 = [
             endpoint("c2", "c2.1i", in, "cen2"),
             endpoint("cen2", "c2.1o", out)
            ],
    Wire4 = [
             endpoint("c3", "c3.0i", in, "cen2"),
             endpoint("cen2", "c3.0o", out)
            ],
    Delta = leviathan_cen:lm_compare(LM0, LM1),

    %% WHEN
    ok = leviathan_cen_store:update_cens(<<"host">>, Delta),

    %% THEN
    ActualLM = leviathan_cen_store:get_levmap(["cen1", "cen2"]),
    assert_equal_levmaps(compose_lm([Cen1, Cen2],
                                    [Cont1, Cont2, Cont3],
                                    [Wire1, Wire2, Wire3, Wire4]),
                         ActualLM).

%% -------------------------------------------------------------------------------
%% assertion helpers
%% -------------------------------------------------------------------------------

assert_equal_instructions(Instructions, AInstructions) ->
    Fn = fun({{_, Item, EValue}, {_, Item, AValue}})
               when is_list(EValue) andalso is_list(AValue) ->
                 ?assertEqualLists(EValue, AValue);
            ({{_, Item, EValue}, {_, Item, AValue}}) ->
                 ?assertEqual(EValue, AValue)
         end,
    lists:foreach(Fn, lists:zip(lists:sort(Instructions),
                                lists:sort(AInstructions))).

assert_equal_levmaps(ELM, ALM) ->
    {ECens, EConts, EWires} = decompose_lm(ELM),
    {ACens, AConts, AWires} = decompose_lm(ALM),
    assert_censmap_equal(ECens, ACens),
    assert_contsmap_equal(EConts, AConts),
    assert_wiresmap_equal(EWires, AWires).

assert_censmap_equal(ECens, ACens) ->
    lists:foreach(fun({ECen, ACen}) ->
                          assert_maps_with_lists_equal([contIDs],
                                                       ECen,
                                                       ACen)
                  end, lists:zip(lists:sort(ECens), lists:sort(ACens))).

assert_contsmap_equal(EConts, AConts) ->
    lists:foreach(fun({ECont, ACont}) ->
                          assert_maps_with_lists_equal([cens, reservedIdNums],
                                                       ECont,
                                                       ACont)
                  end, lists:zip(lists:sort(EConts), lists:sort(AConts))).

assert_wiresmap_equal(EWires, AWires) ->
    ?assertEqualLists(EWires, AWires).

assert_maps_with_lists_equal(KeysForLists, ECens, ACens) ->
    ?assertEqual(maps:without(KeysForLists, ECens),
                 maps:without(KeysForLists, ACens)),
    [?assertEqualLists(maps:get(K, ECens), maps:get(K, ACens))
     || K <- KeysForLists].

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

json_cen(CenId, ContIds) ->
    #{<<"cenID">> => CenId, <<"containerIDs">> => ContIds}.

cen_map(CenId, ContIds, WireType) ->
    #{cenID => CenId,
      wire_type => WireType,
      contIDs => ContIds}.

cont_map(ContId, CenIds, ReservedIds) ->
    #{contID => ContId,
      cens => CenIds,
      reservedIdNums => ReservedIds}.

endpoint(ContId, EndId, Side, Alias) ->
    #{endID => EndId,
      side => Side,
      dest => #{alias => Alias,
                id => ContId,
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

foldcalls(Starting, Fns) ->
    lists:foldl(fun(Fn, Acc) -> Fn(Acc) end, Starting, Fns).

tr() ->
    dbg:start(),
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tpl(leviathan_cen, []).

troff() ->
    dbg:ctpl(leviathan_cen).
