-module(leviathan_cin_eqc).

-export([prop_cin_is_built/0,
         prop_cin_is_stored/0]).

-compile([export_all]).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(HOST, <<"host1">>).
-define(assertEqualLists(A,B), ?assertEqual(lists:sort(A), lists:sort(B))).

%% -------------------------------------------------------------------------------
%% Generators
%% -------------------------------------------------------------------------------

gen_cins_cens_wires_and_cins_subset() ->
    ?LET({CinsWithCens, CenMaps, Wires}, gen_cins_cens_and_wires(),
         {CinsWithCens, CenMaps, Wires, sublist(maps:keys(CinsWithCens))}).

gen_cins_cens_and_wires() ->
    ?LET(CinIds, gen_cin_ids(), gen_cins_cens_and_wires(CinIds)).

gen_cins_cens_and_wires(CinIds) ->
    ?LET(CenIds, gen_cen_ids(CinIds), gen_cins_cens_and_wires(CinIds, CenIds)).

gen_cins_cens_and_wires(CinIds, CenIds) ->
    ?LET(CenMaps, gen_cens(CenIds), {gen_cins(CinIds, CenIds),
                                     CenMaps,
                                     gen_wires(CenMaps)}).

gen_cin_ids() ->
    unique_and_shuffeled(gen_cin_id()).

gen_cen_ids(CinIds) ->
    shuffle(["cen" ++ integer_to_list(N) || N <- lists:seq(1, length(CinIds))]).

gen_cins(CinIds, CenIds0) ->
    CenIds1 = lists:map(fun(Id) -> [Id] end, CenIds0),
    maps:from_list(lists:zip(CinIds, CenIds1)).

gen_cens(CenIds) ->
    maps:from_list([{Id, gen_cen(Id)} || Id <- CenIds]).

gen_cen(CenId) ->
    maps:from_list([{cenID, CenId},
                    {contIDs, gen_cont_ids(CenId)},
                    {wire_type, gen_wire_type()}]).

gen_cont_ids(Base) ->
    ?LET(I,
         choose(1, 10),
         [{gen_host_id(), gen_cont_id(Base, N)} || N <- lists:seq(1, I)]).

gen_wires(CenMaps) ->
    maps:from_list([{Id, gen_cen_wires(maps:get(Id, CenMaps))}
                    || Id <- maps:keys(CenMaps)]).

gen_cen_wires(#{cenID := CenId, contIDs := ContIds}) ->
    [gen_wire(CenId, ContId) || ContId <- ContIds].

gen_wire(CenId, ContId) ->
    [#{endID => gen_endpoint_id(ContId, in),
       side => in,
       dest => #{type => cont,
                 alias => gen_cont_interface_id(CenId),
                 id => ContId}},
     #{endID => gen_endpoint_id(ContId, out),
       side => out,
       dest => #{type => cen,
                 id => CenId}}].

gen_cont_interface_id(CenId) ->
    CenId.

gen_endpoint_id({_, BareContId}, in) ->
    lists:flatten([BareContId, $., "0i"]);
gen_endpoint_id({_, BareContId}, out) ->
    lists:flatten([BareContId, $., "0i"]).

gen_wire_type() ->
    bus.

gen_host_id() ->
    "host1".

gen_cin_id() ->
    ?LET(I, choose(1, 1000), "cin" ++ integer_to_list(I)).

gen_cont_id(Base, N) ->
    Base ++ "-cont" ++ integer_to_list(N).

unique_and_shuffeled(Gen) ->
    ?LET(I, list(Gen), shuffle(lists:usort(I))).

        
%% -----------------------------------------------------------------------------
%% Properties
%% -----------------------------------------------------------------------------

prop_cin_is_built() ->
    ?SETUP(
       mkfn_qc_setup(),
       ?FORALL({CinsWithCens, CenMaps, Wires} = Generated,
               gen_cins_cens_and_wires(),
               begin
                   %% GIVEN
                   per_property_setup(CenMaps, Wires),

                   %% WHEN
                   CinLM = leviathan_cin:build_cins(CinsWithCens),

                   %% THEN
                   collect(maps:size(CinsWithCens),
                           cin_lm_correct(CinLM, Generated))
               end)).

prop_cin_is_stored() ->
    ?SETUP(
       mkfn_qc_setup(),
       ?FORALL({CinsWithCens, CenMaps, Wires, CinsSubset},
               gen_cins_cens_wires_and_cins_subset(),
               begin
                   %% GIVEN
                   per_property_setup(CenMaps, Wires),
                   CinLM = leviathan_cin:build_cins(CinsWithCens),

                   %% WHEN
                   ok = leviathan_cin_store:import_cins(?HOST, CinLM),

                   %% THEN
                   ECinLM = filter_cin_lm(CinLM, CinsSubset),
                   ACinLM = leviathan_cin_store:get_levmap(CinsSubset),
                   collect({maps:size(CinsWithCens), length(CinsSubset)},
                           cin_lms_equal(ECinLM, ACinLM))
               end)).

prop_cin_is_published() ->
    ?SETUP(
       mkfn_qc_setup(),
       ?FORALL({CinsWithCens, CenMaps, Wires, CinsSubset},
               gen_cins_cens_wires_and_cins_subset(),
               begin
                   %% GIVEN
                   per_property_setup(CenMaps, Wires),
                   CinLM0 = leviathan_cin:build_cins(CinsWithCens),
                   CinLM1 = filter_cin_lm(CinLM0, CinsSubset),
                   ok = leviathan_cin_store:import_cins(?HOST, CinLM0),

                   %% WHEN
                   leviathan_cin:prepare(CinsSubset),

                   %% THEN
                   network_configured_correctly(CinLM1)
               end)).

%% -----------------------------------------------------------------------------
%% Properties Helpers: top level CIN LM
%% -----------------------------------------------------------------------------

cin_lm_correct(#{cins := CinMaps, conts := ContMaps}, Generated) ->
    cin_maps_correct(CinMaps, Generated) andalso
        cont_maps_correct({CinMaps, ContMaps}, Generated).

filter_cin_lm(#{cins := Cins, conts := Conts}, CinsToKeep) ->
    FilterFn = fun(#{cinID := Id}) ->
                       lists:member(Id, CinsToKeep)
               end,
    #{cins => lists:filter(FilterFn, Cins),
      conts => lists:filter(FilterFn, Conts)}.

cin_lms_equal(#{cins := ECins, conts := EConts} = _ExpectedLM,
              #{cins := ACins, conts := AConts} = _ActualLM) ->
    CinsFn = mkfn_assert_equal_maps_with_lists([contIDs]),
    ContsFn = mkfn_assert_equal_maps_with_lists([]),
    ok == assert_equal_lists(ECins, ACins, CinsFn) andalso
        ok == assert_equal_lists(EConts, AConts, ContsFn).

network_configured_correctly(#{cins := CinMaps, conts := ContMaps}) ->
    ?assertEqual(length(CinMaps),
                 meck:num_calls(leviathan_linux, set_bus_ip, '_')),
    ?assertEqual(length(ContMaps),
                 meck:num_calls(leviathan_linux, set_ip_address , '_')),
    true.

%% -----------------------------------------------------------------------------
%% Properties Helpers: cin maps of CIN LM
%% -----------------------------------------------------------------------------

cin_maps_correct(CinMaps, {CinsWithCens, _, _} = Generated) ->
    length(CinMaps) == maps:size(CinsWithCens) andalso
        lists:all(mkfn_cin_map_correct(Generated), CinMaps).

mkfn_cin_map_correct({CinsWithCens, CenMaps, _}) ->
    fun(#{cinID := CinId,
          contIDs := ContIds,
          ip_b := IpB,
          addressing := Addressing}) ->
            CenIdsInCin = maps:get(CinId, CinsWithCens),
            ?assertEqualLists(gather_cens_conts(CenIdsInCin, CenMaps),
                              ContIds),
            lists:foreach(
              mkfn_cin_addressing_correct({IpB, Addressing}, CenMaps),
              CenIdsInCin),
            true
    end.

gather_cens_conts(CenIds, CenMaps) ->
    lists:foldl(fun(CenId, Acc) ->
                        CenMap = maps:get(CenId, CenMaps),
                        Acc ++ maps:get(contIDs, CenMap)
                end, [], CenIds).

%% @private Assert the bridge interface for a CIN is set up correctly.
mkfn_cin_addressing_correct({IpB, Addressing}, CenMaps) ->
    fun(CenId) ->
            #{cenID := EBridgeInterface} = maps:get(CenId, CenMaps),
            #{interface := ABridgeInterface,
              ip := Ip} = maps:get(CenId, Addressing),
            ?assertEqual(EBridgeInterface, ABridgeInterface),
            ?assertMatch({ok, {_, IpB, _, _}}, inet_parse:address(Ip))
    end.

%% -----------------------------------------------------------------------------
%% Properties Helpers: cont maps of CIN LM
%% -----------------------------------------------------------------------------

cont_maps_correct({CinMaps, ContMaps}, Generated = {_, CenMaps, _}) ->
    length(ContMaps) == count_conts_in_cens(CenMaps)
        andalso lists:all(mkfn_cont_map_correct(CinMaps, Generated),
                          ContMaps).

count_conts_in_cens(CenMaps) ->
    maps:fold(fun(_CenId, #{contIDs := Conts}, Acc) ->
                      Acc + length(Conts)
              end, 0, CenMaps).


mkfn_cont_map_correct(CinMaps, {CinsWithCens, CenMaps, Wires}) ->
    fun(#{contID := ContId,
          cinID := CinId,
          addressing := Addressing}) ->
            IpB = get_cin_ip_b(CinId, CinMaps),
            CenIdsInCin = maps:get(CinId, CinsWithCens),
            lists:any(mkfn_cont_in_cen_conts(ContId, CenMaps),
                      CenIdsInCin),
            lists:foreach(
              mkfn_cont_addressing_correct({ContId, IpB, Addressing},
                                           Wires),
              CenIdsInCin),
            true
    end.

get_cin_ip_b(CinId, CinMaps) ->
    [#{ip_b := IpB}] = [M || M <- CinMaps,
                             maps:get(cinID, M) =:= CinId],
    IpB.
    

mkfn_cont_in_cen_conts(ContId, CenMaps) ->
    fun(CenId) ->
            ContIds = maps:get(contIDs, maps:get(CenId, CenMaps)),
            lists:member(ContId, ContIds)
    end.

%% @private Assert the container iterface is set up correctly.
mkfn_cont_addressing_correct({ContId, IpB, Addressing}, Wires) ->
    fun(CenId) ->
            [
             #{endID := EEndId, side := in,
               dest := #{alias := EContInterface}},
             #{side := out}
            ] = find_cont_wire(ContId, maps:get(CenId, Wires)),
            #{endID := AEndId,
              interface := AContInterface,
              ip := Ip} = maps:get(CenId, Addressing),
            ?assertEqual(EEndId, AEndId),
            ?assertEqual(EContInterface, AContInterface),
            ?assertMatch({ok, {_, IpB, _, _}}, inet_parse:address(Ip))
    end.

find_cont_wire(ContId, Wires) ->
    Fn = fun([#{side := in, dest := #{id := Id}},
              #{side := out}]) when Id =:= ContId ->
                 false;
            (_) ->
                 true
         end,
    hd(lists:dropwhile(Fn, Wires)).

%% -----------------------------------------------------------------------------
%% Properties Helpers: mocking
%% -----------------------------------------------------------------------------

mock(dby) ->
    ok = meck:new(dby),
    ok = meck:expect(dby, install, 1, {module, ok});
mock(leviathan_linux) ->
    ok = meck:new(leviathan_linux, [passthrough]),
    ok = meck:expect(leviathan_linux, eval, 1, ok);
mock(leviathan_dby) ->
    ok = meck:new(leviathan_dby, [non_strict]),
    ok = meck:expect(leviathan_dby, set_cin_status, 2, ok);
mock(leviathan_docker) ->
    ok = meck:new(leviathan_docker, [non_strict]),
    ok = meck:expect(leviathan_docker, inspect_pid, fun(ContId) ->
                                                            ContId
                                                    end).

expect_dobby_get_cen(CenMaps) ->
    ok = meck:expect(leviathan_dby, get_cen,
                             fun(CenId) ->
                                     maps:get(CenId, CenMaps)
                             end).

expect_dobby_get_wires(Wires) ->
    ok = meck:expect(leviathan_dby, get_wires,
                     fun(CenId) ->
                             maps:get(CenId, Wires)
                     end).

%% -----------------------------------------------------------------------------
%% Assertion Helpers
%% -----------------------------------------------------------------------------

assert_equal_lists(EList, AList, AssertFun) ->
    lists:foreach(fun({Expected, Actual}) ->
                          AssertFun(Expected, Actual)
                  end, lists:zip(lists:sort(EList), lists:sort(AList))).

mkfn_assert_equal_maps_with_lists(KeysForLists) ->
    fun(Expected, Actual) ->
            assert_equal_maps_with_lists(KeysForLists, Expected, Actual)
    end.

assert_equal_maps_with_lists(KeysForLists, Expected, Actual) ->
    ?assertEqual(maps:without(KeysForLists, Expected),
                 maps:without(KeysForLists, Actual)),
    [?assertEqualLists(maps:get(K, Expected), maps:get(K, Actual))
     || K <- KeysForLists].

%% -----------------------------------------------------------------------------
%% Setup Helpers
%% -----------------------------------------------------------------------------

per_property_setup(CenMaps, Wires) ->
    ok = leviathan_db:clear(),
    expect_dobby_get_cen(CenMaps),
    expect_dobby_get_wires(Wires),
    ok = meck:reset(leviathan_linux).

mkfn_qc_setup() ->
    fun() ->
            {MocksToUnload, AppsToStop} = setup(),
            fun() -> teardown(MocksToUnload, AppsToStop) end
    end.

setup() ->
    Mods = [dby, leviathan_dby, leviathan_linux, leviathan_docker],
    [mock(M) || M <- Mods],
    {ok, Apps} = application:ensure_all_started(erl_mnesia),
    leviathan_mnesia:start(),
    {Mods, Apps}.

teardown(Mods, Apps) ->
    lists:foreach(fun application:stop/1, Apps),
    [meck:unload(M) || M <- Mods].
