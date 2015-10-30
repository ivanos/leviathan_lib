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

gen_cins_cens_and_cins_subset() ->
    ?LET({CinsWithCens, CensWithConts}, gen_cins_and_cens(),
         {CinsWithCens, CensWithConts,  sublist(maps:keys(CinsWithCens))}).

gen_cins_and_cens() ->
    ?LET(CinIds, gen_cin_ids(), gen_cins_and_cens(CinIds)).

gen_cins_and_cens(CinIds) ->
    ?LET(CenIds, gen_cen_ids(CinIds), {gen_cins(CinIds, CenIds),
                                       gen_cens(CenIds)}).

gen_cin_ids() ->
    unique_and_shuffeled(gen_cin_id()).

gen_cen_ids(CinIds) ->
    shuffle(["cen" ++ integer_to_list(N) || N <- lists:seq(1, length(CinIds))]).

gen_cins(CinIds, CenIds0) ->
    CenIds1 = lists:map(fun(Id) -> [Id] end, CenIds0),
    maps:from_list(lists:zip(CinIds, CenIds1)).

gen_cens(CenIds) ->
    maps:from_list([{Id, gen_cont_ids(Id)} || Id <- CenIds]).

gen_cont_ids(Base) ->
    ?LET(I,
         choose(1, 10),
         [{gen_host_id(), gen_cont_id(Base, N)} || N <- lists:seq(1, I)]).

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
       ?FORALL({CinsWithCens, CensWithConts},
               gen_cins_and_cens(),
               begin
                   %% GIVEN
                   leviathan_mnesia:clear(),
                   mock_dobby_get_cen_conts(CensWithConts),

                   %% WHEN
                   CinLM = leviathan_cin2:build_cins(CinsWithCens),

                   %% THEN
                   collect(maps:size(CinsWithCens),
                           cin_lm_correct(CinLM, CinsWithCens, CensWithConts))
               end)).

prop_cin_is_stored() ->
    ?SETUP(
       mkfn_qc_setup(),
       ?FORALL({CinsWithCens, CensWithConts, CinsSubset},
               gen_cins_cens_and_cins_subset(),
               begin
                   %% GIVEN
                   leviathan_mnesia:clear(),
                   mock_dobby_get_cen_conts(CensWithConts),
                   CinLM = leviathan_cin2:build_cins(CinsWithCens),

                   %% WHEN
                   ok = leviathan_cin_store:import_cins(?HOST, CinLM),

                   %% THEN
                   ECinLM = filter_cin_lm(CinLM, CinsSubset),
                   ACinLM = leviathan_cin_store:get_levmap(CinsSubset),
                   collect({maps:size(CinsWithCens), length(CinsSubset)},
                           cin_lms_equal(ECinLM, ACinLM))
               end)).

%% -----------------------------------------------------------------------------
%% Properties Helpers
%% -----------------------------------------------------------------------------

cin_lm_correct(#{cins := CinMaps, conts := ContMaps}, CinsWithCens, CensWithConts) ->
    cin_maps_correct(CinMaps, CinsWithCens, CensWithConts) andalso
        cont_maps_correct(ContMaps, CinsWithCens, CensWithConts).

cin_maps_correct(CinMaps, CinsWithCens, CensWithConts) ->
    length(CinMaps) == maps:size(CinsWithCens) andalso
        lists:all(mkfn_cin_map_correct(CinsWithCens, CensWithConts),
                  CinMaps).

cont_maps_correct(ContMaps, CinsWithCens, CensWithConts) ->
    length(ContMaps) == maps:fold(fun(_CenId, Conts, ContsCnt) ->
                                          ContsCnt + length(Conts)
                                  end, 0, CensWithConts)
        andalso lists:all(mkfn_cont_map_correct(CinsWithCens, CensWithConts), ContMaps).

mkfn_cin_map_correct(CinsWithCens, CensWithConts) ->
    fun(#{cinID := CinId,
          cenIDs := [CenId],
          contIDs := ContIds,
          ip_b := IpB,
          ip := Ip}) ->
            ?assertEqual([CenId], maps:get(CinId, CinsWithCens)),
            ?assertEqualLists(ContIds, maps:get(CenId, CensWithConts)),
            ?assertMatch({ok, {_, IpB, _, _}}, inet_parse:address(Ip)),
            true
    end.

mkfn_cont_map_correct(CinsWithCens, CensWithConts) ->
    fun(#{contID := ContId,
          cinID := CinId,
          ip := Ip}) ->
            [CenId] = maps:get(CinId, CinsWithCens),
            ContIds = maps:get(CenId, CensWithConts),
            ?assert(lists:member(ContId, ContIds)),
            ?assertMatch({ok, _}, inet_parse:address(Ip)),
            true
    end.

mock_dobby_get_cen_conts(CensWithConts) ->
    ok = meck:expect(leviathan_dby, get_cen_conts,
                             fun(CenId) ->
                                     maps:get(CenId, CensWithConts)
                             end).

filter_cin_lm(#{cins := Cins, conts := Conts}, CinsToKeep) ->
    FilterFn = fun(#{cinID := Id}) ->
                       lists:member(Id, CinsToKeep)
               end,
    #{cins => lists:filter(FilterFn, Cins),
      conts => lists:filter(FilterFn, Conts)}.
    

cin_lms_equal(#{cins := ECins, conts := EConts} = _ExpectedLM,
              #{cins := ACins, conts := AConts} = _ActualLM) ->
    CinsFn = mkfn_assert_equal_maps_with_lists([cenIDs, contIDs]),
    ContsFn = mkfn_assert_equal_maps_with_lists([]),
    ok == assert_equal_lists(ECins, ACins, CinsFn) andalso
        ok == assert_equal_lists(EConts, AConts, ContsFn).


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

assert_equal_maps_with_lists(KeysForLists, ECens, ACens) ->
    ?assertEqual(maps:without(KeysForLists, ECens),
                 maps:without(KeysForLists, ACens)),
    [?assertEqualLists(maps:get(K, ECens), maps:get(K, ACens))
     || K <- KeysForLists].

%% -----------------------------------------------------------------------------
%% Setup Helpers
%% -----------------------------------------------------------------------------

mkfn_qc_setup() ->
    fun() ->
            {MocksToUnload, AppsToStop} = setup(),
            fun() -> teardown(MocksToUnload, AppsToStop) end
    end.

setup() ->
    ok = meck:new(dby),
    ok = meck:expect(dby, install, 1, {module, ok}),
    ok = meck:new(Mods = [leviathan_dby], [non_strict]),
    {ok, Apps} = application:ensure_all_started(erl_mnesia),
    leviathan_mnesia:start(),
    {[dby | Mods], Apps}.

teardown(Mods, Apps) ->
    lists:foreach(fun application:stop/1, Apps),
    [meck:unload(M) || M <- Mods].
    
