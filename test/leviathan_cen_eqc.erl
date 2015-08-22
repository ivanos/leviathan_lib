-module(leviathan_cen_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

-define(HOST, <<"host1">>).

make_id(Base, I) ->
    Base ++ integer_to_list(I).

pos_int() ->
    ?SUCHTHAT(I, int(), I >= 0).

gen_cen_id() ->
    ?LET(I, pos_int(), make_id("cen-", I)).

gen_cont_id() ->
    ?LET(I, pos_int(), make_id("cont-", I)).

gen_op() ->
    elements([add, destroy]).

gen_instructions() ->
    list({gen_op(), gen_cen_id(), gen_cont_id()}).

prop_lm() ->
    numtests(1000,
        ?SETUP(
            fun() ->
                start_dobby(),
                fun() -> stop_dobby() end
            end,
            ?FORALL(
                {Base, Delta},
                {gen_instructions(), gen_instructions()},
                begin
                    cleanup(),

                    % make base
                    BaseLM = new_lm(Base),

                    % make delta
                    NewLM = run_instructions(Delta, BaseLM),

                    % compute delta (may differ from instructions)
                    % because some instructions will be invalid
                    ComputedDelta = leviathan_cen:lm_compare(BaseLM, NewLM),

                    % insert base into dobby
                    ok = leviathan_dby:import_cens(?HOST, BaseLM),

                    % apply computed delta to dobby
                    ok = leviathan_dby:update_cens(?HOST, ComputedDelta),

                    % extract from dobby
                    CenIds = cenids_from_lm(NewLM),
                    DobbyLM = leviathan_cen:get_levmap(CenIds),

                    % compute delta between new dobby and new LM
                    % (should be no difference)
                    Difference = leviathan_cen:lm_compare(NewLM, DobbyLM),

                    collect(length(ComputedDelta),
                        equals([], Difference))
                end
            ))).

start_dobby() ->
    ok = application:set_env(erl_mnesia, options, [persistent]),
    application:ensure_all_started(dobby),
    mnesia:wait_for_tables([identifiers], 5000).

stop_dobby() ->
    application:stop(dobby).

cleanup() ->
    dby_db:clear().

% LM
new_lm() ->
    #{
        censmap => #{cens => []},
        contsmap => #{conts => []},
        wiremap => #{wires => []}
    }.

new_lm(Instructions) ->
    run_instructions(Instructions, new_lm()).

run_instructions([], LM) ->
    LM;
run_instructions([Op | Rest], LM) ->
    run_instructions(Rest, run_op(Op, LM)).

run_op({add, CenId, ContId}, LM) ->
    leviathan_cen:lm_add_container(CenId, ContId, LM);
run_op({destroy, CenId, ContId}, LM) ->
    leviathan_cen:lm_remove_container(CenId, ContId, LM).

cenids_from_lm(#{censmap := #{cens := Cens}}) ->
    [CenId || #{cenID := CenId} <- Cens].
