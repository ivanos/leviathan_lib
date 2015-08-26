-module(leviathan_cen_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").

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

prop_wires() ->
    numtests(1000,
        ?SETUP(
            fun() ->
                start_dobby(),
                fun() -> stop_dobby() end
            end,
        ?FORALL(
            Instructions,
            gen_instructions(),
            begin
                cleanup(),

                % make LM
                LM = new_lm(Instructions),
                {Cens, _, _} = decompose_lm(LM),

                % check wires
                collect(length(Cens), check_lm(LM))
            end
        ))).

check_lm(LM) ->
    {Cens, Conts, Wires} = decompose_lm(LM),
    leviathan_test_utils:check_wires(Cens, Wires),
    check_cens_conts(Cens, Conts),
    check_wire_count(Cens, Wires),
    true.

check_cens_conts(Cens, Conts) ->
    CensContsFromCens = cens_conts_from_cens(Cens),
    CensContsFromConts = cens_conts_from_conts(Conts),
    ?assertEqual(CensContsFromCens, CensContsFromConts).

cens_conts_from_cens(Cens) ->
    lists:sort(lists:flatten(lists:map(
        fun(#{cenID := CenId, contIDs := ContIds}) ->
            [{CenId, ContId} || ContId <- ContIds]
        end, Cens))).

cens_conts_from_conts(Conts) ->
    lists:sort(lists:flatten(lists:map(
        fun(#{contID := ContId, cens := CenIds}) ->
            [{CenId, ContId} || CenId <- CenIds]
        end, Conts))).

check_wire_count(Cens, Wires) ->
    ExpectedWireCount = expected_wire_count(Cens),
    ?assertEqual(ExpectedWireCount, length(Wires)).

expected_wire_count(Cens) ->
    lists:foldl(
        fun(#{contIDs := ContIds}, Count) when length(ContIds) < 2 ->
            Count;
           (#{contIDs := ContIds}, Count) when length(ContIds) == 2 ->
            % when wire enabled, this is +1
            Count + 2;
           (#{contIDs := ContIds}, Count) ->
            Count + length(ContIds)
        end, 0, Cens).

prop_lm_dby() ->
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
    lager:set_loglevel(lager_console_backend, warning),
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

decompose_lm(#{censmap := #{cens := Cens},
               contsmap := #{conts := Conts},
               wiremap := #{wires := Wires}}) ->
    {Cens, Conts, Wires}.

check_wires(Cens, Wires) ->
    % map CEN to ipaddr
    IpAddrByCen = map_cen_to_ipaddr(Cens),
    
    % list of CEN endpionts
    CenEndpointIpAddrs = cen_endpoints_from_wires(Wires),

    % validate
    cen_ipaddrs_in_cen(CenEndpointIpAddrs, IpAddrByCen).

map_cen_to_ipaddr(Cens) ->
    lists:foldl(
        fun(#{cenID := CenId, ipaddr := IpAddr}, Acc) ->
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
    case {CenA, CenB} == {ContA, ContB} of
        false ->
            io:format("bad ip cen(~s), containter(~s)~n",
                                                    [CenIpaddr, IpAddr]),
            false;
        true ->
            cen_ipaddrs_in_cen(Rest, IpAddrByCen)
    end.
