-module(leviathan_switch).

-export(
   [import_binary/1,
    import_json/1,
    run_switch/2]).

-include("leviathan_logger.hrl").

import_binary(Binary) ->
    SwitchMap = jiffy:decode(Binary, [return_maps]),
    import_json(SwitchMap).

import_json(Switches) when is_list(Switches) ->
    lists:foreach(fun import_json/1, Switches);
import_json(#{<<"type">> := CTypeBin,
              <<"interfaces">> := InterfacesBin} = Switch) ->
    CType = binary_to_list(CTypeBin),
    Interfaces = lists:map(fun binary_to_list/1, InterfacesBin),
    {ok, ContainerId, DatapathId} = run_switch(CType, Interfaces),
    NewSwitch = Switch#{<<"contID">> => ContainerId,
                        <<"datapath_id">> => DatapathId},
    leviathan_dby:import_switch(<<"host1">>, NewSwitch).

run_switch(CType, Interfaces) ->
    AlreadyConnected = weave_ofsh:all_connected(),

    CmdBundle = [leviathan_docker:run(CType, "--net=host --privileged=true", string:join(Interfaces, " "))],
    [ContainerId] = leviathan_linux:eval(CmdBundle, output),
    io:format("switch results:~n~p~n", [ContainerId]),

    %% XXX: Here we wait for a new incoming switch connection, and
    %% assume that it's coming from the newly started container.  This
    %% is a potential race condition.
    DatapathId = wait_for_new_connection(AlreadyConnected),

    {ok, ContainerId, DatapathId}.

wait_for_new_connection(AlreadyConnected) ->
    wait_for_new_connection(AlreadyConnected, 100).

wait_for_new_connection(_, 0) ->
    error(switch_connection_timeout);
wait_for_new_connection(AlreadyConnected, N) when N > 0 ->
    case weave_ofsh:all_connected() -- AlreadyConnected of
        [] ->
            timer:sleep(10),
            wait_for_new_connection(AlreadyConnected, N - 1);
        [DatapathId] ->
            list_to_binary(DatapathId)
    end.
