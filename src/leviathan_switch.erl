%% @doc
%% This module starts an OpenFlow switch and publishes the appropriate
%% information to Dobby.
%%
%% Use it like this:
%%
%% ```
%%     leviathan_switch:import_json(
%%       #{<<"type">> => <<"local/linc">>,
%%         <<"interfaces">> => [<<"cen1">>, <<"cen2">>]}).
%% '''
%%
%% The Docker image named in the `type' attribute will be started with
%% the options `--net=host --privileged=true', and the interface names
%% will be passed as command line arguments.  The Docker image should
%% have an `ENTRYPOINT' appropriately set to accept those arguments.
-module(leviathan_switch).

-export(
   [import_binary/1,
    import_json/1]).

-include("leviathan_logger.hrl").

%% @doc Start one or more switches based on a JSON description.
%%
%% This function parses `Binary' as JSON and passes it to {@link
%% import_json/1}.
import_binary(Binary) ->
    SwitchMap = jiffy:decode(Binary, [return_maps]),
    import_json(SwitchMap).

%% @doc Start one or more switches, and publish info to Dobby.
%%
%% The argument can be a single map, or a list of maps.  Each
%% map should have two keys:
%%
%% The `<<"type">>' key should have a binary value, that names a
%% Docker image.
%%
%% The `<<"interfaces">>' key should have a list of binaries,
%% each naming an interface name that the newly started switch
%% should manage.
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

%% @doc Start a switch, without publishing anything to Dobby.
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
    wait_for_new_connection(AlreadyConnected, 10000).

wait_for_new_connection(_, N) when N =< 0 ->
    error(switch_connection_timeout);
wait_for_new_connection(AlreadyConnected, N) when N > 0 ->
    case weave_ofsh:all_connected() -- AlreadyConnected of
        [] ->
            Sleep = 10,
            timer:sleep(Sleep),
            wait_for_new_connection(AlreadyConnected, N - Sleep);
        [DatapathId] ->
            list_to_binary(DatapathId)
    end.
