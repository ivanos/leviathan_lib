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
    {ok, ContainerId} = run_switch(CType, Interfaces),
    leviathan_dby:import_switch(<<"host1">>, Switch#{<<"contID">> => ContainerId}).

run_switch(CType, Interfaces) ->
    CmdBundle = [leviathan_docker:run(CType, "--net=host --privileged=true", string:join(Interfaces, " "))],
    [ContainerId] = leviathan_linux:eval(CmdBundle, output),
    io:format("switch results:~n~p~n", [ContainerId]),
    {ok, ContainerId}.
