-module(leviathan_switch).

-compile(export_all).

-include("leviathan_logger.hrl").

run_switch(CType, Interfaces) ->
    CmdBundle = [leviathan_docker:run(CType, "--net=host --privileged=true", string:join(Interfaces, " "))],
    Result = leviathan_linux:eval(CmdBundle, output),
    io:format("switch results:~n~p~n", [Result]),
    ok.
