-module(leviathan_docker_events).

-compile(export_all).

-include("leviathan_logger.hrl").


event_listener() ->
    DockerEventsBin = "/usr/bin/docker events --until=\"\"",
    Port = open_port({spawn, DockerEventsBin}, []),
    loop(Port).

loop(Port) ->
    receive
	{Port, {data, Data}} ->
	    io:format("events received: ~p~n",[Data]),
	    loop(Port);
	{'EXIT', Port, Reason} ->
	    exit({port_terminated,Reason})
    end.
		       
get_events(Time)->
    Cmd = "echo \"GET /events?since=" ++ integer_to_list(Time) ++ " HTTP/1.1\r\n\" | nc -U /var/run/docker.sock",
    Results = os:cmd(Cmd),
    Tokens = string:tokens(Results,"\r\n"),
    lists:foldl(fun(Str,Acc)->case string:substr(Str,1,1) of 
				  "{" -> Acc ++ Str;
				  _ -> Acc 
			      end end,[],Tokens).




%% helpers

now_secs({MegaSecs,Secs,_}) ->
    MegaSecs*1000000 + Secs.
