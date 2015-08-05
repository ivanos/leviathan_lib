-module(leviathan_docker_events).

-compile(export_all).

-include("leviathan_logger.hrl").


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
