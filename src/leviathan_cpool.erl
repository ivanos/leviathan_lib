-module(leviathan_cpool).

-compile(export_all).

-include("leviathan_logger.hrl").

import_file(Filename)->
    {ok, Binary} = file:read_file(Filename),
    CPoolsMap = jiffy:decode(Binary, [return_maps]),
    io:format("CPools = ~p~n",[CPoolsMap]),
    #{<<"cpoolList">> := CPoolList} = CPoolsMap,
    lists:foreach(fun(CPool)->
			  #{<<"cins">> := CinsBin,
			    <<"start_with">> := StartWithNum,
			    <<"type">> := CTypeBin
			   } = CPool,
			  start_containers(binary:bin_to_list(CTypeBin),
					   StartWithNum)
		  end,CPoolList).


start_containers(CType,Num) when Num > 0 ->
    start_containers(CType,Num,[]).

start_containers(_,0,Acc)->
    io:format("p~n",Acc),
    Acc;
start_containers(CType,Num,Acc) when Num > 0  ->
    CmdBundle = [leviathan_docker:run(CType,"--net=none","/bin/bash")],
    Result = leviathan_linux:eval(CmdBundle),
    start_containers(CType,Num-1,Acc ++ [Result]).
    



   
