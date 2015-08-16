-module(leviathan_cpool).

-compile(export_all).

-include("leviathan_logger.hrl").

%% some data from run_containers(...)
%%
%["bd17dea31b0ae32b3c3bf1f98888ec330d2bf154c705154e3cd788f56b946d4b",
% "e523c61a5348646660051bcb3cd5c9c4854bc21a14e238a69e4f61a3284ffb33",
% "33ae01a51fefa8bbfe7fd9d7fc15adefdcf4db1b682f754e55146d5d37b8f2cf"]
%["4211e3ae069d7cc6b0990ee65dbe8d14ae424a55ed3b4a7590e820003b9c56f3",
% "ea26d835c67e92f938d67019bf4404c9d528de18281dcb0eb90d1bc8d1d9a704",
% "dc9ff7d787c679df962000ffb7fc335dd790e5a95eeba93747a4656227dfb5cf"]

import_file(Filename)->
    {ok, Binary} = file:read_file(Filename),
    CPoolsMap = jiffy:decode(Binary, [return_maps]),
    io:format("CPools = ~p~n",[CPoolsMap]),
    #{<<"cpoolList">> := CPoolList} = CPoolsMap,
    NewCinDicts  = lists:foldl(fun(CPool,CinDict)->
				       #{<<"cins">> := CinsBin,
					 <<"start_with">> := StartWithNum,
					 <<"type">> := CTypeBin
					} = CPool,
				       Containers = run_containers(binary:bin_to_list(CTypeBin),
								   StartWithNum),
				       add_conts2cins(binlist2list(CinsBin),Containers,CinDict)
			       end,dict:new(),CPoolList),
    io:format("NewCinDicts:~n~p~n",[NewCinDicts]).

binlist2list(BinList)->
    lists:map(fun(Elem)->
		      binary:bin_to_list(Elem) end,BinList).

add_conts2cins(CinList,Containers,CinDict)->
    lists:foldl(fun(Cin,Acc)->
			dict:append_list(Cin,Containers,Acc) end,CinDict,CinList).

run_containers(CType,Num) when Num > 0 ->
    run_containers(CType,Num,[]).

run_containers(_,0,Acc)->
    io:format("container reulsts:~n~p~n",[Acc]),
    Acc;
run_containers(CType,Num,Acc) when Num > 0  ->
    CmdBundle = [leviathan_docker:run(CType,"--net=none","/bin/bash")],
    Result = leviathan_linux:eval(CmdBundle,output),
    run_containers(CType,Num-1,Acc ++ Result).

    



   
