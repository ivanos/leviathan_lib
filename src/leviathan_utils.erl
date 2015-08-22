-module(leviathan_utils).

-include("leviathan_logger.hrl").

-export([connect_to_dobby/0,container_map/1]).

-compile(export_all).

-define(DEFAULT_DOBBY_NODE, 'dobby@127.0.0.1').

%%%===================================================================
%%% API
%%%===================================================================

connect_to_dobby() ->
    DobbyNode = application:get_env(leviathan_lib, dobby_node,
                                    ?DEFAULT_DOBBY_NODE),
    case net_adm:ping(DobbyNode) of
        pong ->
            ?INFO("Connected to dobby node: ~p", [DobbyNode]);
        pang ->
            ?ERROR("Failed to connect to dobby node: ~p", [DobbyNode]),
            throw({connecting_to_dobby_failed, DobbyNode})
    end.


container_get_cins(ContainerID)->
    Map = container_map(ContainerID),
    ?DEBUG("Container Map:~n~p",[Map]),
    Env = container_get_env(Map),
    case lists:filter(fun is_lev_cin/1,Env) of
	[LevCin] -> case container_get_network_mode(Map) of
			<<"none">> -> get_cins_from_env_bin(LevCin);
			_ ->
			    ?INFO("Invalid Leviathan Setting: LEV_CIN set but \"--net=none\" not set"),
			    nomatch
		    end;
	_ -> nomatch
    end.

get_cins_from_env_bin(LevEnvBin)->
    <<"LEV_CIN=",CinsBin/binary>> = LevEnvBin,
    binary:split(CinsBin,<<",">>,[global]).

is_lev_cin(Binary)->
    case binary:match(Binary,<<"LEV_CIN=">>) of
	nomatch ->
	    false;
	_ -> true
    end.
    

container_get_env(ContainerMap)->
    [#{ <<"Config">> :=  #{ <<"Env">> := Environment }}] = ContainerMap,
    Environment.

container_get_network_mode(ContainerMap)->
    [#{ <<"HostConfig">> :=  #{ <<"NetworkMode">> := NetworkMode }}] = ContainerMap,
    NetworkMode.


container_map(ContainerID)->
    CmdBundle = [leviathan_docker:inspect(ContainerID)],
    Result = leviathan_linux:eval(CmdBundle,output_raw),
    jiffy:decode(Result,[return_maps]).
