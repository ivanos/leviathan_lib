-module(leviathan_linux).

-compile(export_all).

-include("leviathan_logger.hrl").

%
% Cid::string() is the runtime Container ID of a running Container as shown by
% > docker ps
% 
% <type>Num (e.g. PeerNum) are internal numbers allocated by Leviathan
%


%
% set_netns(Cid)
%
% creates a new network namespace with a name that is identical the process id
% (pid) of the running Container.  
%
%  
set_netns(Cid)->
    [leviathan_bash:mkdir_p_var_run_netns(),
     leviathan_bash:ln_s_proc_ns_net_var_run_netns(leviathan_docker:inspect_pid(Cid))].

remove_netns(Cid)->
    [leviathan_bash:rm_f_var_run_netns(leviathan_docker:inspect_pid(Cid))].

%
% new_peer(Cid,PeerNum)
%
% creates a new pair of "peer" virtual interfaces and stuffs one end
% (the "i" end) inside the container and renames it 
% Eth<PeerNum>  (e.g. Eth0)
%
% A high level description of what is going here can be found here:
% http://blog.scottlowe.org/2013/09/04/introducing-linux-network-namespaces/
% https://docs.docker.com/articles/networking/#how-docker-networks-a-container
% 
%
%  Bus operations:
%  By definition a bus as more than 2 containers connected 
%  
new_bus(CenId)->
    [leviathan_brctl:addbr(CenId),
     leviathan_ifconfig:dev_up(CenId)].

delete_bus(CenId)->
    [leviathan_ifconfig:dev_down(CenId),
     leviathan_brctl:delbr(CenId)].

set_bus_ip(BusId, Ip) ->
    [leviathan_ip:addr_add_dev(Ip ++ "/16", BusId)].

delete_bus_ip(BusId, Ip) ->
    [leviathan_ip:addr_del_dev(Ip ++ "/16", BusId)].

%
%
%
new_peer(EndId1,EndId2)->
    [leviathan_ip:link_add_type_veth_peer_name(EndId1,EndId2)].
%
%
peer2cont(Cid,EndId,Alias)->
    CPid = leviathan_docker:inspect_pid(Cid),
    [leviathan_ip:link_set_netns(EndId,CPid),
    leviathan_ip:netns_exec_ip_link_set_dev_name(CPid,EndId,Alias),
    leviathan_ip:netns_exec_ip_link_set_up(CPid,Alias)].

peer2cen(CenId,EndId)->
    [leviathan_brctl:addif(CenId,EndId),
     leviathan_ip:link_set_up(EndId)].


delete_peer(EndId)->
    [leviathan_ip:link_delete_type_veth_peer(EndId)].

delete_cont_interface(Cid,Alias)->
    CPid = leviathan_docker:inspect_pid(Cid),
    [leviathan_ip:netns_exec_ip_link_delete_type_veth_peer(CPid,Alias)].
    

new_bridge(BridgeNum)->
    [leviathan_brctl:addbr(BridgeNum)].

set_ip_address(Cid, Alias, IPAddress) ->
    CPid = leviathan_docker:inspect_pid(Cid),
    %% Use /0 as netmask, since the concept of "local network" doesn't
    %% make sense anymore.  Since the entire world is now our local
    %% network, we don't need a gateway either.
    {ok, {A, B, _, _}} = inet_parse:address(IPAddress),
    NetIPAddress = inet_parse:ntoa({A, B, 0, 0}),
    [leviathan_ip:netns_exec_ip_addr_add_dev(CPid,IPAddress ++ "/0",Alias),
     %% Add a route to send everything out through the network interface.
     leviathan_ip:netns_exec_ip_route_add(CPid, NetIPAddress ++ "/16", Alias),
     leviathan_ip:netns_exec_ip_route_add_default_dev(CPid,Alias)].

delete_ip_address(Cid, Alias, IPAddress) ->
    CPid = leviathan_docker:inspect_pid(Cid),
    {ok, {A, B, _, _}} = inet_parse:address(IPAddress),
    NetIPAddress = inet_parse:ntoa({A, B, 0, 0}),
    [leviathan_ip:netns_exec_ip_addr_del_dev(CPid,IPAddress ++ "/0",Alias),
     %% Add a route to send everything out through the network interface.
     leviathan_ip:netns_exec_ip_route_del(CPid, NetIPAddress ++ "/16", Alias),
     leviathan_ip:netns_exec_ip_route_del_default_dev(CPid,Alias)].

eval(CmdBundle)->
    EvalBundle = lists:map(fun(X)->Result = leviathan_os:cmd(X), {X,Result} end,CmdBundle),
    lists:map(fun({Cmd,Output})->io:format("   Cmd: ~p~nOutput: ~p~n",[Cmd,Output]) end,EvalBundle).

eval(CmdBundle,output)->
    EvalBundle = lists:map(fun(X)->Result = leviathan_os:cmd(X), {X,Result} end,CmdBundle),
    Results = lists:foldl(fun({Cmd,Output},Acc)->io:format("   Cmd: ~p~nOutput: ~p~n",[Cmd,Output]),Acc++[binary:list_to_bin(string:substr(string:strip(Output,right,$\n),1,12))] end,[],EvalBundle),
    Results;  
eval(CmdBundle,output_raw)->
    EvalBundle = lists:map(fun(X)->Result = leviathan_os:cmd(X), {X,Result} end,CmdBundle),
    Results = lists:foldl(fun({_Cmd,Output},Acc)->%io:format("   Cmd: ~p~nOutput: ~p~n",[Cmd,Output]),
				  Acc++[Output] end,[],EvalBundle),
    Results.
