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
new_bus(CenId,IPAddress)->
    [leviathan_brctl:addbr(CenId),
     leviathan_ip:addr_add_dev(IPAddress ++ "/16",CenId),
     leviathan_ifconfig:dev_up(CenId)].

delete_bus(CenId)->
    [leviathan_brctl:delbr(CenId)].

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

set_ip_address(Cid, Alias, IPAddress)->
    CPid = leviathan_docker:inspect_pid(Cid),
    {ok,{A,B,_,_}} = inet:parse_ipv4_address(IPAddress),
    Gateway = {A,B,0,1},
    GatewayString = inet:ntoa(Gateway),
    [leviathan_ip:netns_exec_ip_addr_add_dev(CPid,IPAddress ++ "/16",Alias), %% XXX hardcoded to /16
     leviathan_ip:netns_exec_ip_route_add_default_via(CPid,GatewayString)].

eval(CmdBundle)->
    EvalBundle = lists:map(fun(X)->Result = os:cmd(X), {X,Result} end,CmdBundle),
    Output = lists:map(fun({Cmd,Output})->io:format("   Cmd: ~p~nOutput: ~p~n",[Cmd,Output]),Acc++[Ouput] end,EvalBundle).

eval(CmdBundle,output)->
    EvalBundle = lists:map(fun(X)->Result = os:cmd(X), {X,Result} end,CmdBundle),
    Output = lists:foldl(fun({Cmd,Output},Acc)->io:format("   Cmd: ~p~nOutput: ~p~n",[Cmd,Output]),Acc++[Ouput] end,EvalBundle,[]),
    Output.
    
    
    
