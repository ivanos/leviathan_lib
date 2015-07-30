-module(leviathan_linux).

-compile(export_all).


set_netns(Cid)->
    [leviathan_bash:mkdir_p_var_run_netns(),
     leviathan_bash:ln_s_proc_ns_net_var_run_netns(leviathan_docker:inspect_pid(Cid))].

remove_netns(Cid)->
    [leviathan_bash:rm_f_var_run_netns(leviathan_docker:inspect_pid(Cid))].


new_peer(LevNumA,LevNumB)->
    LevNameA = mk_peer_lev_name(LevNumA),
    LevNameB = mk_peer_lev_name(LevNumB),
    [leviathan_ip:link_add_type_veth_peer_name(LevNameA,LevNameB)].


mk_peer_lev_name(LevNum)->
    "lev" ++ integer_to_list(LevNum).


eval(CmdBundle)->
    lists:map(fun(X)->		   
		      os:cmd(X) end,CmdBundle).
    
    
