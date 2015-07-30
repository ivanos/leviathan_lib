-module(leviathan_linux).

-compile(export_all).


set_netns(Cid)->
    [leviathan_bash:mkdir_p_var_run_netns(),
     leviathan_bash:ln_s_proc_ns_net_var_run_netns(leviathan_docker:inspect_pid(Cid))].

remove_netns(Cid)->
    [leviathan_bash:rm_f_var_run_netns(leviathan_docker:inspect_pid(Cid))].


new_peer(Cid)->
    LevNameOut = mk_peer_lev_name_out(Cid),
    LevNameIn = mk_peer_lev_name_in(Cid),
    [leviathan_ip:link_add_type_veth_peer_name(LevNameOut,LevNameIn)].


mk_peer_lev_name_in(Cid)->
    mk_peer_lev_name_prefix(Cid) ++ ".in".

mk_peer_lev_name_out(Cid)->
    mk_peer_lev_name_prefix(Cid) ++ ".out".

mk_peer_lev_name_prefix(Cid)->
    "levPeer." ++ Cid.


eval(CmdBundle)->
    lists:map(fun(X)->		   
		      os:cmd(X) end,CmdBundle).
    
    
