-module(leviathan_linux).

-compile(export_all).


set_netns(Cid)->
    CmdBundle = 
	[leviathan_bash:mkdir_p_var_run_netns(),
	 leviathan_ln_s_proc_ns_net_var_run_netns(leviathan_docker:inspect_pid(Cid))].
    
