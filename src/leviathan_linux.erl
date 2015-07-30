-module(leviathan_linux).

-compile(export_all).


set_netns(Cid)->
	[leviathan_bash:mkdir_p_var_run_netns(),
	 leviathan_bash:ln_s_proc_ns_net_var_run_netns(leviathan_docker:inspect_pid(Cid))].

remove_netns(Cid)->
	[leviathan_bash:rm_f_var_run_netns(leviathan_docker:inspect_pid(Cid))].

    
