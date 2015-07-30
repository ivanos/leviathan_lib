-module(leviathan_bash).

-compile(export_all).

%
% mkdir -p /host/var/run/netns
%
mkdir_p_var_run_netns()->
    "mkdir -p /host/var/run/netns".

%
%ln -s /proc/$pid/ns/net /var/run/netns/$pid
%
ln_s_proc_ns_net_var_run_netns(Cpid)->
    "ln -s /proc/$" ++ Cpid ++ "/ns/net /var/run/netns/$" ++ Cpid.
