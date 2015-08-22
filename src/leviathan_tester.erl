-module(leviathan_tester).

-compile(export_all).


gen_ip_events()->
    spawn(fun ip_event_generator/0).


ip_event_generator()->
    CmdBundle = [leviathan_ip:link_add_type_veth_peer_name("test0.0","test0.1"),
		 leviathan_ip:link_set_dev_name("test0.1","talias0.1"),
		 leviathan_ip:link_set_up("talias0.1"),
		 leviathan_ip:addr_add_dev("10.3.3.2/16","talias0.1")],
    leviathan_linux:eval(CmdBundle),
    timer:sleep(5000),
    CmdBundle2 = [leviathan_ip:link_delete_type_veth_peer("test0.0")],
    leviathan_linux:eval(CmdBundle2),
    timer:sleep(5000),
    ip_event_generator().
