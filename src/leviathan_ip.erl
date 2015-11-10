-module(leviathan_ip).

-compile(export_all).



%% All parameters must be strings

link_add_type_veth_peer_name(DevNameA,DevNameB)->
    "ip link add " ++  DevNameA ++ " type veth peer name " ++ DevNameB.

link_delete_type_veth_peer(DevName)->
    "ip link delete " ++  DevName ++ " type veth peer".

link_set_dev_name(OrigDevName,NewDevName)->
    "ip link set dev " ++ OrigDevName ++ " name " ++ NewDevName.
    
link_set_up(DevName)->
    "ip link set " ++ DevName ++ " up".

link_set_netns(DevName,CPid)->
    "ip link set " ++ DevName ++ " netns " ++ CPid.

addr_add_dev(Address,DevName)->
    "ip addr add " ++ Address ++ " dev " ++ DevName.

netns_exec_ip_link_set_dev_name(CPid,OrigDevName,NewDevName)->
    "ip netns exec " ++ CPid ++ " ip link set dev " ++ OrigDevName ++ " name " ++ NewDevName.

netns_exec_ip_link_set_address(CPid,DevName,Address)->
    "ip netns exec " ++ CPid ++ " ip link set " ++ DevName ++ " address " ++ Address.

netns_exec_ip_link_set_up(CPid,DevName)->
    "ip netns exec " ++ CPid ++ " ip link set " ++ DevName ++ " up".


netns_exec_ip_link_delete_type_veth_peer(CPid,DevName)->
    "ip netns exec " ++ CPid ++ " ip link delete " ++ DevName ++ " type veth peer".


%
% Example "ip netns exec $pid ip addr add 172.17.42.99/16 dev eth0"
%
netns_exec_ip_addr_add_dev(CPid,Address,DevName)->
    "ip netns exec " ++ CPid ++ " ip addr add " ++ Address ++ " dev " ++ DevName.

%
% Example: "ip netns exec $pid ip route add default via 172.17.42.1"
%
netns_exec_ip_route_add_default_via(CPid,Address)->
    "ip netns exec "  ++ CPid ++ " ip route add default via " ++ Address.
    
%
% Example: "ip netns exec $pid ip route add default dev cen2"
%
netns_exec_ip_route_add_default_dev(CPid,DevName)->
    "ip netns exec "  ++ CPid ++ " ip route add default dev " ++ DevName.


netns_exec_ip_route_add(CPid, NetIPAddress, DevName) ->
    "ip netns exec " ++ CPid ++ " ip route add " ++ NetIPAddress ++ " dev " ++ DevName.
