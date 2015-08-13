# leviathan_lib
Erlang code specific to Leviathan: Docker Container Network Orchestrator

Notes:


Linux:

ip links of type veth peer have the following name convention

```
<container id>.<leviathan peer number>i   - endpoint to be stuffed
inside the container

<container id>.<leviathan peer number>o   - endpoint to remain
outside the container
```

For Example:
```
4c01db0b339c.0i
4c01db0b339c.0o
```
Maximum Levianthan generated veth peers per cotainer is currently set
at hard limit of 10,000 ranging from 0-9999.  This limit stems from
the Linux network device name length limits and the Leviathan naming
convention that embeds the local container ID in each ip link peer
device name.

Generally the "inside" end of a peer link, will be renamed to
Eth<leviathan peer number> (e.g. Eth0) when it is assigned the
namespace for the container.

## CENs

Lucet provides an utility to publish CENs. See example [CEN config file](cen.json).

To test it:

1. Start [dobby_allinone_node](https://github.com/ivanos/dobby_allinone_node)
2. Run leviathan_lib
   1. set dobby node to `dobby_allinone@127.0.0.1` in the [sys.config file](sys.config)
   2. run with `make run cookie=dobby_allinone
3. Import the `cen.json`
   ```erlang
   leviathan_cen:import_cen_to_dobby("cen.json").
   ```
4. Check the Dobby Visualizer
[http://localhost:8080/static/www/index.html](http://localhost:8080/static/www/index.html)

> It's not possible to see all the CENs simultaneously as the visualizer
> can only display one root node at once.

## Leviathan Erlang Data Structures

A CEN is a represented by a map:

Key | Value | Description
--- | ----- | -----------
cenID | CEN ID | CEN Identifier
wiring_type | bus, wire, or null | type of wiring used
contIDs | list of container IDs | containers in the CEN

Example:
```
#{cenID => "cen1", wire_type => bus, contIDs => ["c1","c2","c3"]}
```

A Container is represented by a map:

Key | Value | Description
--- | ----- | -----------
contID | container ID | Container ID
cens | list of CEN IDs | Container is in these CENs

Example:
```
#{contID => "c1", cens => ["cen1","cen2"]}
```

A Wire is represented by a pair of maps in a list. Each map has:

Key | Value | Description
--- | ----- | -----------
endID | endpoint | Endpoint identifier (description above)
dest | destination map | See below

A destination map:

Key | Value | Description
--- | ----- | -----------
type | cont or cen | the endpoint is a container or CEN
ID | identifier | name of the CEN or the container ID
alias | string | (only for containers) interface name in the container
ip_address | string | (only for containers) IP address for interface

Examples:
```
[#{endID =>"c1.0i",
   dest => #{type => cont,
             id =>"c1",
             alias =>"eth0",
             ip_address => "10.8.2.13"}},
 #{endID =>"c1.0o",
   dest => #{type => cen,
             id =>"cen1"}}]
```
```
[#{endID =>"c2.2i",
   dest => #{type => cont,
             id =>"c2",
             alias =>"eth2",
             ip_address => "10.9.2.13"}},
 #{endID =>"c4.0i",
  dest => #{type => cont,
            id =>"c4",
            alias =>"eth0",
            ip_address => "10.9.2.14"}}]
```

## Dobby Data Model

Identifier names.  Fields starting with a Capital letter are the fillins:

Type | Name Format | Example
---- | ----------- | -------
CEN | lev_cen>CEN | lev_cen>cen1
container | lev_cont>Host>Container | lev_cen>4c01db0b339c
endpoint | lev_endpoint>Host>Endpoint | lev_endpiont>host1>4c01db0b339c.0i
bridge | lev_bridge>Host>Bridge | lev_bridge>host1>cen1
ip_address | lev_ip>IpAddress | lev_ip>10.9.2.14

Endpoints may be inside the container (in) or outside the container (out).

CEN metadata:

Key | Value | Description
--- | ----- | -----------
wiring_type | bus, wire, null | Type of wiring
status | pending, preparing, ready | Status of the CEN

There is no Container metadata.

Endpiont metadata:

Key | Value | Description
--- | ----- | -----------
alias | Alias Name | alias for this endpoint in the container (e.g., eth0)
status | pending, preparing, ready | Status of the endpoint

Bridge metadata:

Key | Value | Description
--- | ----- | -----------
status | pending, preparing, ready | Status of the bridge

Ip Address metadata:

None.

Links:

Type, Type | Link Type | Description
---------- | --------- | -----------
CEN, Container | part_of | container is in the CEN
Endpoint(in), Endpoint(in) | connected_to | connection between the in endpoints (wire)
Endpoint(in), Container | bound_to | endpoint is bound_to the container
Endpoint(in), Endpoint(out) | veth_peer | endpoints are ethernet peers
Endpoint(out), Bridge | bound_to | endpoint is bound_to a network bridge
Endpoint(cont), IpAddress | bound_to | IP Address of container endpoint
Bridge, Cen | policy_engine | Bridge manages network traffic for Cen

Where Endpoint(in) is an "inside" endpiont, Endpoint(out) is an "outside"
endpoint, Endpoint(cont) is a container endpoint.
