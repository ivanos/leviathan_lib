# leviathan_lib
Erlang code specific to Leviathan: Docker Container Network Orchestrator

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [leviathan_lib](#leviathanlib)
    - [Application Environment Variables](#application-environment-variables)
    - [CENs](#cens)
    - [JSON format example](#json-format-example)
    - [Top level API](#top-level-api)
    - [Environment Variables](#environment-variables)
    - [Leviathan Erlang Data Structures](#leviathan-erlang-data-structures)
        - [Leviathan Map: The top level structure:](#leviathan-map-the-top-level-structure)
        - [Leviathan Authoritative Store: persistent store](#leviathan-authoritative-store-persistent-store)
    - [Dobby Data Model](#dobby-data-model)

<!-- markdown-toc end -->


## Application Environment Variables
| Varible | Description |
| ------- | ----------- |
| docker_bin | Location of the docker bin for reading events |

For testing without Docker, set `docker_bin` to `"cat"`.

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

Leviathan provides utility functions to publish CENs. See example [CEN config file](cen.json).

To test it:

1. Start [dobby_allinone_node](https://github.com/ivanos/dobby_allinone_node)
2. Run leviathan_lib
   1. set dobby node to `dobby_allinone@127.0.0.1` in the [sys.config file](sys.config)
   2. run with `make run cookie=dobby_allinone
3. Import the `cen.json`
   ```erlang

leviathan_cen:import_file("host1", "cen.json").
   ```
4. Check the Dobby Visualizer
[http://localhost:8080/static/www/index.html](http://localhost:8080/static/www/index.html)

It's not possible to see all the CENs simultaneously as the visualizer
can only display one root node at once.

## JSON format example
``` json
{"cenList":
 [{
     "cenID" : "cen1",
     "containerIDs" : [ "c1","c2","c13","c14"]
  },
  {
      "cenID":"cen2",
      "containerIDs":["c4","c5","c6","c7"]
  },
  {
      "cenID":"cen3",
      "containerIDs":["c15","c16","c9","c10","c11"]
  },
  {
      "cenID":"cen4",
      "containerIDs":["c11","c12"]
  },
  {
      "cenID":"cen5",
      "containerIDs":["c2","c3"]
  }]
}
```
Where:
* `cenID` is the CEN identifier
* `containerIDs` is a list of container identifiers

## Top level API
Function | Args | Description
-------- | ---- | -----------
`leviathan_cen:import_file/2` | Hostname, Filename | imports CEN JSON file into Dobby
`leviathan_cen:decode_file/1` | Filename | creates a Leviathan Map from CEN JSON file
`leviathan_cen:lm_compare/2` | OldLeviathanMap1, NewLeviathanMap2 | generates a delta list to transition from the old map to the new map
`leviathan_cen:add_container_to_cen/3` | Hostname, ContainerId, CenId | add a container to the CEN in Dobby and reconfigure the host
`leviathan_cen:remove_container_from_cen/3` | Hostname, ContainerId, CenId | remove a container from a CEN in Dobby and reconfigure the host
`leviathan_dby:import_cens/2` | Hostname, Filename | imports a Leviathan Map into Dobby
`leviathan_dby:update_cens/2` | Hostname, Delta | applies deltas from `leviathan_cen:lm_compare/2` to Dobby
`leviathan_store:import_cens/2` | Hostname, Filename | imports a Leviathan Map into Authoritative Store
`leviathan_store:update_cens/2` | Hostname, Delta | applies deltas from `leviathan_cen:lm_compare/2` to Authoritative Store
`leviathan_store:get_levmap/1` | List of CEN Ids | constructs Leviathan Map for CENs

## Environment Variables
Variable | Value | Description
-------- | ----- | -----------
docker_bin | `/usr/bin/docker events --util=""` | command line to read docker events

Set `docker_bin` to some other Unix utility (e.g., `cat`) if you do not have docker installed and want to experiment with this application.

## Leviathan Erlang Data Structures

### Leviathan Map: The top level structure:

Key | Value | Description
--- | ----- | -----------
censmap | #{cens => Cens} | map with list of CEN maps
contsmap | #{conts => Conts} | map with list of containers maps
wiremap | #{wires => Wires} | map with list of wire maps

Example:
``` erlang
 #{censmap => #{cens => [...]},
   contsmap => #{conts => [...]},
   wiremap => #{wires => [...]}
 }
```

A CEN is a represented by a map:

Key | Value | Description
--- | ----- | -----------
cenID | CEN ID | CEN Identifier
wire_type | bus, wire, or null | type of wiring used
contIDs | list of container IDs | containers in the CEN
ipaddr_b | integer | (only for bus) B part of the IP addresses for CEN
ipaddress | string | IP address of bridge
reservedIp | list of strings | Ip addresses in use in this CEN

The list of containers in the CEN and the list of reserved IP addresses
are in the same order. That is, the first container uses the first IP
address.

Example:
``` erlang
 #{cenID => "cen1",
   wire_type => bus,
   contIDs => ["c1","c2","c3"],
   ipaddr_b => 17,
   reservedIp => ["10.17.0.1", "10.17.0.2", ...]
 }
```

A Container is represented by a map:

Key | Value | Description
--- | ----- | -----------
contID | container ID | Container ID
cens | list of CEN IDs | Container is in these CENs
reservedIdNums | list of integers | Endpoint interfaces Ids used by this Container

The list of CENs and the list of reserved interface Ids are in the same order.
That is, the first the container uses the first interface Id for the first CEN.

Example:
``` erlang
 #{contID => "c1",
   cens => ["cen1","cen2"],
   reservedIdNums => [0, 1, 2, ...]
 }
```

A Wire is represented by a pair of maps in a list. Each map has:

Key | Value | Description
--- | ----- | -----------
endID | endpoint | Endpoint identifier (description above)
side | in or out | desintation is inside or outside the container
dest | destination map | See below

A destination map:

Key | Value | Description
--- | ----- | -----------
type | cont or cen | the endpoint is a container or CEN
ID | identifier | name of the CEN or the container ID
alias | string | (only for containers) interface name in the container
ip_address | string | (only for containers) IP address for interface

Examples:
``` erlang 
[#{endID =>"c1.0i",
   side => in,
   dest => #{type => cont,
             id =>"c1",
             alias =>"eth0",
             ip_address => "10.8.2.13"}},
 #{endID =>"c1.0o",
   side => out,
   dest => #{type => cen,
             id =>"cen1"}}]
```
``` erlang
[#{endID =>"c2.2i",
   side => in,
   dest => #{type => cont,
             id =>"c2",
             alias =>"eth2",
             ip_address => "10.9.2.13"}},
 #{endID =>"c4.0i",
   side => in,
   dest => #{type => cont,
             id =>"c4",
             alias =>"eth0",
             ip_address => "10.9.2.14"}}]
```

### Leviathan Authoritative Store: persistent store

**leviathan_cen** table:

Key | Value Type | Description
--- | ----- | -----------
cen | string | CEN identifier
data | map | Data describing CEN
wires | list containing map pairs in a list | Wire follows the same format as described in above.

Map describing CEN:

Key | Value Type | Description
--- | ----- | -----------
contIDs | string | list of Container IDs
wire_type | atom: bus, wire or null | the type of wiring used
ipaddr_b | integer | (only for bus) B part of the IP addresses for CEN
ipaddr | string | IP address of bridge

Example table record:

```erlang
{leviathan_cen, "cen1",
 #{contIDs => ["cont1","cont2"],
   ipaddr => "10.10.0.1",
   ipaddr_b => 10,
   wire_type => bus},
 [[#{dest => #{alias => "cen1",
               id => "cont1",
               ip_address => "10.10.0.10",
               type => cont},
     endID => "cont1.0i",
     side => in},
   #{dest => #{id => "cen1",type => cen},
     endID => "cont1.0o",
     side => out}],
  [#{dest => #{alias => "cen1",
               id => "cont2",
               ip_address => "10.10.0.11",
               type => cont},
     endID => "cont2.0i",
     side => in},
   #{dest => #{id => "cen1",type => cen},
     endID => "cont2.0o",
     side => out}]]}
```

**leviathan_cont** table:

Key | Value Type | Description
--- | ----- | -----------
cont | string | Container identifiers
cen | string | CEN id the Container is in
data | map | Data describing Container

Map describing Container:

Key | Value Type | Description
--- | ----- | -----------
idnumber | integer | Endpoint interfaces Ids used by the Container in this CEN
ip_address | string | IP address of the Container

Example table record:

```erlang
{leviathan_cont, "cont2", "cen1", #{idnumber => 0,ip_address => "10.10.0.11"}},
```

## Dobby Data Model

Identifier names.  Fields starting with a Capital letter are the fillins:

Type | Name Format | Example
---- | ----------- | -------
CEN | lev_cen>CEN | lev_cen>cen1
container | lev_cont>Host>Container | lev_cen>4c01db0b339c
endpoint | lev_endpoint>Host>Endpoint | lev_endpiont>host1>4c01db0b339c.0i
bridge | lev_bridge>Host>Bridge | lev_bridge>host1>cen1
ipaddr | lev_ip>IpAddress | lev_ip>10.9.2.14

Endpoints may be inside the container (in) or outside the container (out).

CEN metadata:

Key | Value | Description
--- | ----- | -----------
wire_type | bus, wire, null | Type of wiring
status | pending, preparing, ready | Status of the CEN

There is no Container metadata.

Endpoint metadata:

Key | Value | Description
--- | ----- | -----------
alias | Alias Name | alias for this endpoint in the container (e.g., eth0)
status | pending, preparing, ready | Status of the endpoint

Bridge metadata:

Key | Value | Description
--- | ----- | -----------
status | pending, preparing, ready | Status of the bridge
ipaddr | String | IP Address of the bridge

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
