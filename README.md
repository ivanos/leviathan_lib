# leviathan_lib
Erlang code specific to Leviathan: Docker Container Network Orchestrator

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [leviathan_lib](#leviathanlib)
    - [Application Environment Variables](#application-environment-variables)
    - [CENs](#cens)
    - [JSON format example](#json-format-example)
    - [CENs top level API](#cens-top-level-api)
    - [CINs top level API](#cins-top-level-api)
    - [Environment Variables](#environment-variables)
    - [Leviathan CEN Layer](#leviathan-cen-layer)
        - [Leviathan CEN Map: The top level structure:](#leviathan-cen-map-the-top-level-structure)
        - [Leviathan CEN Authoritative Store: persistent store](#leviathan-cen-authoritative-store-persistent-store)
        - [Dobby CEN Data Model](#dobby-cen-data-model)
        - [Sequence Diagrams](#sequence-diagrams)
            - [Import CENs step](#import-cens-step)
            - [Make CENs step](#make-cens-step)
    - [Leviathan CIN Layer](#leviathan-cin-layer)
        - [Leviathan CIN Map: The top level structure:](#leviathan-cin-map-the-top-level-structure)
        - [Leviathan CIN Authoritative Store: persistent store](#leviathan-cin-authoritative-store-persistent-store)
        - [Leviathan CIN Dobby Data Model](#leviathan-cin-dobby-data-model)
        - [Sequence Diagrams](#sequence-diagrams)
            - [Import CINs step](#import-cins-step)
            - [Make CINs step](#make-cins-step)

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

## CENs top level API
Function | Args | Description
-------- | ---- | -----------
`leviathan_cen:import_file/2` | Hostname, Filename | constructs a CEN Leviathan Map from the file and imports it into the Authoritative Store and Dobby
`leviathan_cen:decode_file/1` | Filename | creates a Leviathan Map from CEN JSON file
`leviathan_cen:lm_compare/2` | OldLeviathanMap1, NewLeviathanMap2 | generates a delta list to transition from the old map to the new map
`leviathan_cen:add_container_to_cen/3` | Hostname, ContainerId, CenId | add a container to the CEN in Dobby and reconfigure the host
`leviathan_cen:remove_container_from_cen/3` | Hostname, ContainerId, CenId | remove a container from a CEN in Dobby and reconfigure the host
`leviathan_cen:prepare/1` | list of CEN identifiers | configures networking for the CENs by creating bridges and wires
`leviathan_cen:destroy/1` | list of CEN identifiers | destroys the network configuration for the CENs
`leviathan_dby:import_cens/2` | Hostname, Filename | imports a Leviathan Map into Dobby
`leviathan_dby:update_cens/2` | Hostname, Delta | applies deltas from `leviathan_cen:lm_compare/2` to Dobby
`leviathan_dby:set_cen_status/2` | CEN identifier, `preparing | pending | delta` | sets a CEN status in Dobby
`leviathan_cen_store:import_cens/2` | Hostname, Filename | imports a Leviathan Map into Authoritative Store
`leviathan_cen_store:update_cens/2` | Hostname, Delta | applies deltas from `leviathan_cen:lm_compare/2` to Authoritative Store
`leviathan_cen_store:get_levmap/1` | List of CEN Ids | constructs Leviathan Map for CENs

## CINs top level API
Function | Args | Description
-------- | ---- | -----------
`leviathan_cin:build_cins/1` | mappings between CINs and and CENs spanning given CIN  | creates CIN Leviathan Map
`leviathan_cin:prepare/1` | list of CIN identifiers | configures networking for the CINs by assigning IP addresses
`leviathan_cin:destroy/1` | list of CIN identifiers | destroys the network configuration for the CINs
`leviathan_dby:import_cins/2` | Hostname, CIN Leviathan Map | imports a Leviathan Map into Dobby
`leviathan_dby:update_cins/2` | Hostname, Delta | applies deltas from `leviathan_cin:lm_compare/2` to Dobby (not implemented)
`leviathan_dby:set_cin_status/2` | CIN identifier, `preparing | pending | delta` | sets a CIN status in Dobby
`leviathan_cin_store:import_cins/2` | Hostname, CIN Leviathan Map | imports a Leviathan Map into Authoritative Store
`leviathan_cin_store:update_cens/2` | Hostname, Delta | applies deltas from `leviathan_cin:lm_compare/2` to Authoritative Store (not implemented)
`leviathan_cin_store:get_levmap/1` | List of CIN identifiers | constructs Leviathan Map for CINs

## Environment Variables
Variable | Value | Description
-------- | ----- | -----------
docker_bin | `/usr/bin/docker events --util=""` | command line to read docker events

Set `docker_bin` to some other Unix utility (e.g., `cat`) if you do not have docker installed and want to experiment with this application.


## Leviathan CEN Layer
### Leviathan CEN Map: The top level structure:

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
contIDs | list of {string, string} | identifiers of containers in this CEN: {HostId, ContId}

Example:
``` erlang
 #{cenID => "cen1",
   wire_type => bus,
   contIDs => [{"h1", "c1"}, {"h1", "c2"}, {"h1", "c3"}],
 }
```

A Container is represented by a map:

Key | Value | Description
--- | ----- | -----------
contID | {string, string} | Container ID: {HostId, ContId}
cens | list of Container IDs | Container ids in these CENs
reservedIdNums | list of integers | Endpoint interfaces Ids used by this Container

The list of CENs and the list of reserved interface Ids are in the same order.
That is, the first the container uses the first interface Id for the first CEN.

Example:
``` erlang
 #{contID => {"h1", "c1"},
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

Examples:
``` erlang 
[#{endID =>"c1.0i",
   side => in,
   dest => #{type => cont,
             id => {"h1", "c1"},
             alias =>"eth0"}},
 #{endID =>"c1.0o",
   side => out,
   dest => #{type => cen,
             id => "cen1"}}]
```
``` erlang
[#{endID =>"c2.2i",
   side => in,
   dest => #{type => cont,
             id => {"h1", "c2"},
             alias =>"eth2"}},
 #{endID =>"c4.0i",
   side => in,
   dest => #{type => cont,
             id => {"h1", "c4"},
             alias =>"eth0"}}]
```

### Leviathan CEN Authoritative Store: persistent store

**leviathan_cen** table:

Key | Value Type | Description
--- | ----- | -----------
cen | string | CEN identifier
data | map | Data describing CEN
wires | list containing map pairs in a list | Wire follows the same format as described above.

Map describing CEN:

Key | Value Type | Description
--- | ----- | -----------
contIDs | list of {string, string} | list of Container IDs ({HostId, ContId})
wire_type | atom: bus, wire or null | the type of wiring used

Example table record:

```erlang
{leviathan_cen, "cen1",
 #{contIDs => [{"h1", "cont1"}, {"h1", "cont2"}],
   wire_type => bus},
 [[#{dest => #{alias => "cen1",
               id => {"h1", "cont1"},
               type => cont},
     endID => "cont1.0i",
     side => in},
   #{dest => #{id => "cen1",type => cen},
     endID => "cont1.0o",
     side => out}],
  [#{dest => #{alias => "cen1",
               id => {"h1", "cont2"},
               type => cont},
     endID => "cont2.0i",
     side => in},
   #{dest => #{id => "cen1",type => cen},
     endID => "cont2.0o",
     side => out}]]}
```

**leviathan_cen_cont** table:

Key | Value Type | Description
--- | ----- | -----------
cont | {string, string} | Container identifier: {HostId, ContId}
cen | string | CEN id the Container is in
idnumber | integer | Endpoint interfaces Ids used by the Container in this CEN

Example table record:

```erlang
{leviathan_cont, {"h1", "cont2"}, "cen1", 0}
```

### Dobby CEN Data Model

Identifier names.  Fields starting with a Capital letter are the fillins:

Type | Name Format | Example
---- | ----------- | -------
CEN | lev_cen>CEN | lev_cen>cen1
container | lev_cont>Host>Container | lev_cen>4c01db0b339c
endpoint | lev_endpoint>Host>Endpoint | lev_endpiont>host1>4c01db0b339c.0i
bridge | lev_bridge>Host>Bridge | lev_bridge>host1>cen1

Endpoints may be inside the container (in) or outside the container (out).

CEN metadata:

Key | Value | Description
--- | ----- | -----------
wire_type | bus, wire, null | Type of wiring
status | pending, preparing, ready | Status of the CEN

Container metadata:

Key | Value | Description
--- | ----- | -----------
host_id | string | identifier of a host that the container is in

Endpoint metadata:

Key | Value | Description
--- | ----- | -----------
alias | Alias Name | alias for this endpoint in the container (e.g., eth0)
status | pending, preparing, ready | Status of the endpoint

Bridge metadata:

Key | Value | Description
--- | ----- | -----------
status | pending, preparing, ready | Status of the bridge

Links:

Type, Type | Link Type | Description
---------- | --------- | -----------
CEN, Container | part_of | container is in the CEN
Endpoint(in), Endpoint(in) | connected_to | connection between the in endpoints (wire)
Endpoint(in), Container | bound_to | endpoint is bound_to the container
Endpoint(in), Endpoint(out) | veth_peer | endpoints are ethernet peers
Endpoint(out), Bridge | bound_to | endpoint is bound_to a network bridge
Bridge, Cen | policy_engine | Bridge manages network traffic for Cen

Where Endpoint(in) is an "inside" endpiont, Endpoint(out) is an "outside"
endpoint, Endpoint(cont) is a container endpoint.

### Sequence Diagrams

#### Import CENs step
![sd](http://www.websequencediagrams.com/cgi-bin/cdraw?lz=cGFydGljaXBhbnQgQ2xpZW50CgAHDFJFU1QgQVBJIGFzAAcFABANbGV2aWF0aGFuX2NlbiBhcyBDRU4ABxpfc3RvcmUgYXMgU3RvcmUAMxdkYnkgYXMgRGJ5CgoAgQoGIC0-AIECBTogL2Nlbi9pbXBvcnQKbm90ZSBvdmVyAIEvBywAgSYFLCBDRU46IHRoZQCBRgcgc2VuZHMgYSBKU09OIGZpbGUgZGVzY3JpYmluZyBDRU5zCgoAgV8FLT4ANQZkZWNvZGVfYmluYXJ5KEpTT04pCkNFTi0AfAlDZW5MTQArCQCBRgU6IACBDwZfY2VucygAHAUpAE0JRGJ5AAUbLT4Agm8HOg&s=roundgreen)

#### Make CENs step
![sd](http://www.websequencediagrams.com/cgi-bin/cdraw?lz=cGFydGljaXBhbnQgQ2xpZW50CgAHDFJFU1QgQVBJIGFzAAcFABANbGV2aWF0aGFuX2NlbiBhcyBDRU4ABxpfc3RvcmUgYXMgU3RvcmUAMxdkYnkgYXMgRGJ5CgoAgQoGIC0-AIECBTogL2Nlbi9tYWtlCm5vdGUgb3ZlcgCBLQcsAIEkBSwgQ0VOOiB0aGUAgUQHIHNlbmRzICdbImNlbjEiLCJjZW4yIl0nCgCBUgUtPgAqBnByZXBhcmUoQ2VuSWRzKSBbYXN5bmNdAB4HLT4AghEHOgoKQ0VOIC0-AIE6BjogZ2V0X2xldm1hcAAzCAoAgVUFAC4GRU46IENlbkxNAC4JRGJ5OiBzZXQgc3RhdHVzIG9mAIElBkVOcyBhcwCBAAdpbmcAXwgAgREMX2NlbnMoAEsFKQAKFm9udAAEHXdpcmUAMhAAcx9yZWFkeQCCZgZyaWdodCBvZgCCXAZOb3cAgmEFYmlyZGdlcyBhbmRcbiBpbnRlcmZhY2VzIGFyZSBicm91Z2h0IHVwXG4gYW5kIHdpcmVkIHRvZ2V0aGVyLg&s=roundgreen)

## Leviathan CIN Layer
### Leviathan CIN Map: The top level structure:

Key | Value | Description
--- | ----- | -----------
cins | list of CinMaps | list of CIN maps
conts | list of ContMaps | list of Containers maps

Example:
``` erlang
 #{cins => [CinMap1, CinMap2, ...],
   conts => [ContMap1, ContMap2, ...]
 }
```

CIN map:

Key | Value | Description
--- | ----- | -----------
cinID | string | CIN Identifier
contIDs | list of container IDs | containers ids ({HostId, ContId}) in this CIN
ip_b | integer | (only for bus) B part of the IP addresses for CIN
addressing | map | A map that describes IP addressing in this CIN

CIN addressing map:

Key | Value | Description
--- | ----- | -----------
CenId (string) | #{interface => string, ip => string &#124; null} | a map consisting of the bridge interface for a CEN and its IP address

Examples of CIN maps:
``` erlang
 #{cinID => "cin1",
   contIDs => [{"h1", "c1"}, {"h1", "c2"},  {"h1", "c3"}],
   ip_b => 17,
   addressing => #{"cen1" => #{interface => "cen1_br", ip => "10.17.0.1""}}
  }
```

``` erlang
 #{cinID => "cin1",
   contIDs => [{"h1", "c1"}, {"h1", "c2"},  {"h1", "c3"}],
   ip_b => 17,
   addressing => #{"cen1" => #{interface => "cen1", ip => "10.17.0.1"},
                   "cen2" => #{interface => "cen2", ip => "10.17.0.2"}}}
  }
```

A Container is represented by a map:

Key | Value | Description
--- | ----- | -----------
contID | {string, string} | Container identifier {HostId, ContId}
cinID | string | CIN identifier the container is in
addressing | map | A map that describes IP addressing of the Container in this CIN

Key | Value | Description
--- | ----- | -----------
CenId (string) | #{endID => string, interface => string, ip => string &#124; null} | a map consisting of the Container inside endpoint id, its interface, and the IP address of the endpoint in this CEN

Examples of Container maps:
``` erlang
 #{contID => {"h1", "c1"},
   cinID => "cin1",
   addressing => #{"cen1" => #{endID => "c1.0i", interface => "eth0", ip => "10.17.0.10"}}
  } 
```

``` erlang
 #{contID => {"h1", "c1"},
   cinID => "cin1",
   addressing => #{"cen1" => #{endID => "c1.0i", interface => "eth0", ip => "10.17.0.10"},
                   "cen2" => #{endID => "c1.1i", interface => "eth1", ip => "10.17.0.11"}}
  } 
```

### Leviathan CIN Authoritative Store: persistent store

**leviathan_cin** table:

Key | Value Type | Description
--- | ----- | -----------
cin | string | CIN identifier
data | map | Data describing CIN

CIN description map:

Key | Value Type | Description
--- | ----- | -----------
contIDs | {string, string} | list of Container IDs ({HostId, ContId})
ip_b | integer | (only for bus) B part of the IP addresses for CIN
addressing | map | A map that describes IP addressing in this CIN

Example table record:

```erlang
{leviathan_cin, "cin1",
  #{contIDs => [{"host1", "cont1"},{"host1", "cont2"}],
    ip_b => 10,
    addressing => #{"cen1" => #{interface => "cen1_br", ip => "10.17.0.1""}}
   }
}
```

**leviathan_cin_cont** table:

Key | Value Type | Description
--- | ----- | -----------
cont | {string, string} | Container identifier ({HostId, ContId})
cin | string | CIN identifier the Container is in
data | map | Data describing the Container

Container description map:

Key | Value Type | Description
--- | ----- | -----------
addressing | map | A map that describes IP addressing of the Containers in this CIN

Example table record:

```erlang
{leviathan_cin_cont, {"host1", "cont2"}, "cin1",
   addressing => #{"cen1" => #{endID => "c1.0i", interface => "cen1", ip => "10.17.0.10"}}
}
```

### Leviathan CIN Dobby Data Model

Identifier names.  Fields starting with a Capital letter are the fillins:

Type | Name Format | Example
---- | ----------- | -------
cin | lev_cin>CIN | lev_cin>cin1
ipaddr | lev_ip>IpAddress | lev_ip>10.9.2.14

CIN metadata:

Key | Value | Description
--- | ----- | -----------
status | pending, preparing, ready | Status of the CIN

There is no Ip Address metadata.

Links:

Type, Type | Link Type | Description
---------- | --------- | -----------
CIN, IpAddress | part_of | IP Address of a container/bridge in this CIN (IP Address is in the CIN)
Endpoint(in), IpAddress | bound_to | IP Address of a container in this CIN (cross layer link)
Bridge, IpAddress | bound_to | IP Address of a bridge in this CIN (corss layer link)
CIN, CEN | part_of | A CEN that this CIN covers

`Endpoint(in)`, `CEN` and `Bridge` identifiers are created in the CEN Layer.

### Sequence Diagrams

#### Import CINs step
![sd](http://www.websequencediagrams.com/cgi-bin/cdraw?lz=cGFydGljaXBhbnQgQ2xpZW50CgAHDFJFU1QgQVBJIGFzAAcFABANbGV2aWF0aGFuX2NpbiBhcyBDSU4ABxpfc3RvcmUgYXMgU3RvcmUAMxdkYnkgYXMgRGJ5CgoAgQoGIC0-AIECBTogL2Npbi9pbXBvcnQKbm90ZSBvdmVyAIEvBywAgSYFLCBDSU46IHRoZQCBRgcgc2VuZHMgJ1siY2luMSI6WyJjZW4xIl0sICJjaW4yAAoHMiJdXScKAIFnBS0-AD0GYnVpbGRfY2lucyhDaW5JZFRvQ2VuSWRzTWFwKQBxDElOLACBSgY6IAAYECBkZWlmaW5lcyBDRU5zLCBhIACCZwZ1bGFyIENpbiB3aWxsIGNvbnRhaW47Cgpsb29wIGZvciBlYWNoIABqBSBpbgBGEQogICAgQ0lOIC0-IERieTogZ2V0X2NlbigAgRwFKQAaBURieSAtAIFDCENlbk1hcCBhcyBkZWZpbmVkIGJ5AIMwDGVuCmVuZACBPRgANwZpcyB1c2VkIHRvIGdldCBDb250SWRzIGFuZCBjcmVhdGUgQWRkcmVzc2luZwCBPwUAgn4FSU5zAIEZPndpcmVzAIFAGVdpcmVzAIEhNwA1B3JlAIFPCQCBLRsAgx0GZXJzAIMLBUlOcwoAgnsFAIUQCUNpbkxNAIQ3CQCECAcAhSMGAIQ6CUxNKQCEWQkAgzIFAAcZLT4AhwMHOg&s=roundgreen)

#### Make CINs step
![sd](http://www.websequencediagrams.com/cgi-bin/cdraw?lz=cGFydGljaXBhbnQgQ2xpZW50CgAHDFJFU1QgQVBJIGFzAAcFABANbGV2aWF0aGFuX2NpbiBhcyBDSU4ABxpfc3RvcmUgYXMgU3RvcmUAMxdkYnkgYXMgRGJ5CgoAgQoGIC0-AIECBTogL2Npbi9tYWtlCm5vdGUgb3ZlcgCBLQcsAIEkBSwgQ0lOOiB0aGUAgUQHIHNlbmRzICdbImNpbjEiLCJjaW4yIl0nCgoAgVMFLT4AKwZwcmVwYXJlKENpbklkcykgW2FzeW5jXQAeBy0-AIISBzoKQ0lOIC0-AIE6BjogZ2V0X2xldm1hcAAyCAoAgVUFAC0GSU46IENpbkxNAC4IAIEXBWRvXwBjC0xNKQoATQhEYnk6IHNldCBzdGF0dXNlcyBvZgCBRQZJTnMgaW4ARwYgYXMAgSgHaW5nAE0NAIE-B19jaW5zAFQIAAsVb250ABEQAFIqcmVhZHkKAIJ0DElOLACCCQhub3cAgngFYnJpZGdlcyBhbmQAgwgFY29udGFpbmVycyBoYXZlIHRoZWlyIElQcw&s=roundgreen)
