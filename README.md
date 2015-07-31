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
