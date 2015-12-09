-type wire_type() :: null | wire | bus.

-type in_endpoint() :: #{endID => string(),
                         side => in,
                         dest => #{type => cont,
                                   id => string(),
                                   alias => string(),
                                   ip_address => string()}}.

-type out_endpoint() :: #{endID => string(),
                          side => out,
                          dest => #{type => cen,
                                    id => string()}}.

% map of cen to containers
-record(leviathan_cen, {cen :: string(),
                        data :: #{contIDs => [string()],
                                  wire_type => wire_type(),
                                  bridges => list(),
                                  master_hostid => string(),
                                  hostid_to_node => maps:map(),
                                  tunnels => list()},
                        wires :: [[in_endpoint() | out_endpoint()]]}).

% connection between cen and container
-record(leviathan_cen_cont, {cont :: string(),
                             cen :: string(),
                             idnumber :: non_neg_integer()}).

-record(leviathan_cin, {cin :: string(),
                        data :: #{contIDs => [leviathan_cin:cont_id()],
                                  ip_b => 0..255,
                                  addressing => leviathan_cin:addressing()}}).

-record(leviathan_cin_cont, {cont :: leviathan_cin:cont_id(),
                             cin :: string(),
                             addressing :: leviathan_cin:addressing()}).

% persistent counters
-record(counter, {id :: atom(), count :: integer()}).
