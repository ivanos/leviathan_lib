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
                                  ipaddr_b => non_neg_integer(),
                                  ipaddr => string()},
                        wires :: [[in_endpoint() | out_endpoint()]]}).

% connection between cen and container
-record(leviathan_cont, {cont :: string(),
                         cen :: string(),
                         data :: #{idnumber => integer(),
                                   ip_address => string()}}).

-record(leviathan_cin, {cin :: string(),
                        data :: #{cenIDs => [string()],
                                  contIDs => [string()],
                                  ip_b => 0..255,
                                  ip => inet:ip4_address()}}).

-record(leviathan_cin_cont, {cont :: string(),
                             cin :: string(),
                             ip :: inet:ip4_address()}).

% persistent counters
-record(counter, {id :: atom(), count :: integer()}).
