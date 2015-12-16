% macros for matching Leviathan identifiers in dobby

% pattern match a field of metadata
-define(MDVALUE(Key, Var), Key := #{value := Var}).

% pattern match the type of the identifier
-define(MDTYPE(Type), ?MDVALUE(<<"type">>, Type)).

% pattern match for a container and the container id
-define(MATCH_CONTAINER(ContId), #{?MDTYPE(<<"container">>),
                                   ?MDVALUE(<<"contID">>, ContId)}).

-define(MATCH_CONTAINER(HostId, ContId), #{?MDTYPE(<<"container">>),
                                          ?MDVALUE(<<"contID">>, ContId),
                                          ?MDVALUE(<<"host_id">>, HostId)}).

% mattern match for a bridge, the bridge id, and the ip address
-define(MATCH_BRIDGE(BridgeId), #{?MDTYPE(<<"bridge">>),
                                  ?MDVALUE(<<"bridgeID">>, BridgeId)}).

-define(MATCH_BRIDGE(HostId, BridgeId), #{?MDTYPE(<<"bridge">>),
                                          ?MDVALUE(<<"host_id">>, HostId),
                                          ?MDVALUE(<<"bridgeID">>, BridgeId)}).

% pattern match for a CEN, the CEN id, and the wire type
-define(MATCH_CEN(CenId, WireType), #{?MDTYPE(<<"cen">>),
                                       ?MDVALUE(<<"cenID">>, CenId),
                                       ?MDVALUE(<<"wire_type">>, WireType)}).

% pattern match for an endpoint and the endpoint id
-define(MATCH_ENDPOINT(EndId), #{?MDTYPE(<<"endpoint">>),
                                 ?MDVALUE(<<"endID">>, EndId)}).

% pattern match for an "inside" endpoint, the endpoint id, and the alias
-define(MATCH_IN_ENDPOINT(EndId, Alias), #{?MDTYPE(<<"endpoint">>),
                                          ?MDVALUE(<<"side">>, <<"in">>),
                                          ?MDVALUE(<<"endID">>, EndId),
                                          ?MDVALUE(<<"alias">>, Alias)}).

% pattern match for an "outside" endpoint and the endpoint id
-define(MATCH_OUT_ENDPOINT(EndId), #{?MDTYPE(<<"endpoint">>),
                                     ?MDVALUE(<<"side">>, <<"out">>),
                                     ?MDVALUE(<<"endID">>, EndId)}).

% pattern match for the ipaddr metadata field
-define(MATCH_IPADDR(IpAddr), #{?MDTYPE(<<"ipaddr">>),
                                ?MDVALUE(<<"ipaddr">>, IpAddr)}).
