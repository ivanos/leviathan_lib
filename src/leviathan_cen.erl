-module(leviathan_cen).

-compile(export_all).

-export([decode_file/1,
         decode_binary/1,
         remove_container_from_cen/3,
         add_container_to_cen/3,
         destroy_cen/1,
         new_cen/1]).

-ifdef(TEST).
-export([decode_jiffy/1]).
-endif.

-include("leviathan_logger.hrl").

%-------------------------------------------------------------------------------
% API
%-------------------------------------------------------------------------------

decode_file(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    decode_binary(Binary).

decode_binary(Binary) ->
    #{<<"cenList">> := Cens} = jiffy:decode(Binary, [return_maps]),
    decode_jiffy(Cens).


% Add a container to a CEN
add_container_to_cen(HostId, ContainerId, CenId) ->
    % XXX not implemented
    ?INFO("Add container to cen: Container(~s, ~s), Cen(~s)",
                                            [HostId, ContainerId, CenId]),
    ok.

% Remove a container from a CEN
remove_container_from_cen(HostId, ContainerId, CenId) ->
    % XXX not implemented
    ?INFO("Remove container from cen: Container(~s, ~s), Cen(~s)",
                                            [HostId, ContainerId, CenId]),
    ok.

% Create new CEN
new_cen(CenId) ->
    % XXX not implemented
    ?INFO("Create cen: Cen(~s)", [CenId]),
    ok.
    
% Destroy CEN
destroy_cen(CenId) ->
    % XXX not implemented
    ?INFO("Remove cen: Cen(~s)", [CenId]),
    ok.

% To test:
% 1. load the cen.json file in this repo via leviathan_dby:import_file/2
%    or use curl and the REST interface (see leviathan_rest_lib).
%    The host name must be "host1"
% 2. test_cens/0 returns the cen IDs of the cens in the .json file, so
%    you can use that to save typing
% 3. inspect the levmap:
%       leviathan_cen:get_levmap(leviathan_cen:test_cens()).
% 4. test prepare:
%       leviathan_cen:test_local_prepare_lev(leviathan_cen:test_cens()).

% return list of cens ids in the cen.json file
test_cens() ->
    ["cen1","cen2", "cen3", "cen4","cen5"].

% call main entry point to run prepare
test_local_prepare_lev(CenIds)->
    prepare_lev(get_levmap(CenIds)).

% build the top level data structure for prepare; pulls data from dobby
get_levmap(CenIds) ->
    Cens = get_cens(CenIds),
    #{censmap => #{cens => Cens},
     contsmap => #{conts => get_conts(Cens)},
     wiremap => #{wires => get_wiremaps(Cens)}
    }.

get_cens(CenIds) ->
    [leviathan_dby:get_cen(CenId) || CenId <- CenIds].

% XXX host is hardcoded
get_conts(Cens) ->
    ContIds = contids_from_cens(Cens),
    [leviathan_dby:get_cont("host1", ContId) || ContId <- ContIds].

% XXX host is hardcoded
% leviathan_dby:get_wires/1 returns the list of wires per Cen. Flatten
% the list with lists:append/1 rather than with lists:flatten/1 because
% the wires themselves are lists.
get_wiremaps(Cens) ->
    lists:append([leviathan_dby:get_wires(Cen) || Cen <- Cens]).

% make a list of unique container ids by inspecting the cens
contids_from_cens(Cens) ->
    sets:to_list(lists:foldl(
        fun(#{contIDs := ContIds}, Acc) ->
            sets:union(Acc, sets:from_list(ContIds))
        end, sets:new(), Cens)).

% prepare cens from a list of cen ids
prepare(CenIds) ->
    prepare_lev(get_levmap(CenIds)).

%
% Top Level Processor
%
prepare_lev(#{censmap := CensMap, contsmap := ContsMap, wiremap := WireMap}) ->
    % mark cens as preparing
    cens_status(CensMap, preparing),
    prepare_cens(CensMap),
    prepare_conts(ContsMap),
    prepare_wires(WireMap),
    % mark cens as ready
    cens_status(CensMap, ready).

cens_status(#{cens := Cens}, Status) ->
    lists:foreach(
        fun(#{cenID := CenId}) ->
            leviathan_dby:set_cen_status(CenId, Status)
        end, Cens).

prepare_cens(#{cens := Cens}) ->
    %%     make any necessary Ethernet buses
    %%     if a Cen has more than 2 containers, we'll create a bus
    %%
    lists:foreach(fun(CenMap)->
			  #{wire_type := CenType, 
			    ip_address := IPAddress} = CenMap,
			  case CenType of
			      bus ->
				  #{cenID := CenId} = CenMap,
				  CmdBundle = leviathan_linux:new_bus(CenId,IPAddress),
				  leviathan_linux:eval(CmdBundle);
			      _ -> ok %% don't create a bus
			  end
		  end, Cens).

prepare_conts(#{conts := Conts}) ->
    lists:foreach(fun(#{contID := ContId})->
			  CmdBundle = leviathan_linux:set_netns(ContId),
			  leviathan_linux:eval(CmdBundle)
		  end,Conts).

prepare_wires(WireMap)->
    #{wires := Wires} = WireMap,
    lists:foreach(fun(Wire)->prepare_wire(Wire) end, Wires).

prepare_wire(Wire = [#{endID := EndId1}, #{endID := EndId2}]) ->
    CmdBundle = leviathan_linux:new_peer(EndId1, EndId2),
    leviathan_linux:eval(CmdBundle),				      
    lists:foreach(fun(WireEnd) -> prepare_wire_end(WireEnd) end, Wire).

prepare_wire_end(#{endID := EndId, dest := #{type := cen, id := CenId}}) ->
    CmdBundle = leviathan_linux:peer2cen(CenId,EndId),
    leviathan_linux:eval(CmdBundle);				      
prepare_wire_end(#{endID := EndId,
                dest := #{type := cont, id := ContId, alias := Alias}} = WireEnd) ->
    CmdBundle = leviathan_linux:peer2cont(ContId, EndId, Alias),
    leviathan_linux:eval(CmdBundle),
    case WireEnd of
	#{endID := EndId,
	  dest := Dest} ->
	    leviathan_cin:prepare_wire_end(Dest);
	_ -> ok  %% No IP Address
    end.
	


%% === DESTROY ===== %%%

% destroy cens from a list of cen ids
destroy(CenIds) ->
    destroy_lev(get_levmap(CenIds)).

%
% Top Level Processor
%
destroy_lev(#{censmap := CensMap, wiremap := WireMap}) ->
    cens_status(CensMap, destroy),
    destroy_cens(CensMap),
    destroy_wires(WireMap),
    cens_status(CensMap, pending).

destroy_cens(#{cens := Cens}) ->
    %%     make any necessary Ethernet buses
    %%     if a Cen has more than 2 containers, we'll create a bus
    %%
    lists:foreach(fun(CenMap)->
			  #{wire_type := CenType} = CenMap,
			  case CenType of
			      bus ->
				  #{cenID := CenId} = CenMap,
				  CmdBundle = leviathan_linux:delete_bus(CenId),
				  leviathan_linux:eval(CmdBundle);
			      _ -> ok %% don't create a bus
			  end
		  end, Cens).

destroy_wires(WireMap)->
    #{wires := Wires} = WireMap,
    lists:foreach(fun(Wire)->destroy_wire(Wire) end, Wires).

destroy_wire(Wire) ->
    lists:foreach(fun(WireEnd) -> destroy_wire_end(WireEnd) end, Wire).

destroy_wire_end(#{endID := EndId, dest := #{type := cen}}) ->
    CmdBundle = leviathan_linux:delete_peer(EndId),
    leviathan_linux:eval(CmdBundle);				      
destroy_wire_end(#{dest := #{type := cont, id := ContId, alias := Alias}}) ->
    CmdBundle = leviathan_linux:delete_cont_interface(ContId,Alias),
    leviathan_linux:eval(CmdBundle).

% -----------------------------------------------------------------------------
%
% Decode JSON
%
% -----------------------------------------------------------------------------

%% process decoded json
decode_jiffy(CensJson) ->
    ?DEBUG("CensJson:~n~p~n",[CensJson]),
    Cens = cens_from_jiffy(CensJson),
    Conts = conts_from_jiffy(CensJson),
    Wires = wire_cens(Cens),
    #{
        censmap => #{cens => Cens},
        contsmap => #{conts => Conts},
        wiremap => #{wires => Wires}
    }.

% cens
cens_from_jiffy(CensJson) ->
    {_, Cens} = lists:foldl(
        fun(#{<<"cenID">> := Cen, <<"containerIDs">> := Conts}, {Count, Acc}) ->
            {Count + 1, [cen(Count, Cen, wire_type(Conts), Conts) | Acc]}
        end, {1, []}, CensJson),
    Cens.

wire_type(Conts) when length(Conts) < 2 ->
    null;
wire_type(Conts) when length(Conts)  == 2 ->
    wire;
wire_type(Conts) when length(Conts)  > 2 ->
    bus.

cen(Count, Cen, bus, Conts) ->
     #{cenID => Cen,
       wire_type => bus,
       contIDs => Conts,
       ipaddr => cen_ip_addr(Count)};
cen(_, Cen, WireType, Conts) ->
     #{cenID => Cen,
       wire_type => WireType,
       contIDs => Conts}.

% conts
conts_from_jiffy(CensJson) ->
    Pairs = cen_cont_pairs(CensJson),
    Index = lists:foldl(
        fun ({Cen, Cont}, Acc) ->
            maps_append(Cont, Cen, Acc)
        end, #{}, Pairs),
    maps:fold(
        fun(Cont, Cens, Acc) ->
            [#{contID => Cont, cens => Cens} | Acc]
        end, [], Index).

cen_cont_pairs(CensJson) ->
    lists:foldl(
        fun(#{<<"cenID">> := CenId, <<"containerIDs">> := ContainerIds}, Acc) ->
            make_pair(CenId, ContainerIds, Acc);
           (_, _) ->
            throw(bad_json)
        end, [], CensJson).

make_pair(Const, List, Acc) ->
    lists:foldl(
        fun(Element, Acc0) ->
            [{Const, Element} | Acc0]
        end, Acc, List).

maps_append(Key, Value, Map) ->
    Old = maps:get(Key, Map, []),
    maps:put(Key, [Value | Old], Map).

wire_cens(Cens) ->
    #{wires := Wires} = lists:foldl(
        fun(#{cenID := CenId, contIDs := ContIds}, Context) ->
            wire_cen(count_cen(Context), CenId, ContIds)
        end, #{count => #{}, wires => []}, Cens),
    Wires.

% wiring helpers

wire_cen(Context, _, []) ->
    Context;
wire_cen(Context, _, [_]) ->
    Context;
wire_cen(Context0, CenId, [ContId1, ContId2]) ->
    % wire the containers directly if there are two containers in the CEN
    Context1 = count_cont(Context0, CenId),
    {Context2, ContId1InEndpoint} = next_in_endpoint(Context1, ContId1),
    {Context3, Cont1Eth} = next_eth(Context2, ContId1),
    Cont1IpAddr = ip_addr(Context3, CenId),
    Context4 = count_cont(Context3, CenId),
    {Context5, ContId2InEndpoint} = next_in_endpoint(Context4, ContId2),
    {Context6, Cont2Eth} = next_eth(Context5, ContId2),
    Cont2IpAddr = ip_addr(Context6, CenId),
    maps_append(wires, [#{
        endID => ContId1InEndpoint,
        side => in,
        dest => #{
                    type => cont,
                    id => ContId1,
                    alias => Cont1Eth,
                    ip_address => Cont1IpAddr
                }
     },
     #{
        endID => ContId2InEndpoint,
        side => in,
        dest => #{
                    type => cont,
                    id => ContId2,
                    alias => Cont2Eth,
                    ip_address => Cont2IpAddr
                }
     }
    ], Context6);
wire_cen(Context, CenId, ContainerIds) ->
    lists:foldl(wire_cen_to_container(CenId), Context, ContainerIds).

wire_cen_to_container(CenId) ->
    fun(ContId, Context0) ->
        Context1 = count_cont(Context0, CenId),
        {Context2, InEndpoint} = next_in_endpoint(Context1, ContId),
        {Context3, OutEndpoint} = next_out_endpoint(Context2, ContId),
        {Context4, Eth} = next_eth(Context3, ContId),
        IpAddr = ip_addr(Context4, CenId),
        maps_append(wires, [#{
            endID => InEndpoint,
            side => in,
            dest => #{
                        type => cont,
                        id => ContId,
                        alias => Eth,
                        ip_address => IpAddr
                    }
         },
         #{
            endID => OutEndpoint,
            side => out,
            dest => #{
                        type => cen,
                        id => CenId
                    }
         }
        ], Context4)
    end.

% publish context helpers

% mark the next cen
count_cen(Context) ->
    {Context1, _} = next_count(Context, cen, fun(_) -> ok end),
    Context1.

% mark next container in cen
count_cont(Context, CenId) ->
    {Context1, _} = next_count(Context, {conts, CenId}, fun(_) -> ok end),
    Context1.

% get next eth port for a container
next_eth(Context, ContId) ->
    next_count(Context, {eth, ContId}, fun eth_name/1).

% get next inside port for a container
next_in_endpoint(Context, ContId) ->
    FormatFn = fun(N) -> in_endpoint_name(ContId, N) end,
    next_count(Context, {in_endpoint, ContId}, FormatFn).

% get next outside port for a container
next_out_endpoint(Context, ContId) ->
    FormatFn = fun(N) -> out_endpoint_name(ContId, N) end,
    next_count(Context, {out_endpoint, ContId}, FormatFn).

% helper
next_count(Context = #{count := CountMap}, Key, FormatFn) ->
    N = maps:get(Key, CountMap, 0),
    {maps:update(count, maps:put(Key, N + 1,  CountMap), Context),
     FormatFn(N)}.

% format ip addr
ip_addr(#{count := CountMap}, CenId) ->
    CenCount = maps:get(cen, CountMap),
    ContCount = maps:get({conts, CenId}, CountMap),
    leviathan_cin:ip_address(CenCount, ContCount).

% format ip addr for cens
cen_ip_addr(CenCount) ->
    leviathan_cin:cen_ip_address(CenCount).

% name formatters
eth_name(N) ->
    Nbinary = integer_to_binary(N),
    <<"eth", Nbinary/binary>>.

in_endpoint_name(ContId, N) ->
    endpoint_name(ContId, <<"i">>, N).

out_endpoint_name(ContId, N) ->
    endpoint_name(ContId, <<"o">>, N).

endpoint_name(ContId, Side, N) ->
    Nbinary = integer_to_binary(N),
    <<ContId/binary, $., Nbinary/binary, Side/binary>>.
