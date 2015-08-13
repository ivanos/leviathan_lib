-module(leviathan_cen).

-compile(export_all).

-export([remove_container_from_cen/3,
        add_container_to_cen/3,
        destroy_cen/1,
        new_cen/1]).

-include("leviathan_logger.hrl").

%-------------------------------------------------------------------------------
% API
%-------------------------------------------------------------------------------

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

% XXX host is hardcoded
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
			  #{wire_type := CenType} = CenMap,
			  case CenType of
			      bus ->
				  #{cenID := CenId} = CenMap,
				  CmdBundle = leviathan_linux:new_bus(CenId),
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
                dest := #{type := cont, id := ContId, alias := Alias}}) ->
    CmdBundle = leviathan_linux:peer2cont(ContId, EndId, Alias),
    leviathan_linux:eval(CmdBundle).


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
