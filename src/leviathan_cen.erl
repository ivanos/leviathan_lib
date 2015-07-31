-module(leviathan_cen).

-compile(export_all).

-include("leviathan_logger.hrl").


-define(CENMAP1,#{"cenID" => "cen1","contIDs" => ["c1","c2","c3"]}).
-define(CENMAP2,#{"cenID" => "cen2","contIDs" => ["c1","c2"]}).
-define(CENMAP4,#{"cenID" => "cen4","contIDs" => ["c2","c4"]}).
-define(CENMAP5,#{"cenID" => "cen5","contIDs" => ["c2","c4","c5"]}).
-define(CENSMAP,#{"cens" => [?CENMAP1,?CENMAP2,?CENMAP4,?CENMAP5]}).


-define(CONT1,#{"contID" => "c1","cens" => [#{"cenID"=>"cen1","peerId"=>"eth0"},
					    #{"cenID"=>"cen2","peerId"=>"eth1"}]}).

-define(CONT2,#{"contID" => "c2","cens" => [#{"cenID"=>"cen1","peerId"=>unassigned},
					    #{"cenID"=>"cen2","peerId"=>unassigned},
					    #{"cenID"=>"cen4","peerId"=>unassigned},
					    #{"cenID"=>"cen5","peerId"=>unassigned}]}).

-define(CONT3,#{"contID" => "c3","cens" => [#{"cenID"=>"cen1","peerId"=>unassigned}]}).

-define(CONT4,#{"contID" => "c4","cens" => [#{"cenID"=>"cen4","peerId"=>unassigned},
					    #{"cenID"=>"cen5","peerId"=>unassigned}]}).

-define(CONT5,#{"contID" => "c5","cens" => [#{"cenID"=>"cen5","peerId"=>unassigned}]}).
-define(CONTSMAP,#{"conts"=>[?CONT1,?CONT2,?CONT3,?CONT4,?CONT5]}).


-spec import_cen_to_dobby(filename:filename_all()) -> ok | {error, Reason} when
      Reason :: term().

import_cen_to_dobby(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    import_cen_binary_to_dobby(Binary).

import_cen_binary_to_dobby(Binary) ->
    #{<<"cenList">> := Cens} = jiffy:decode(Binary, [return_maps]),
    ProcessedCens = process_cens(Cens),
    publsh_cens(ProcessedCens).


%
%
%  Create Erlang Maps
%
%   CenMap = #{"cenID" => "cen1","contIDs" => ["c1","c2","c3"]}.
%
%

prepare_cens_test()->
    prepare_cens(?CENSMAP).

prepare_cens(CensMap)->
    {ok,Cens} = maps:find("cens",CensMap),

    %%  2 passes are required sine the second pass assumes that
    %%  all busses have already be created so that every container  
    %%  must touched only once in the second pass
    %%
    %%  Pass 1: 
    %%     make any necessary Ethernet buses
    %%     if a Cen has more than 2 containers, we'll create a bus
    %%
    lists:foreach(fun(CenMap)->
			  case get_cen_type(CenMap) of
			      bus ->
				  {ok,CenId} = maps:find("cenID",CenMap),
				  CmdBundle = leviathan_linux:new_bus(CenId),
				  leviathan_linux:eval(CmdBundle);				      
			      _ -> ok %% don't create a bus
			  end
		  end, Cens),
    
    %%  Pass 2: 
    %%     configure containers for each Cen
    %%     and create any pipes for Cens with exactly 2 containers
    lists:foreach(fun(CenMap)->prepare_cen(CenMap) end, Cens).




get_cen_type(CenMap) ->
    {ok,ContIds} = maps:find("contIDs",CenMap),
    case length(ContIds) of
	0 -> null;			       
	1 -> null;
	2 -> pipe;
	_ -> bus
    end.
			      


prepare_cen(CenMap)->
    %% get container list
    {ok,ContIds} = maps:find("contIDs",CenMap),

    %% if there are only 2 containers in this Cen,
    %%
    case get_cen_type(CenMap) of
	null -> ok; %% don't do anything
	pipe -> ok; %% make a pipe
	bus -> 
	    %% iterate over containers and add to the bus
	    lists:foreach(fun(ContId)->
				  prepare_cont(ContId) end,ContIds)
    end.


prepare_cont(ContId)->

    % fake call needs to be replace with rea
    % maps navigation
    ContMap = get_cont(ContId),
    set_netns(ContId),

    {ok,Cens} = maps:find("cens",ContMap),
    Update = lists:foldl(fun(Cen,Acc)->
				 {CenUpdateAcc,PeerNum} = Acc,
				 CenUpdate = prepare_cont(ContId,Cen,PeerNum),
				 {CenUpdateAcc++[CenUpdate],PeerNum+1} end,
			 {[],0},Cens),
    NewContMap = maps:update("cens",Update,ContMap),

    %%=== CALL Lucet ===
    %%lucet:set_cont(NewConMap),
    
    %fake call
    set_cont(NewContMap).

prepare_cont(ContId,Cen,PeerNum)->
    {ok,CenId} = maps:find("cenID",Cen),
    {ok,PeerID} = maps:find("peerId",Cen),
    case PeerID of
	unassigned ->
	    create_peer(CenId,ContId,PeerNum),
	    maps:update("peerId",leviathan_linux:mk_lev_eth_name(PeerNum),Cen);
	_ ->
	    ?DEBUG("Peer already created! ContId = ~p, Cen = ~p, PeerNum = ~p",[ContId,Cen,PeerNum]),
	    Cen
    end.

set_netns(CenId)->
    CmdBundle = leviathan_linux:set_netns(CenId),
    leviathan_linux:eval(CmdBundle).

create_peer(CenId,ContId,PeerNum)->
    CmdBundle = leviathan_linux:new_peer(CenId,ContId,PeerNum),
    leviathan_linux:eval(CmdBundle).

    








%% Fake calls to lucet:get_cen(...) and lucet:get_cont(...) for development

get_cont("c1")->
    ?CONT1;
get_cont("c2") ->
    ?CONT2;
get_cont("c3") ->
    ?CONT3;
get_cont("c4")->
    ?CONT4;
get_cont("c5") ->
    ?CONT5.


set_cont(NewContMap)->ok.
    %%io:format("NewContMap = ~p",[NewContMap]).
    

%% Internal functions

process_cens(CensMap) ->
    lists:foldl(fun process_cen/2, {[], sets:new(), []}, CensMap).

process_cen(#{<<"cenID">> := CenId, <<"containerIDs">> := ContainerIds},
            {CenIds, ContainerIdsSet, CenLinks}) ->
    {[CenId | CenIds],
     sets:union(ContainerIdsSet, sets:from_list(ContainerIds)),
     [{CenId, C} || C <- ContainerIds] ++ CenLinks};
process_cen(_, _) ->
    throw(bad_json).

publsh_cens({CenIds, ContainerIds, CenLinks}) ->
    ToPublish = [lucet_dby:cen_ep(C) || C <-CenIds]
        ++ [lucet_dby:cen_container_ep(C) || C <- sets:to_list(ContainerIds)]
        ++ [lucet_dby:cen_to_container_link(L) || L <- CenLinks],
    lucet_dby:publish(<<"lucet_cn">>, ToPublish).
