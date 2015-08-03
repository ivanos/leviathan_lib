-module(leviathan_cen).

-compile(export_all).

-include("leviathan_logger.hrl").

-define(PUBLISHER, atom_to_binary(?MODULE, utf8)).

%% CEN to Container Map
%  5 CENs
%  This will define which, if any, bridges to construct
%
%

-define(CENMAP1,#{"cenID" => "cen1", "type"=>"bus", "contIDs" => ["c1","c2","c3"]}).   %% 3 wires
-define(CENMAP2,#{"cenID" => "cen2", "type"=>"wire", "contIDs" => ["c1","c2"]}).  %% 1 wire
-define(CENMAP4,#{"cenID" => "cen4", "type"=>"wire", "contIDs" => ["c2","c4"]}). %% 1 wire
-define(CENMAP5,#{"cenID" => "cen5", "type"=>"bus","contIDs" => ["c2","c4","c5"]}).  %% 3 wires
-define(CENSMAP,#{"cens" => [?CENMAP1,?CENMAP2,?CENMAP4,?CENMAP5]}).


%% Container to CEN Map
%% 5 Containers
%% This will define which, if any, Linux network namespaces to construct

-define(CONT1,#{"contID" => "c1","cens" => ["cen1","cen2"]}).  
-define(CONT2,#{"contID" => "c2","cens" => ["cen1","cen2","cen3","cen4"]}).
-define(CONT3,#{"contID" => "c3","cens" => ["cen1"]}).
-define(CONT4,#{"contID" => "c4","cens" => ["cen4","cen5"]}).
-define(CONT5,#{"contID" => "c5","cens" => ["cen5"]}).
-define(CONTSMAP,#{"conts"=>[?CONT1,?CONT2,?CONT3,?CONT4,?CONT5]}).

%% Wire Map
%% Total of 8 wires
%% This will define which, if any, Linux network veth peers to construct
%% and what to name them

%% 3 wires for cen1
-define(WIRE1,[#{"endID"=>"c1.0i",
		 "dest"=>#{"type"=>"cont",
			   "ID"=>"c1",
			   "alias"=>"eth0"}},
	       #{"endID"=>"c1.0o",
		 "dest"=>#{"type"=>"cen",
			   "ID"=>"cen1"}}]).
-define(WIRE2,[#{"endID"=>"c2.0i",
		 "dest"=>#{"type"=>"cont",
			   "ID"=>"c2",
			   "alias"=>"eth0"}},
	       #{"endID"=>"c2.0o",
		 "dest"=>#{"type"=>"cen",
			   "ID"=>"c1"}}]).
-define(WIRE3,[#{"endID"=>"c3.0i",
		 "dest"=>#{"type"=>"cont",
			   "ID"=>"c3",
			   "alias"=>"eth0"}},
	       #{"endID"=>"c3.0o",
		 "dest"=>#{"type"=>"cen",
			   "ID"=>"cen1"}}]).

%% 1 wire for cen2
-define(WIRE4,[#{"endID"=>"c1.1i",
		 "dest"=>#{"type"=>"cont",
			   "ID"=>"c1",
			   "alias"=>"eth1"}},
	       #{"endID"=>"c2.1i",
		 "dest"=>#{"type"=>"cont",
			   "ID"=>"c2",
			   "alias"=>"eth1"}}]).

%% 1 wire for cen4
-define(WIRE5,[#{"endID"=>"c2.2i",
		 "dest"=>#{"type"=>"cont",
			   "ID"=>"c2",
			   "alias"=>"eth2"}},
	       #{"endID"=>"c4.0i",
		 "dest"=>#{"type"=>"cont",
			   "ID"=>"c4",
			   "alias"=>"eth0"}}]).

%% 3 wires for cen5
-define(WIRE6,[#{"endID"=>"c2.3i",
		 "dest"=>#{"type"=>"cont",
			   "ID"=>"c2",
			   "alias"=>"eth3"}},
	       #{"endID"=>"c2.3o",
		 "dest"=>#{"type"=>"cen",
			   "ID"=>"cen5"}}]).
-define(WIRE7,[#{"endID"=>"c4.1i",
		 "dest"=>#{"type"=>"cont",
			   "ID"=>"c4",
			   "alias"=>"eth1"}},
	       #{"endID"=>"c4.1o",
		 "dest"=>#{"type"=>"cen",
			   "ID"=>"cen5"}}]).
-define(WIRE8,[#{"endID"=>"c5.0i",
		 "dest"=>#{"type"=>"cont",
			   "ID"=>"c5",
			   "alias"=>"eth0"}},
	       #{"endID"=>"c5.1o",
		 "dest"=>#{"type"=>"cen",
			   "ID"=>"cen5"}}]).
-define(WIREMAP,#{"wires"=>[?WIRE1,?WIRE2,?WIRE3,?WIRE4,?WIRE5,?WIRE6,?WIRE7,?WIRE8]}).

-define(LEVMAP,#{"censmap"=>?CENSMAP,"contsmap"=>?CONTSMAP,"wiremap"=>?WIREMAP}).





-define(CENSLIST, [<<"cen1">>,<<"cen2">>,<<"cen3">>,<<"cen4">>,<<"cen5">>]).
-define(TESTDATA, <<"{\"cenList\":
 [{
     \"cenID\" : \"cen1\",
     \"containerIDs\" : [ \"c1\",\"c2\",\"c13\",\"c14\"]
  },
  {
      \"cenID\":\"cen2\",
      \"containerIDs\":[\"c4\",\"c5\",\"c6\",\"c7\"]
  },
  {
      \"cenID\":\"cen3\",
      \"containerIDs\":[\"c15\",\"c16\",\"c9\",\"c10\",\"c11\"]
  },
  {
      \"cenID\":\"cen4\",
      \"containerIDs\":[\"c11\",\"c12\"]
  },
  {
      \"cenID\":\"cen5\",
      \"containerIDs\":[\"c2\",\"c3\"]
  }]
}">>).

% functions for demos
%test_import() ->
%    import_cen_binary_to_dobby(?TESTDATA),
%    test_add_peer_id(<<"c1">>,<<"cen1">>,<<"eth0">>),
%    test_add_peer_id(<<"c1">>,<<"cen2">>,<<"eth1">>).

%-define(CENIDS, ["cen1","cen2","cen4","cen5"]).

%test_prepare_cens() ->
%    CensMaps = lists:map(
%        fun(CenId) ->
%            lucet_dby:get_cen(CenId)
%        end, ?CENIDS),
%    prepare_cens(CensMaps).


test_local_prepare_lev()->
    prepare_lev(?LEVMAP).

test_add_peer_id(Container, Cen, PeerId) ->
    ok = dby:publish(?PUBLISHER, {Container, Cen, [{<<"peerId">>, PeerId}]}, [persistent]).

% exports

%-spec import_cen_to_dobby(filename:filename_all()) -> ok | {error, Reason} when
%      Reason :: term().
%import_cen_to_dobby(Filename) ->
%    {ok, Binary} = file:read_file(Filename),
%    import_cen_binary_to_dobby(Binary).

%import_cen_binary_to_dobby(Binary) ->
%    #{<<"cenList">> := Cens} = jiffy:decode(Binary, [return_maps]),
%    ProcessedCens = process_cens(Cens),
%    publsh_cens(ProcessedCens).



%
% Top Level Processor
%
prepare_lev(LevMap)->
    #{ "censmap" := CensMap,
       "contsmap" := ContsMap,
       "wiremap" := WireMap } = LevMap,
    prepare_cens(CensMap),
    prepare_conts(ContsMap),
    prepare_wires(WireMap).

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
    #{ "cens" := Cens } = CensMap,

    %%     make any necessary Ethernet buses
    %%     if a Cen has more than 2 containers, we'll create a bus
    %%
    lists:foreach(fun(CenMap)->
			  #{"type" := CenType} = CenMap,
			  case CenType of
			      "bus" ->
				  #{"cenID" := CenId} = CenMap,
				  CmdBundle = leviathan_linux:new_bus(CenId),
				  leviathan_linux:eval(CmdBundle);				      
			      _ -> ok %% don't create a bus
			  end
		  end, Cens).
    
prepare_conts(ContsMap)->
    #{ "conts" := Conts } = ContsMap,
    lists:foreach(fun(Cont)->
			  #{"contID" := ContId } = Cont,
			  CmdBundle = leviathan_linux:set_netns(ContId),
			  leviathan_linux:eval(CmdBundle)
		  end,Conts).

prepare_wires(WireMap)->
    #{ "wires" := Wires } = WireMap,
    lists:foreach(fun(Wire)->prepare_wire(Wire) end,Wires).

prepare_wire(Wire) when length(Wire) == 2 ->
    [End1,End2] = Wire,
    #{ "endID" := EndId1 } = End1, 
    #{ "endID" := EndId2 } = End2,
    CmdBundle = leviathan_linux:new_peer(EndId1,EndId2),
    leviathan_linux:eval(CmdBundle),				      
    lists:foreach(fun(WireEnd)->
			 prepare_wire_end(WireEnd) end,Wire).

prepare_wire_end(WireEnd)->
    #{ "endID" := EndId,
       "dest" := Dest } = WireEnd,
    #{ "type" := DestType } = Dest,
    case DestType of
	"cen" -> 
	    #{ "ID" := CenId } = Dest,
	    CmdBundle = leviathan_linux:peer2cen(CenId,EndId),
	    leviathan_linux:eval(CmdBundle);				      
	"cont" ->
	    #{ "ID" := Cid } = Dest,
	    #{ "alias" := Alias} = Dest,
	    CmdBundle = leviathan_linux:peer2cont(Cid,EndId,Alias),
	    leviathan_linux:eval(CmdBundle)
    end.

	    

publsh_cens({CenIds, ContainerIds, CenLinks}) ->
    ToPublish = [lucet_dby:cen_ep(C) || C <-CenIds]
        ++ [lucet_dby:cen_container_ep(C) || C <- sets:to_list(ContainerIds)]
        ++ [lucet_dby:cen_to_container_link(L) || L <- CenLinks],
    lucet_dby:publish(<<"lucet_cn">>, ToPublish).
