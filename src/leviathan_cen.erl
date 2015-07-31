-module(leviathan_cen).

-compile(export_all).

-include("leviathan_logger.hrl").

-define(CENMAP,#{"cenID" => "cen1","contIDs" => ["c1","c2","c3"]}).
-define(CONT1,#{"contID" => "c1","cens" => [#{"cenID"=>"cen1","peerId"=>"Eth0"},
					    #{"cenID"=>"cen2","peerId"=>"Eth1"}]}).

-define(CONT2,#{"contID" => "c2","cens" => [#{"cenID"=>"cen1","peerId"=>unassigned},
					    #{"cenID"=>"cen4","peerId"=>unassigned},
					    #{"cenID"=>"cen5","peerId"=>unassigned}]}).

-define(CONT3,#{"contID" => "c3","cens" => [#{"cenID"=>"cen1","peerId"=>unassigned}]}).


%
%
%  Create Erlang Maps
%
%   CenMap = #{"cenID" => "cen1","contIDs" => ["c1","c2","c3"]}.
%
%
prepare_cen(CenId)->
    %%CenMap = lucet:get_cen(CenId),
    CenMap = get_cen("cen1"),
    {ok,ContIds} = maps:find("contIDs",CenMap),
    lists:foreach(fun(ContId)->
			  prepare_cont(ContId) end,ContIds).

prepare_cont(ContId)->
    ContMap = get_cont(ContId),
    {ok,Cens} = maps:find("cens",ContMap),
    lists:foldl(fun(Cen,Acc)->
			prepare_cont(ContId,Cen,Acc), Acc+1 end,0,Cens).

prepare_cont(ContId,Cen,PeerNum)->
    {ok,CenId} = maps:find("cenID",Cen),
    {ok,PeerID} = maps:find("peerId",Cen),
    case PeerID of
	unassigned ->
	    create_peer(ContId,PeerNum);
	_ ->
	    ?DEBUG("Peer already created! ContId = ~p, Cen = ~p, PeerNum = ~p",[ContId,Cen,PeerNum])
    end.


create_peer(ContId,PeerNum)->
    CmdBundle = leviathan_linux:new_peer(ContId,PeerNum),
    leviathan_linux:eval(CmdBundle).

    








%% Fake calls to lucet:get_cen(...) and lucet:get_cont(...) for development

get_cen("cen1")->
    ?CENMAP.

get_cont("c1")->
    ?CONT1;
get_cont("c2") ->
    ?CONT2;
get_cont("c3") ->
    ?CONT3.



    



			 
