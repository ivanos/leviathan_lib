-module(leviathan_docker_events).

-include("leviathan_logger.hrl").

-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_link/0,
         get_events/1,
         now_secs/1
        ]).

%% ===================================================================
%% API
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ===================================================================
%% Shell utilities
%% ===================================================================

get_events(Time)->
    Cmd = "echo \"GET /events?since=" ++ integer_to_list(Time) ++ " HTTP/1.1\r\n\" | nc -U /var/run/docker.sock",
    Results = leviathan_os:cmd(Cmd),
    Tokens = string:tokens(Results,"\r\n"),
    lists:foldl(fun(Str,Acc)->case string:substr(Str,1,1) of 
				  "{" -> Acc ++ Str;
				  _ -> Acc 
			      end end,[],Tokens).

now_secs({MegaSecs,Secs,_}) ->
    MegaSecs*1000000 + Secs.

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    DockerBin = application:get_env(leviathan_lib, docker_bin,
                                "/usr/bin/docker events --until=\"\""),
    gen_server:cast(self(), start),
    {ok, #{port => undefined, docker_bin => DockerBin}}.

handle_call(Request, _From, State) ->
    {stop, {not_implemented, Request}, State}.
    
handle_cast(start, State = #{docker_bin := DockerEventsBin}) ->
    Port = open_port({spawn, DockerEventsBin}, [exit_status]),
    {noreply, State#{port := Port}};
handle_cast(Request, State) ->
    {stop, {not_implemented, Request}, State}.

handle_info({'EXIT', Port, PosixCode}, State = #{port := Port}) ->
    {stop, {port_error, PosixCode}, State#{port := undefined}};
handle_info({Port, {exit_status, Status}}, State = #{port := Port}) ->
    {stop, {port_error, exit_status, Status}, State#{port := undefined}};
handle_info({Port, {data, Data}}, State = #{port := Port}) ->
    Parsed = parse_event(Data),
    Mapped = event2map(Parsed),
    handle_event(Mapped),
    {norpely, State};
handle_info(Message, State) ->
    {stop, {not_implemtned, Message}, State}.

terminate(_Reason, _State) ->
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

parse_event(EventString)->
    Stripped = string:strip(EventString, right, $\n),
    Tokens = string:tokens(Stripped," "),
    lists:map(
        fun(Token) ->
            T1 = string:strip(Token, right, $)),
            T2 = string:strip(T1, right, $:),
            string:strip(T2, left, $()
        end, Tokens).

event2map([Timestamp, Cid, "from", Tag, Event]) ->
    #{event => list_to_atom(Event),
      cid => string:substr(Cid, 1, 12),
      tag => Tag,
      time => Timestamp
    };
event2map(_) ->
    #{event => undefined}.

handle_event(#{event := create, cid := Cid})->
    ?DEBUG("docker event: create cid(~p)", [Cid]),
    Cins = leviathan_utils:container_get_cins(Cid),
    case Cins of
	nomatch ->
            ok;
	_ ->
            lists:foreach(
                fun(Cin)->
                    leviathan_cen:add_container_to_cen("host1",
                                                       Cid,
                                                       binary_to_list(Cin))
                end, Cins)
    end;
handle_event(#{event := die, cid := Cid})->
    ?DEBUG("docker event: die cid(~p)", [Cid]),
    Cins = leviathan_utils:container_get_cins(Cid),
    case Cins of 
	nomatch ->
            ok;
	_->
	    lists:foreach(
                fun(_Cin)->
                  % leviathan_cen:remove_container_from_cen("host1",
                  %                                         Cid,
                  %                                         binary_to_list(Cin))
                    ok
                end, Cins)
    end;
handle_event(#{event := Type, cid := Cid}) ->
    ?DEBUG("docker event: ~p cid(~p)", [Type, Cid]);
handle_event(Event) ->
    ?DEBUG("unknown docker event: ~p", [Event]).
