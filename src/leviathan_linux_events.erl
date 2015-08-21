-module(leviathan_linux_events).

-compile(export_all).

-export([print_event/1,test_ip_monitor/0]).
-export([ip_monitor/1]).

-include("leviathan_logger.hrl").

% == API ==
%
% Defined events
%    #{ event => set_address, 
%       interface => Alias, 
%       ipaddr => IP,
%       netmask => NetMask };
%
% Sample user defined ip_monitor event handlers
%
%

%
% simple handler to print the whole event
%
print_event(Event)->
    io:format("Event:~p~n",[Event]).

%
% matches "set_address" events
% extracts the IP address and prints it
%
handle_new_ip(Event)->
    case Event of
	#{ event := set_address,
	   ipaddr := IP} ->
	    io:format("IP Address = ~p~n",[IP]);
	_ -> ignore_event
    end.

%
% matches "set address" events where 
% alias (i.e. interface name) is "cin2"
% and prints a message
%
configure_cin2(Event)->
    case Event of
	#{ event := set_address,
	   alias := "cin2",
	   ipaddr := IP} ->
	    io:format("cin2 IP Address = ~p~n",[IP]);
	_ -> ignore_event
    end.


%
% Test call to ip_monitor passing it three user defined event handlers: 
% print_event(Event) and handle_new_ip(Event) 
%
test_ip_monitor()->
    ip_monitor([fun print_event/1,fun handle_new_ip/1,fun configure_cin2/1]),
    leviathan_tester:gen_ip_events().


%
% Spawn Erlang process to listen to "ip monitor"
%
ip_monitor(EventHandlers) when is_list(EventHandlers)->
    IPMonitorBin = "ip monitor",
    spawn(fun()->Port = open_port({spawn, IPMonitorBin}, []),
    		      loop(Port,EventHandlers) end).

%
% ============ Library Implementation ===============
%		       

%
% 1. Recieve the output of "ip monitor"
% 2. Parse the Events
% 3. Call the EventHanlders
%
loop(Port,EventHandlers) when is_list(EventHandlers)->
    receive
	{Port, {data, Data}} ->
	    %io:format("events received: ~p~n",[Data]),
	    Events = parse_event(Data),
	    %io:format("events: ~p~n",[Events]),
	    parse_events(EventHandlers,Events),
	    %handle_event(Mapped),
	    loop(Port,EventHandlers);
	{'EXIT', Port, Reason} ->
	    exit({port_terminated,Reason})
    end.



parse_events(EventHandlers,Events) when is_list(EventHandlers)->
    lists:foreach(fun(Event)->
			  Parsed = parse_line(Event),
			  %io:format("parsed: ~p~n",[Parsed]),
			  case event2map(Parsed) of
			      nomatch -> 	
				  %io:format("nomatch~n");
				  ok;
			      EventMap ->
				  handle_event(EventHandlers,EventMap)
			  end end,Events).


handle_event(EventHandlers,Event) when is_list(EventHandlers)->
    lists:map(fun(EventHandler)->case is_function(EventHandler) of
				     true->apply(EventHandler,[Event]);
				     false -> bad_handler 
				 end end,EventHandlers).

parse_event(EventString)->
    Tokens = string:tokens(EventString,"\n"),
    lists:map(fun(Token)->string:strip(Token) end,Tokens).


parse_line(EventLine)->
	Stripped = string:strip(EventLine,right,$\n),
	Tokens = string:tokens(Stripped," "),
	lists:map(fun(Token)->T1=string:strip(Token,right,$:),
			      string:strip(T1,right,$\n) end,
		  Tokens).

event2map([_,_Name,"inet",FQIP,"scope","global",Alias])->
    [IP,NetMask] = string:tokens(FQIP,"/"),
    #{ event => set_address, 
       interface => Alias, 
       ipaddr => IP,
       netmask => "/" ++ NetMask };
event2map(_) ->
    nomatch.

    
	
