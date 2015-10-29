-module(leviathan_common_store).

-export([next_count/2]).

-include("leviathan.hrl").

%% -------------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------------

next_count(Key, InitialValue) ->
    Fn = fun() ->
                 case leviathan_db:read({counter, Key}) of
                     [] ->
                         update_count(Key, InitialValue + 1),
                         InitialValue;
                     [#counter{count = Count}] ->
                         update_count(Key, Count + 1),
                         Count
                 end
         end,
    leviathan_db:transaction(Fn).

%% -----------------------------------------------------------------------------
%% Local Functions
%% -----------------------------------------------------------------------------

update_count(Key, NewValue) ->
    leviathan_db:write(#counter{id = Key, count = NewValue}).
