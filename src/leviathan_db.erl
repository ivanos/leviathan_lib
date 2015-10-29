-module(leviathan_db).

-export([start/0,
         transaction/1,
         abort/1,
         write/1,
         read/1,
         select/2,
         exists/1,
         delete/1,
         match_object/1,
         delete_object/1,
         foldl/3,
         clear/0]).

start() ->
    dby_mnesia:start().

transaction(Fn) ->
    case mnesia:transaction(Fn) of
        {aborted, Reason} ->
            throw({mnesia, Reason});
        {atomic, Ret} ->
            Ret
    end.

abort(Reason) ->
    mnesia:abort(Reason).
    % does not return

write(Records) when is_list(Records) ->
    ok = lists:foreach(fun(Record) -> write(Record) end, Records);
write(Record) ->
    ok = mnesia:write(Record).

read(Key) ->
    mnesia:read(Key).

select(Table, MatchSpec) ->
    mnesia:select(Table, MatchSpec).

exists(Key) ->
    [] /= mnesia:read(Key).

delete(Key) ->
    ok = mnesia:delete(Key).

match_object(Record) ->
    mnesia:match_object(Record).

delete_object(Record) ->
    ok = mnesia:delete_object(Record).

foldl(Fn, Acc, Table) ->
    mnesia:foldl(Fn, Acc, Table).

clear() ->
    leviathan_mnesia:clear().
