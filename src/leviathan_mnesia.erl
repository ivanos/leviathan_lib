-module(leviathan_mnesia).

-export([start/0, clear/0]).

-include("leviathan.hrl").

start() ->
    erl_mnesia:tables(tabledefs()).

clear() ->
    lists:foreach(
        fun({Name, _}) ->
            {atomic, ok} = mnesia:clear_table(Name)
        end,
    tabledefs()).

tabledefs() ->
    [
        {leviathan_cen,  [{attributes, record_info(fields, leviathan_cen)},
                          {disc_copies, [node()]},
                          {type, set}]},
        {leviathan_cen_cont, [{attributes, record_info(fields, leviathan_cen_cont)},
                              {disc_copies, [node()]},
                              {type, bag}]},
        {leviathan_cin,  [{attributes, record_info(fields, leviathan_cin)},
                          {disc_copies, [node()]},
                          {type, set}]},
        {leviathan_cin_cont, [{attributes, record_info(fields, leviathan_cin_cont)},
                              {disc_copies, [node()]},
                              {type, bag}]},
        {counter,        [{attributes, record_info(fields, counter)},
                          {disc_copies, [node()]},
                          {type, set}]}
    ].
