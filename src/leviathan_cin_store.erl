-module(leviathan_cin_store).

-export([import_cins/2,
         import_cins_in_cluster/2,
         get_levmap/1,
         update_cins/2]).

-define(LM_EMPTY, ?LM([], [])).
-define(LM(Cins, Conts), #{cins => Cins, conts => Conts}).

-define(MATCH_LM(Cins, Conts), #{cins := Cins, conts := Conts}).
-define(MATCH_LM_EMPTY, ?MATCH_LM([], [])).

-include("leviathan.hrl").

%% -------------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------------

import_cins_in_cluster(HostId, CinLM) ->
    [rpc:call(N, leviathan_cin, import_cins, [HostId, CinLM]) || N <- nodes()],
    import_cins(HostId, CinLM).

-spec import_cins(HostId :: string(), leviathan_cin:cin_lm()) -> ok.

import_cins(_, ?MATCH_LM_EMPTY) ->
    ok;
import_cins(_HostId, ?MATCH_LM(CinMaps, ContMaps)) ->
    Recs = create_cin_records(CinMaps)
        ++ create_cont_records(ContMaps),
    leviathan_db:transaction(fun() -> leviathan_db:write(Recs) end).


-spec get_levmap([CinId :: string()]) -> leviathan_cin:cin_lm().

get_levmap([]) ->
    ?LM([], []);
get_levmap(CinIds) ->
    Fn = fun() ->
                 {get_cin_records(CinIds), get_cont_records(CinIds)}
         end,
    {CinRecords, ContRecords} = leviathan_db:transaction(Fn),
    ?LM(create_cin_maps(CinRecords), create_cont_maps(ContRecords)).

update_cins(Host, Instructions0) ->
    ok.

%% -----------------------------------------------------------------------------
%% Local Functions: importing CIN maps
%% -----------------------------------------------------------------------------

create_cin_records(CinMaps) ->
    Fn = fun(#{cinID := CinId} = CinMap) ->
                 #leviathan_cin{cin = CinId,
                                data = maps:without([cinID], CinMap)}
         end,
    lists:map(Fn, CinMaps).

create_cont_records(ContMaps) ->
    Fn = fun(ContMap) ->
                 #leviathan_cin_cont{cont = maps:get(contID, ContMap),
                                     cin = maps:get(cinID, ContMap),
                                     addressing = maps:get(addressing,
                                                           ContMap)}
         end,
    lists:map(Fn, ContMaps).

%% -----------------------------------------------------------------------------
%% Local Functions: constructing CIN maps
%% -----------------------------------------------------------------------------

get_cin_records(CinIds) ->
    Fn = fun(Id) -> leviathan_db:read({leviathan_cin, Id}) end,
    lists:flatten(lists:map(Fn, CinIds)).

get_cont_records(CinIds) ->
    Fn = fun(Id) ->
                 leviathan_db:match_object(
                   #leviathan_cin_cont{cin = Id, _ = '_'})
         end,
    lists:flatten(lists:map(Fn, CinIds)).

create_cin_maps(CinRecords) ->
  lists:map(fun(#leviathan_cin{cin = CinId, data = CinData}) ->
                    maps:merge(#{cinID => CinId}, CinData)
            end, CinRecords).

create_cont_maps(ContMaps) ->
    Fn = fun(#leviathan_cin_cont{cont = ContID,
                                 cin = CinID,
                                 addressing = Addr}) ->
                 #{contID => ContID,
                   cinID => CinID,
                   addressing => Addr}
         end,
    lists:map(Fn, ContMaps).









