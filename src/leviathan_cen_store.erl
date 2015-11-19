-module(leviathan_cen_store).

-export([import_cens/2,
         get_levmap/1,
         update_cens/2]).

-define(LM_EMPTY, #{censmap := #{cens := []},
                    contsmap := #{conts := []},
                    wiremap := #{wires := []}
                   }).

-define(LM(Cens, Conts, Wires),#{censmap => #{cens => Cens},
                                 contsmap => #{conts => Conts},
                                 wiremap => #{wires => Wires}
                                }).

-include_lib("stdlib/include/ms_transform.hrl").
-include("leviathan.hrl").

%% -------------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------------

import_cens(_, ?LM_EMPTY) ->
    ok;
import_cens(Host, LM) ->
    Records = lists:flatten([create_cen_records(Host, LM),
                             create_cont_records(Host, LM)]),
    ok = leviathan_db:transaction(
           fun() -> leviathan_db:write(Records) end).

get_levmap([]) ->
    ?LM([], [], []);
get_levmap(CenIds) ->
    Cens = get_cens(CenIds),
    ?LM(Cens, get_conts(Cens), get_wiremap(CenIds)).

update_cens(Host, Instructions0) ->
    Instructions = filter_and_sort_update_instructions(Instructions0),
    Fns = lists:map(
            fun(Instruction) ->
                    update_instruction(Host, Instruction)
            end, Instructions),
    Fn = fun() ->
        lists:foreach(fun(Fn) -> Fn() end, lists:flatten(Fns))
    end,
    ok = leviathan_db:transaction(Fn).

%% -----------------------------------------------------------------------------
%% Local Functions: importing cens
%% -----------------------------------------------------------------------------

create_cen_records(Host, #{censmap := #{cens := Cens},
                           wiremap := #{wires := Wires}}) ->
    GroupedWires = group_wires_by_cen(Wires),
    Fun = fun(Cen) ->
                  cen_record(Host, Cen, GroupedWires)
          end,
    lists:map(Fun, Cens).

group_wires_by_cen(Wires) ->
    lists:foldl(fun group_wires_by_cen/2, #{}, Wires).

group_wires_by_cen(Wire = [
                           #{dest := #{alias := CenId}} = _InEndpoint,
                           #{dest := #{id := CenId}} = _OutEndpoint
                          ], Acc) ->
    CenWires = maps:get(CenId, Acc, []),
    maps:put(CenId, [Wire | CenWires], Acc).

cen_record(_Host, #{cenID := CenId,
                    wire_type := WireType,
                    contIDs := ContIds},
           Wires) ->
    #leviathan_cen{cen = CenId,
                   data = #{contIDs => ContIds,
                            wire_type => WireType},
                   wires = maps:get(CenId, Wires, [])}.

create_cont_records(Host, #{contsmap := #{conts := Conts}}) ->
    lists:map(fun(Cont) -> cont_record(Host, Cont) end, Conts).

cont_record(_Host, #{contID := ContId,
                     cens := CenIds,
                     reservedIdNums := Ids}) ->
    lists:map(
      fun({CenId, IdNumber}) ->
              #leviathan_cen_cont{
                 cont = ContId,
                 cen = CenId,
                 idnumber = IdNumber}
      end, lists:zip(CenIds, Ids)).

%% -----------------------------------------------------------------------------
%% Local Functions: constructing leviathan map
%% -----------------------------------------------------------------------------

get_cens(CenIds) ->
    lists:reverse(valid_list(fun get_cen/1, CenIds)).

get_cen(CenId) ->
    Fn = fun() ->
                 case leviathan_db:read({leviathan_cen, CenId}) of
                     [] -> not_found;
                     [CenRecord] -> cen_map(CenRecord)
                 end
         end,
    leviathan_db:transaction(Fn).

%% convert a cen record to a cen map
cen_map(#leviathan_cen{cen = CenId, data = #{contIDs := ContIds,
                                             wire_type := WireType}}) ->
    #{cenID => CenId, wire_type => WireType, contIDs => ContIds}.

get_conts(Cens) ->
    ContIds = contids_from_cens(Cens),
    Fun = fun(ContId, ContsAcc) ->
                  case get_cont("host1", ContId) of
                      not_found ->
                          ContsAcc;
                      Cont ->
                          [Cont | ContsAcc]
                  end
          end,
    lists:foldl(Fun, [], ContIds).

%% make a list of unique container ids by inspecting the cens
contids_from_cens(Cens) ->
    sets:to_list(lists:foldl(
                   fun(#{contIDs := ContIds}, Acc) ->
                           sets:union(Acc, sets:from_list(ContIds))
                   end, sets:new(), Cens)).

get_cont(_Host, ContId) ->
    Fn = fun() ->
                 case leviathan_db:read({leviathan_cen_cont, ContId}) of
                     [] ->
                         not_found;
                     ContRecords ->
                         cont_map(ContId, ContRecords)
                 end
         end,
    leviathan_db:transaction(Fn).

%% convert a list of container records into a cont map
cont_map(ContId, ContRecords) ->
    Fun = fun(#leviathan_cen_cont{cont = CId, cen = CenId, idnumber = Id},
              #{cens := Cens, reservedIdNums := Ids} = ContMap)
                when CId == ContId ->
                  ContMap#{cens := Cens ++ [CenId],
                           reservedIdNums := Ids ++ [Id]}
          end,
    lists:foldl(Fun,
                #{contID => ContId, cens => [], reservedIdNums => []},
                ContRecords).

%% get_wires/1 returns the list of wires per Cen. Flatten
%% the list with lists:append/1 rather than with lists:flatten/1 because
%% the wires themselves are lists.
get_wiremap(CenIds) ->
    lists:append([get_wires(CenId) || CenId <- CenIds]).

get_wires(CenId) ->
    Fn = fun() ->
                 case leviathan_db:read({leviathan_cen, CenId}) of
                     [] -> [];
                     [CenRecord] -> wire_map(CenRecord)
                 end
         end,
    leviathan_db:transaction(Fn).

wire_map(#leviathan_cen{wires = Wires}) ->
    Wires.

%% -----------------------------------------------------------------------------
%% Local Functions: updating cens
%% -----------------------------------------------------------------------------

%% @doc Filter and sort updated instructions before constructing db updates.
%%
%% All the {_, cont, _} instructions ale filtered as they duplicate information
%% contained in {_, cont_in_cen, _} ones. {add, cen, _} instructions is
%% modified so that the `contIDs' field is empty as {add, cont_in_cen, _}
%% update the appropriate #leviathan_cen record,
%%
%% The instructions are sorted according to the following rules:
%% * {add, _, _} before any {destroy, _, _}
%% * {add, cen, _} before any other {add, _, _}
%% * {add, wire, _} after any other {add, _, _}
%% * {destroy, cont_in_ceny, _} before any other {destroy, _, _}
%% * {destroy, wire, _} after any other {destroy, _, _}.
filter_and_sort_update_instructions(Instructions0) ->
    Instructions1 = lists:filtermap(fun({add, cen, CenMap}) ->
                                            {true, {add, cen,
                                                    CenMap#{contIDs => []}}};
                                       ({_, cont, _}) ->
                                            false;
                                       (_) ->
                                            true
                                    end, Instructions0),
    lists:sort(fun({add, _AllOther, _}, {add, wire, _}) ->
                       true;
                  ({add, cen, _}, {add, _AllOther, _}) ->
                       true;
                  ({add, _, _}, {destroy, _, _}) ->
                       true;
                  ({destroy, _AllOther, _}, {destroy, wire, _}) ->
                       true;
                  ({destroy, cont_in_cen, _}, {destroy, _AllOther, _}) ->
                       true;
                  (_, _) ->
                       false
               end, Instructions1).

update_instruction(Host, {add, cen, CenMap = #{contIDs := []}}) ->
    write_fn(cen_record(Host, CenMap));
update_instruction(Host, {add, cont_in_cen, {ContMap = #{contID := ContId},
                                               CenMap = #{cenID := CenId}}}) ->
    [
     update_fn({leviathan_cen, CenId},
               fun(#leviathan_cen{data = Data0} = R) ->
                       #{contIDs := Conts} = Data0,
                       Data1 = Data0#{contIDs => [ContId | Conts]},
                       R#leviathan_cen{data = Data1}
               end),
     write_fn(cont_record2(Host, ContMap, CenMap))
    ];
update_instruction(_Host, {add, wire, Wire}) ->
    CenId = cenid_from_wire(Wire),
    update_fn({leviathan_cen, CenId},
              fun(CenRecord = #leviathan_cen{wires = Wires}) ->
                      CenRecord#leviathan_cen{wires = [Wire | Wires]}
              end);
update_instruction(_, {add, bridge, _}) ->
    %% not used
    [];
update_instruction(_, {destroy, cen, #{cenID := CenId}}) ->
    delete_fn({leviathan_cen, CenId});
update_instruction(_, {destroy, cont_in_cen, {#{contID := ContId},
                                              #{cenID := CenId}}}) ->
    [
     update_fn({leviathan_cen, CenId},
               fun(CenRecord = #leviathan_cen{data = Data0}) ->
                       #{contIDs := ContIds} = Data0,
                       Data1 = Data0#{contIDs := ContIds -- [ContId]},
                       CenRecord#leviathan_cen{data = Data1}
               end),
     delete_record_fn({leviathan_cen_cont, ContId},
                      fun(ContRecords) ->
                              [Record | _] = lists:dropwhile(
                                               fun(#leviathan_cen_cont{cen = C}) ->
                                                       C =/= CenId
                                               end, ContRecords),
                              Record
                      end)
    ];
update_instruction(_, {destroy, wire, Wire}) ->
    CenId = cenid_from_wire(Wire),
    update_fn({leviathan_cen, CenId},
              fun(CenRecord = #leviathan_cen{wires = Wires}) ->
                      CenRecord#leviathan_cen{wires = Wires -- [Wire]}
              end);
update_instruction(_, {destroy, bridge, _}) ->
    %% not used
    [];
update_instruction(_, {set, wire_type, _}) ->
    %% not used
    [].

write_fn(Record) ->
    fun() -> leviathan_db:write(Record) end.

delete_fn(Key) ->
    fun() -> leviathan_db:delete(Key) end.

delete_record_fn(Key, SelectFn) ->
    fun() ->
        Records = leviathan_db:read(Key),
        Record = SelectFn(Records),
        leviathan_db:delete_object(Record)
    end.

update_fn(Key, UpdateFn) ->
    fun() ->
        [Record0] = leviathan_db:read(Key),
        Record1 = UpdateFn(Record0),
        leviathan_db:write(Record1)
    end.

cenid_from_wire(Wire) ->
    OutEndpoint = out_endpoint(Wire),
    #{dest := #{type := cen, id := CenId}} = OutEndpoint,
    CenId.

out_endpoint([Out = #{side := out}, _]) ->
    Out;
out_endpoint([_, Out = #{side := out}]) ->
    Out.

cont_id_number(CenId, #{cens := Cens, reservedIdNums := Ids}) ->
    [{CenId, Id} | _] = lists:dropwhile(fun({CId, _Id}) ->
                                                CId =/= CenId
                                        end, lists:zip(Cens, Ids)),
    Id.

cen_record(_, #{cenID := CenId,
                wire_type := WireType,
                contIDs := ContIds}) ->
                #leviathan_cen{cen = CenId,
                               data = #{contIDs => ContIds,
                                        wire_type => WireType},
                               wires = []}.

cont_record2(_, ContMap = #{contID := ContId}, #{cenID := CenId}) ->
    #leviathan_cen_cont{cont = ContId,
                        cen = CenId,
                        idnumber = cont_id_number(CenId, ContMap)}.

%% -----------------------------------------------------------------------------
%% Local Functions: helpers
%% -----------------------------------------------------------------------------

valid_list(GetFn, Keys) ->
    lists:foldl(
        fun(Key, Acc) ->
            case GetFn(Key) of
                not_found -> Acc;
                Cont -> [Cont | Acc]
            end
        end, [], Keys).
