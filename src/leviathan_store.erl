-module(leviathan_store).

-export([import_cens/2,
         get_levmap/1,
         update_cens/2,
         next_count/2,
         add_cen/2,
         add_container/2,
         get_cen_ip/1,
         get_ips_from_cen/1,
         get_ids_from_cont/1,
         get_wire_data/2,
         wire_cont_to_cen/3]).

-define(LM_EMPTY, #{censmap := #{cens := []},
                    contsmap := #{conts := []},
                    wiremap := #{wires := []}
                   }).

-include("leviathan.hrl").

import_cens(_, ?LM_EMPTY) ->
    ok;
import_cens(Host, LM) ->
    {CensRecords, ContsIpsMap} = cens_and_cont_ips_from_lm(Host, LM),
    Records = [CensRecords,
               containers_from_lm(Host, ContsIpsMap, LM)],
    Fn = fun() ->
        ok = leviathan_db:write(lists:flatten(Records))
    end,
    ok = leviathan_db:transaction(Fn).

get_levmap(CenIds) ->
    Cens = get_cens(CenIds),
    {Conts, ContsIpsMap} =  get_conts_and_their_ips(Cens),
    #{censmap => #{cens => fill_reserved_ips_in_cens(Cens, ContsIpsMap)},
      contsmap => #{conts => Conts},
      wiremap => #{wires => get_wiremap(CenIds)}
    }.

fill_reserved_ips_in_cens(Cens, ContsIpsMap) ->
    Fun = fun(#{cenID := CenId, contIDs := ContIds} = Cen) ->
                  ReservedIps = lists:foldl(
                                  mk_conts_ips_in_cen_fun(CenId, ContsIpsMap),
                                  [],
                                  ContIds),
                  Cen#{reservedIps => lists:reverse(ReservedIps)}
          end,
    lists:map(Fun, Cens).

mk_conts_ips_in_cen_fun(CenId, IpsMap) ->
    fun(ContId, ResrvedIps) ->
            [maps:get({CenId, ContId}, IpsMap) | ResrvedIps]
    end.

update_cens(Host, Instructions) ->
    Fn = fun() ->
        Records = lists:map(
            fun(Instruction) -> update_instruction(Host, Instruction) end,
            Instructions),
        ok = leviathan_db:write(lists:flatten(Records))
    end,
    ok = leviathan_db:transaction(Fn).

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

add_cen(CenId, Ip) ->
    Fn = fun() ->
                 CenData = #{contIDs => [], wirte_type => bus,
                             ipaddr => binary_to_list(Ip)},
                 Cen = #leviathan_cen{cen = CenId, data = CenData},
                 ok = leviathan_db:write(Cen)
         end,
    ok = leviathan_db:transaction(Fn).


%% add container to authoritative data
add_container(CenId, ContId) ->
    CenIp = get_cen_ip(CenId),
    UsedIps = get_ips_from_cen(CenId),
    Ip = leviathan_cin:ip_address(cen_b(CenIp), UsedIps),
    UsedIds = get_ids_from_cont(ContId),
    Id = gen_container_id_number(ContId, UsedIds),
    Fn = fun() ->
                 ContData = #{idnumber => Id, ip_address => Ip},
                 Cont = #leviathan_cont{cont = ContId, cen = CenId,
                                        data = ContData},
                 ok = leviathan_db:write(Cont)
         end,
    ok = leviathan_db:transaction(Fn).

get_cen_ip(CenId) ->
    MatchHead = #leviathan_cen{cen = CenId, data = '$1', _ = '_'},
    MatchSpec = [{MatchHead, _Guard = [], _Result = ['$1']}],
    Fn = fun() ->
                 [#{ipaddr := Ip}] = leviathan_db:select(leviathan_cen, MatchSpec),
                 Ip
         end,
    leviathan_db:transaction(Fn).


get_ips_from_cen(CenId) ->
    MatchHead = #leviathan_cont{cen = CenId, data = '$1', _ = '_'},
    MatchSpec = [{MatchHead,
                  _Guard = [],
                  _Result = [#{ip_address => '$1'}]
                 }],
    Fn = fun() ->
                 leviathan_db:select(leviathan_cont, MatchSpec)
         end,
    lists:foldl(fun(#{ip_address := Ip}, Acc) ->
                        [Ip | Acc]
                end, [], leviathan_db:transaction(Fn)).

get_ids_from_cont(ContId) ->
    MatchHead = #leviathan_cont{cont = ContId, data = '$1', _ = '_'},
    MatchSpec = [{MatchHead, _Guard = [], _Result = ['$1']}],
    Fn = fun() ->
                 leviathan_db:select(leviathan_cont, MatchSpec)
         end,
    lists:foldl(fun(#{idnumber := Id}, Acc) ->
                        [Id | Acc];
                   (_, Acc) ->
                        Acc
                end, [], leviathan_db:transaction(Fn)).

get_wire_data(CenId, ContId) ->
    MatchHead = #leviathan_cont{cen = CenId, cont = ContId, data = '$1', _ = '_'},
    MatchSpec = [{MatchHead, _Guard = [], _Result = ['$1']}],
    Fn = fun() ->
                 [Data] = leviathan_db:select(leviathan_cont, MatchSpec),
                 Data
         end,
    leviathan_db:transaction(Fn).

wire_cont_to_cen(CenId, ContId, Wire) ->
    Fn =
        fun() ->
                CenKey = {leviathan_cen, CenId},
                [#leviathan_cen{data = Data0, wires = Wires0}] = leviathan_db:read(CenKey),
                #{contIDs := ContIds} = Data0,
                Wires1 = update_leviathan_cen_wires(Wire, Wires0),
                Data1 = Data0#{contIDs => [ContId | ContIds]},
                leviathan_db:write(#leviathan_cen{cen = CenId,
                                                  data = Data1,
                                                  wires = Wires1})
        end,
    ok = leviathan_db:transaction(Fn).



% -----------------------------------------------------------------------------
%
% local functions
%
% -----------------------------------------------------------------------------

update_count(Key, NewValue) ->
    leviathan_db:write(#counter{id = Key, count = NewValue}).

% XXX not implemented
update_instruction(_, _) -> [].


containers_from_lm(Host, ContsIpsMap, #{contsmap := #{conts := Conts}}) ->
    lists:map(fun(Cont) -> cont_record(Host, Cont, ContsIpsMap) end, Conts).

cens_and_cont_ips_from_lm(Host, #{censmap := #{cens := Cens},
                                  wiremap := #{wires := Wires}}) ->
    Fun = fun(Cen, Acc) ->
                  {cen_record(Host, Cen, Wires),
                   conts_ips_from_cen(Cen, Acc)}
          end,
    lists:mapfoldl(Fun, #{}, Cens).

conts_ips_from_cen(#{cenID := CenId, contIDs := ContIds, reservedIps := Ips},
                   ContsIpsMap) ->
    lists:foldl(fun({ContId, Ip}, Acc) ->
                        maps:put({CenId, ContId}, Ip, Acc)
                end, ContsIpsMap, lists:zip(ContIds, Ips)).

% convert a container record to a container map
cont_record(_Host,
            #{contID := ContId, cens := CenIds, reservedIdNums := Ids},
            ContIpsMap) ->
    lists:map(
      fun({CenId, IdNumber}) ->
              #leviathan_cont{
                 cont = ContId,
                 cen = CenId,
                 data = #{
                   idnumber => IdNumber,
                   ip_address => maps:get({CenId, ContId}, ContIpsMap)
                  }
                }
      end, lists:zip(CenIds, Ids)).

% convert a cen map to a cen record
cen_record(_Host, #{cenID := CenId,
                    wire_type := WireType,
                    contIDs := ContIds,
                    ipaddr_b := IpAddrB,
                    ip_address := IpAddr}, Wires) ->
    #leviathan_cen{
        cen = CenId,
        data = #{
          contIDs => ContIds,
          wire_type => WireType,
          ipaddr_b => IpAddrB,
          ipaddr => IpAddr
        },
        wires = Wires
    }.

% convert a cen record to a cen map
cen_map(#leviathan_cen{
            cen = CenId,
            data = #{contIDs := ContIds,
                     wire_type := WireType,
                     ipaddr_b := IpAddrB,
                     ipaddr := IpAddr}}) ->
    #{cenID => CenId,
      wire_type => WireType,
      contIDs => ContIds,
      ipaddr_b => IpAddrB,
      ip_address => IpAddr,
      reservedIps => to_be_filled}.

%% convert a list of container records into a cont map
cont_map(ContId, ContRecords, Ips0) ->
    Fun = fun(#leviathan_cont{cont = CId,
                              cen = CenId,
                              data = #{idnumber := Id, ip_address := Ip}},
              {ContMap, IpsAcc}) when CId == ContId ->

                  #{cens := Cens, reservedIdNums := Ids} = ContMap,
                  {ContMap#{cens := [CenId | Cens],
                            reservedIdNums := [Id | Ids]},
                   maps:put({CenId, ContId}, Ip, IpsAcc)}
          end,
    lists:foldl(Fun, {#{contID => ContId, cens => [], reservedIdNums => []}, Ips0},
                ContRecords).

% convert a cen record into a list of wires
wire_map(#leviathan_cen{wires = Wires}) ->
    Wires.

% read cens from db
get_cens(CenIds) ->
    valid_list(fun get_cen/1, CenIds).

% read one cen from db
get_cen(CenId) ->
    Fn = fun() ->
                 case leviathan_db:read({leviathan_cen, CenId}) of
                     [] -> not_found;
                     [CenRecord] -> cen_map(CenRecord)
                 end
         end,
    leviathan_db:transaction(Fn).

% read containers from db
% XXX host is hardcoded
get_conts_and_their_ips(Cens) ->
    ContIds = contids_from_cens(Cens),
    Fun = fun(ContId, {ContsAcc, Ips0} = Acc) ->
                  case get_cont("host1", ContId, Ips0) of
                      not_found ->
                          Acc;
                      {Cont, Ips1} ->
                          {[Cont | ContsAcc], Ips1}
                  end
          end,
    lists:foldl(Fun, {[], #{}}, ContIds).

get_cont(_Host, ContId, Ips) ->
    Fn = fun() ->
                 case leviathan_db:read({leviathan_cont, ContId}) of
                     [] ->
                         not_found;
                     ContRecords ->
                         cont_map(ContId, ContRecords, Ips)
                 end
         end,
    leviathan_db:transaction(Fn).

% get_wires/1 returns the list of wires per Cen. Flatten
% the list with lists:append/1 rather than with lists:flatten/1 because
% the wires themselves are lists.
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

% filter out not_found
valid_list(GetFn, Keys) ->
    lists:foldl(
        fun(Key, Acc) ->
            case GetFn(Key) of
                not_found -> Acc;
                Cont -> [Cont | Acc]
            end
        end, [], Keys).

% make a list of unique container ids by inspecting the cens
contids_from_cens(Cens) ->
    sets:to_list(lists:foldl(
        fun(#{contIDs := ContIds}, Acc) ->
            sets:union(Acc, sets:from_list(ContIds))
        end, sets:new(), Cens)).

gen_container_id_number(ContId, UsedIds) ->
    gen_container_id_number(ContId, UsedIds, length(UsedIds)).

gen_container_id_number(ContId, UsedIds, N) ->
    case lists:member(N, UsedIds) of
        false ->
            N;
        true ->
            gen_container_id_number(ContId,
                                    UsedIds,
                                    (N+1) rem (_DummyInterfacesLimit = 1000))
    end.

                                                % set B network for Cen
cen_b(IpAddr) ->
    {ok, {_, CenB, _, _}} = inet:parse_address(IpAddr),
    CenB.

update_leviathan_cen_wires(Wire, undefined) ->
    [Wire];
update_leviathan_cen_wires(Wire, Wires) ->
    [Wire | Wires].

