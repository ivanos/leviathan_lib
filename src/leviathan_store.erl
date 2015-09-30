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

-define(LM(Cens, Conts, Wires),#{censmap => #{cens => Cens},
                                 contsmap => #{conts => Conts},
                                 wiremap => #{wires => Wires}
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

get_levmap([]) ->
    ?LM([], [], []);
get_levmap(CenIds) ->
    Cens = get_cens(CenIds),
    {Conts, ContsIpsMap} =  get_conts_and_their_ips(Cens),
    ?LM(fill_reserved_ips_in_cens(Cens, ContsIpsMap),
        Conts,
        get_wiremap(CenIds)).

% special case for zero and one containers?
fill_reserved_ips_in_cens(Cens, ContsIpsMap) ->
    Fun =
        fun(#{contIDs := []} = Cen) ->
            Cen#{reservedIps := []};
           (#{ip_addrb := CenB, cenID := CenId, contIDs := [ContId]} = Cen) ->
            IpAddr = case maps:get({CenId, ContId}, ContsIpsMap) of
                undefined ->
                    leviathan_cin:ip_address(CenB, []);
                SetAddr ->
                    SetAddr
            end,
            Cen#{reservedIps := [IpAddr]};
           (#{cenID := CenId, contIDs := ContIds} = Cen) ->
            ReservedIps = lists:map(
                              mk_conts_ips_in_cen_fun(CenId, ContsIpsMap),
                              ContIds),
            Cen#{reservedIps := ReservedIps}
        end,
    lists:map(Fun, Cens).

mk_conts_ips_in_cen_fun(CenId, IpsMap) ->
    fun(ContId) ->
        maps:get({CenId, ContId}, IpsMap)
    end.

update_cens(Host, Instructions) ->
    {Fns, _} = lists:mapfoldl(
                 fun(Instruction, ContIpsMap) ->
                         update_instruction(Host, Instruction, ContIpsMap)
                 end, #{}, Instructions),
    Fn = fun() ->
        lists:foreach(fun(Fn) -> Fn() end, lists:flatten(Fns))
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
% - {add, cen, CenMap}
% - {add, cont, ContMap}
% - {add, wire, Wire}
% - {add, cont_in_cen, {ContId, CenId}}
% - {add, bridge, {CenId, IpAddr}} XXX not used
% - {destroy, cen, CenMap}
% - {destroy, cont, ContMap}
% - {destroy, wire, Wire}
% - {destroy, cont_in_cen, {ContId, CenId}}
% - {destroy, bridge, CenId} XXX not used
% - {set, wire_type, {CenId, WireType}} XXX not used
% Return lists of instructions functions that updates the
% authoritative store.
update_instruction(Host, {add, cen, Cen}, ContIpsMap) ->
    {write_fn(cen_record(Host, Cen)),
     conts_ips_from_cen(Cen, ContIpsMap)};
update_instruction(Host, {add, cont, Cont = #{contID := ContId}}, ContIpsMap) ->
    Fn = lists:map(
           fun(Record) ->
                   write_fn(Record)
           end, cont_record(Host, Cont, ContIpsMap)),
    {Fn, ContIpsMap};
update_instruction(_, {add, wire, Wire}, ContIpsMap) ->
    %% XXX set the ip address in the container
    CenId = cenid_from_wire(Wire),
    Fn = update_fn({leviathan_cen, CenId},
                   fun(CenRecord = #leviathan_cen{wires = Wires}) ->
                           CenRecord#leviathan_cen{wires = [Wire | Wires]}
                   end),
    {Fn, ContIpsMap};
update_instruction(_, {add, cont_in_cen, {ContId, CenId}}, ContIpsMap) ->
    %% XXX cens added to container by {add, cont, Cont}?
    %% update_fn({leviathan_cen, CenId},
    %%           fun(CenRecord = #leviathan_cen{data = Data0}) ->
    %%                   #{contIDs := ContIds, ipaddr_b = IpB} = Data0,
    %%                   leviathan_cin:ip_address(IpB, RIps),
    %%                   Data1 = Data0#{contIDs := [ContId] ++ ContIds},
    %%                   CenRecord#leviathan_cen{data = Data1}
    %%           end),
    {fun() ->
             %% update leviathan_cen
             [#leviathan_cen{data = Data0} = CenRecord] =
                 leviathan_db:read({leviathan_cen, CenId}),
             #{contIDs := ContIds, ipaddr_b := IpB} = Data0,
             Data1 = Data0#{contIDs := ContIds ++ [ContId]},
             leviathan_db:write(CenRecord#leviathan_cen{data = Data1}),

             %% create leviathan cont
             Ip = binary_to_list(
                    leviathan_cin:ip_address(IpB,
                                             get_cen_reserved_ips(CenId))),
             Id = next_cont_id_num(get_cont_reserved_ids(ContId)),
             ContRecord = #leviathan_cont{cont = ContId, cen = CenId,
                                          data = #{ip_address => Ip,
                                                   idnumber => Id}},
             leviathan_db:write(ContRecord)
     end, ContIpsMap}.
update_instruction(_, {add, bridge, _}) ->
    % not used
    [];
update_instruction(_, {destroy, cen, #{cenID := CenId}}) ->
    delete_fn({leviathan_cen, CenId});
update_instruction(_, {destroy, cont, #{contID := ContId}}) ->
    delete_fn({leviathan_cont, ContId});
update_instruction(_, {destroy, wire, Wire}) ->
    % XXX remove ip address from container?
    CenId = cenid_from_wire(Wire),
    update_fn({leviathan_cen, CenId},
        fun(CenRecord = #leviathan_cen{wires = Wires}) ->
            CenRecord#leviathan_cen{wires = Wires -- [Wire]}
        end);
update_instruction(_, {destroy, cont_in_cen, {ContId, CenId}}) ->
    [
        % remove container from cen
        update_fn({leviathan_cen, CenId},
            fun(CenRecord = #leviathan_cen{data = Data0}) ->
                #{contIDs := ContIds} = Data0,
                Data1 = Data0#{contIDs := ContIds -- [ContId]},
                CenRecord#leviathan_cen{data = Data1}
            end),
        % remove container
        delete_record_fn({leviathan_cont, ContId},
            fun(ContRecords) ->
                [Record | _] = lists:dropwhile(
                    fun(#leviathan_cont{cen = C}) ->
                        C == CenId
                    end, ContRecords),
                Record
            end)
    ];
update_instruction(_, {destroy, bridge, _}) ->
    % not used
    [];
update_instruction(_, {set, wire_type, _}) ->
    % not used
    [].

% fun to write the Record
write_fn(Record) ->
    fun() -> leviathan_db:write(Record) end.

% fun to delete the Key
delete_fn(Key) ->
    fun() -> leviathan_db:delete(Key) end.

% fun to delete record
delete_record_fn(Key, SelectFn) ->
    fun() ->
        Records = leviathan_db:read(Key),
        Record = SelectFn(Records),
        leviathan_db:delete_object(Record)
    end.

% fun to update the record with Key using the UpdateFn
update_fn(Key, UpdateFn) ->
    fun() ->
        [Record0] = leviathan_db:read(Key),
        Record1 = UpdateFn(Record0),
        leviathan_db:write(Record1)
    end.

cenid_from_wire(Wire) ->
    % in and out endpoints
    OutEndpoint = out_endpoint(Wire),
    % CenId from out endpoint
    #{dest := #{type := cen, id := CenId}} = OutEndpoint,
    CenId.

contip_from_wire(Wire) ->
    InEndpoint = in_endpoint(Wire),
    #{dest := #{type := cont, ip_address := Ip}} = InEndpoint,
    Ip.

contid_from_wire(Wire) ->
    InEndpoint = in_endpoint(Wire),
    #{dest := #{type := cont, id := Id}} = InEndpoint,
    Id.

out_endpoint([Out = #{side := out}, _]) ->
    Out;
out_endpoint([_, Out = #{side := out}]) ->
    Out.

in_endpoint([In = #{side := in}, _]) ->
    In;
in_endpoint([_, In = #{side := in}]) ->
    In.

cen_record(_, #{cenID := CenId,
                wire_type := WireType,
                contIDs := ContIds,
                ipaddr_b := BIpAddr,
                ip_address := IpAddr}) ->
    #leviathan_cen{
        cen = CenId,
        data = #{
            contIDs => ContIds,
            wire_type => WireType,
            ipaddr_b => BIpAddr,
            ipaddr => IpAddr
        },
        wires = []
    }.

% XXX overwrites existing container/cen combinations?
cont_record(_, #{contID := ContId,
                 cens := CenIds}) ->
    lists:map(
        fun(CenId) ->
            #leviathan_cont{
                cont = ContId,
                cen = CenId,
                data = #{
                    idnumber => 0,
                    ip_address => undefined
                }
            }
        end, CenIds).

containers_from_lm(Host, ContsIpsMap, #{contsmap := #{conts := Conts}}) ->
    lists:map(fun(Cont) -> cont_record(Host, Cont, ContsIpsMap) end, Conts).

cens_and_cont_ips_from_lm(Host, #{censmap := #{cens := Cens},
                                  wiremap := #{wires := Wires}}) ->
    GroupedWires = group_wires_by_cen(Wires),
    Fun = fun(Cen, Acc) ->
                  {cen_record(Host, Cen, GroupedWires),
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
       wires = maps:get(CenId, Wires, [])
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
                  {ContMap#{cens := Cens ++ [CenId],
                            reservedIdNums := Ids ++ [Id]},
                   maps:put({CenId, ContId}, Ip, IpsAcc)}
          end,
    lists:foldl(Fun, {#{contID => ContId, cens => [], reservedIdNums => []}, Ips0},
                ContRecords).

% convert a cen record into a list of wires
wire_map(#leviathan_cen{wires = Wires}) ->
    Wires.

% read cens from db
get_cens(CenIds) ->
    lists:reverse(valid_list(fun get_cen/1, CenIds)).

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

group_wires_by_cen(Wires) ->
    lists:foldl(fun group_wires_by_cen/2, #{}, Wires).

group_wires_by_cen(Wire = [
                            #{dest := #{alias := CenId}} = _InEndpoint,
                            #{dest := #{id := CenId}} = _OutEndpoint
                          ], Acc) ->
    CenWires = maps:get(CenId, Acc, []),
    maps:put(CenId, [Wire | CenWires], Acc).

get_cont_reserved_ids(ContId) ->
    MatchHead = #leviathan_cont{cont = ContId, data = '$1', _ = '_'},
    MatchSpec = [{MatchHead, _Guard = [], _Result = ['$1']}],
    lists:foldl(fun(#{idnumber := Id}, Acc) ->
                        [Id | Acc];
                   (_, Acc) ->
                        Acc
                end, [], leviathan_db:select(leviathan_cont, MatchSpec)).

get_cen_reserved_ips(CenId) ->
    MatchHead = #leviathan_cont{cen = CenId, data = '$1', _ = '_'},
    MatchSpec = [{MatchHead, _Guard = [], _Result = ['$1']}],
    lists:foldl(fun(#{ip_address := Ip}, Acc) ->
                        [Ip | Acc];
                   (_, Acc) ->
                        Acc
                end, [], leviathan_db:select(leviathan_cont, MatchSpec)).

next_cont_id_num(ReservedIds) ->
    %% TODO: throw an exception when there're no IPs left
    next_cont_id_number(ReservedIds, length(ReservedIds)).

next_cont_id_number(ReservedIds, IdCandidate) ->
    %% TODO: throw an exception when there're no id numbers left
    case lists:member(IdCandidate, ReservedIds) of
        false ->
            IdCandidate;
        true ->
            next_cont_id_number(ReservedIds,
                                (IdCandidate+1) rem (_DummyInterfacesLimit = 1000))
    end.
