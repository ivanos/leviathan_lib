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

-include_lib("stdlib/include/ms_transform.hrl").
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

update_cens(Host, Instructions0) ->
    Instructions = sort_update_instructions(Instructions0),
    Fns = lists:map(
            fun(Instruction) ->
                    update_instruction(Host, Instruction)
            end, Instructions),
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

%% @doc Return lists of instructions functions that updates the authoritative store.
%%
%% The instruction list passed to the funciton is sorted as described by
%% {@link sort_update_instructions/1}. Below is a list of alload instrcutions:
%% - {add, cen, CenMap}
%% - {add, cont, ContMap}
%% - {add, wire, Wire}
%% - {add, cont_in_cen, {ContMap, CenMap}}
%% - {add, bridge, {CenId, IpAddr}} XXX not used
%% - {destroy, cen, CenMap}
%% - {destroy, cont, ContMap}
%% - {destroy, wire, Wire}
%% - {destroy, cont_in_cen, {ContMap, CenMap}}
%% - {destroy, bridge, CenId} XXX not used
%% - {set, wire_type, {CenId, WireType}} XXX not used
update_instruction(Host, {add, cen, Cen = #{cenID := CenId,
                                            contIDs := Conts,
                                            reservedIps := Ips}}) ->
    %% If {add, cen, _} instruction exists in the list and there are
    %% any containers in the contIDs of Cen then:
    %% 1) This instruction was preceded by {add, cont, _} instructions
    %%    for all the containers in the contIDs; thus #leviathan_cont
    %%    records for these containers already exists
    %% 2) The aforementioned records are missing IP address information;
    %%    thus we update them with IP addresses based on reservedIps
    %%    field from this Cen
    %% 3) The IP address may not be already filled by {add, cont_in_cen, _}
    %%    instruction as it does not occur together with this instruction
    %%    for a particular cen
    MkSelectFun =
        fun(FCenId, FContId) ->
                ets:fun2ms(fun(#leviathan_cont{cen = CId, cont = CtId} = R)
                                 when CId =:= FCenId andalso CtId =:= FContId ->
                                   R
                           end)
        end,
    MkUpdateFun =
        fun(ContId, Ip) ->
                SelectFun = MkSelectFun(CenId, ContId),
                ContUpdateFun = fun(R = #leviathan_cont{data = Data0}) ->
                                    Data1 = Data0#{ip_address => Ip},
                                    R#leviathan_cont{data = Data1}
                            end,
                update_selected_or_write_fn({leviathan_cont, SelectFun}, ContUpdateFun)
                                    
        end,
    [
     write_fn(cen_record(Host, Cen)),
     lists:foldl(fun({ContId, Ip}, Acc) ->
                         [MkUpdateFun(ContId, Ip) | Acc]
                 end, [], lists:zip(Conts, Ips))
    ];
update_instruction(Host, {add, cont, Cont = #{contID := ContId}}) ->
    %% If {add, cont, _} instruction exists in the list then:
    %% 1) The cont is seen for the firt time and there are no records
    %%    for {AnyCen, ContId}; thus we create #leviathan_cont record
    %%    for each CenId in the Cont.
    %% 2) ip_address field for each record will be filled later as this
    %%    instrction is always followed by either {add, cen, _} or
    %%    {add, cont_in_cen, _} that refers to the cont in this instruction
    lists:map(fun(Record) ->
                      write_fn(Record)
              end, cont_record(Host, Cont));
update_instruction(_, {add, wire, Wire}) ->
    CenId = cenid_from_wire(Wire),
    update_fn({leviathan_cen, CenId},
              fun(CenRecord = #leviathan_cen{wires = Wires}) ->
                      CenRecord#leviathan_cen{wires = [Wire | Wires]}
              end);
update_instruction(_,
                   {add, cont_in_cen, {#{contID := ContId,
                                        reservedIdNums := Ids} = Cont,
                                       #{cenID := CenId,
                                         reservedIps := Ips} = Cen
                                      }}) ->
    %% If {add, cont_in_cen, _} instruction exists in the list then:
    %% 1) This instruction may have been preceded by {add, cont, _} for
    %%    the same container and appropriate #leviathan_cont may had
    %%    been created; thus we check for such record existence, and
    %%    create it if necessary.
    %% 2) If #leviathan_cont record already exists for {ContId, CenId}
    %%    we fill its IP address field.
    %% 3) There is no {add, cen, _} instruction for the CenId thus we
    %%    also need to update the Cen with new ContId
    MkSelectFun =
        fun(FCenId, FContId) ->
                ets:fun2ms(fun(#leviathan_cont{cen = CId, cont = CtId} = R)
                                 when CId =:= FCenId andalso CtId =:= FContId ->
                                   R
                           end)
        end,
    SelectFun = MkSelectFun(CenId, ContId),
    Ip = cont_ip_address(ContId, Cen),
    ContUpdateFun = fun(#leviathan_cont{data = Data0} = R) ->
                            Data1 = Data0#{ip_address => Ip},
                            R#leviathan_cont{data = Data1};
                       (no_match) ->
                            #leviathan_cont{
                               cont = ContId,
                               cen = CenId,
                               data = #{idnumber => cont_id_number(CenId, Cont),
                                        ip_address => Ip}
                              }
                    end,
    CenUpdateFun =
        fun(#leviathan_cen{data = Data0} = R) ->
                #{contIDs := Conts} = Data0,
                Data1 = Data0#{contIDs => Conts ++ [ContId]},
                R#leviathan_cen{data = Data1}
        end,
    [
     update_selected_or_write_fn({leviathan_cont, SelectFun}, ContUpdateFun),
     update_fn({leviathan_cen, CenId}, CenUpdateFun)
    ];
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

update_selected_or_write_fn({Table, Match}, UpdateOrWriteFn) ->
    %% fun() ->
    %%         Fn = fun(Record0) ->
    %%                      leviathan_db:write(UpdateFn(Record0)),
    %%                      leviathan_db:delete_object(Record0)
    %%              end,
    %%         lists:foreach(Fn, leviathan_db:select(Table, Match))
    %% end.
    fun() ->
            case leviathan_db:select(Table, Match) of
                [Record0] ->
                    leviathan_db:write(UpdateOrWriteFn(Record0)),
                    leviathan_db:delete_object(Record0);
                [] ->
                    leviathan_db:write(UpdateOrWriteFn(no_match))
            end
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

cont_id_number(CenId, #{cens := Cens, reservedIdNums := Ids}) ->
    [{CenId, Id} | _] = lists:dropwhile(fun({CId, _Id}) ->
                                                CId =/= CenId
                                        end, lists:zip(Cens, Ids)),
    Id.

cont_ip_address(ContId, #{contIDs := Conts, reservedIps := Ips}) ->
    [{ContId, Ip} | _] = lists:dropwhile(fun({CtId, _Ip}) ->
                                                 CtId =/= ContId
                                         end, lists:zip(Conts, Ips)),
    Ip.

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

cont_record(_, #{contID := ContId, cens := CenIds, reservedIdNums := Ids}) ->
    lists:map(
      fun({CenId, Id}) ->
              #leviathan_cont{cont = ContId,
                              cen = CenId,
                              data = #{idnumber => Id,
                                       ip_address => to_be_filled
                                      }
                             }
      end, lists:zip(CenIds, Ids)).

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

%% @doc Sort update instructions.
%%
%% This function sorts the instruction list by an instruction item
%% (regardles of the operation: add/destroy). The output order of
%% instruction list is following:
%% cont < cen < cont_in_cen < wire. 
sort_update_instructions(Instructions) ->
    Fn = fun({_, cont_in_cen, _}, {_, wire, _}) ->
                 %% wires should be at the end of the list; cont should go before wires
                 true;
            ({_, cen, _}, {_, ContInCenOrWire, _})
               when ContInCenOrWire =:= cont_in_cen orelse ContInCenOrWire =:= wire ->
                 %% cen sholud be before cont_in_cens and wires
                 true;
            ({_, cont, _}, _) ->
                 %% cont should be at the beginning
                 true;
            (_, _) ->
                 false
         end,
    lists:sort(Fn, Instructions).



