-module(leviathan_store).

-export([import_cens/2,
         get_levmap/1,
         update_cens/2,
         get_next_cin_ip/0]).

-include("leviathan.hrl").

import_cens(Host, LM) ->
    Records = [
        cens_from_lm(Host, LM),
        containers_from_lm(Host, LM)
    ],
    Fn = fun() ->
        ok = leviathan_db:write(lists:flatten(Records))
    end,
    ok = leviathan_db:transaction(Fn).

get_levmap(CenIds) ->
    Cens = get_cens(CenIds),
    #{censmap => #{cens => Cens},
      contsmap => #{conts => get_conts(Cens)},
      wiremap => #{wires => get_wiremaps(CenIds)}
    }.

update_cens(Host, Instructions) ->
    Fn = fun() ->
        Records = lists:map(
            fun(Instruction) -> update_instruction(Host, Instruction) end,
            Instructions),
        ok = leviathan_db:write(lists:flatten(Records))
    end,
    ok = leviathan_db:transaction(Fn).

get_next_cin_ip() ->
    % XXX not implemented.
    ok.

% -----------------------------------------------------------------------------
%
% local functions
%
% -----------------------------------------------------------------------------

% XXX not implemented
update_instruction(_, _) -> [].


containers_from_lm(Host, #{contsmap := #{conts := Conts}}) ->
    lists:map(fun(Cont) -> cont_record(Host, Cont) end, Conts).

cens_from_lm(Host, #{censmap := #{cens := Cens},
                     wiresmap := #{wires := Wires}}) ->
    lists:map(fun(Cen) -> cen_record(Host, Cen, Wires) end, Cens).

% convert a container record to a container map
cont_record(_Host, #{contID := ContId, cens := CenIds}) ->
    lists:map(
        fun(CenId) ->
            #leviathan_cont{
                cont = ContId,
                cen = CenId,
                data = #{
                    idnumber => null,
                    ip_address => null
                }
            }
        end, CenIds).

% convert a cen map to a cen record
cen_record(_Host, #{cenID := CenId,
                   wire_type := WireType,
                   contIDs := ContIds,
                   ipaddr := IpAddr}, Wires) ->
    #leviathan_cen{
        cen = CenId,
        data = #{
            contIDs => ContIds,
            wire_type => WireType,
            ipaddr => IpAddr
        },
        wires = Wires
    }.

% convert a cen record to a cen map
cen_map(#leviathan_cen{
            cen = CenId,
            data = #{contIDs := ContIds,
                    wire_type := WireType,
                    ipaddr := IpAddr}}) ->
    #{cenID => CenId,
     wire_type => WireType,
     contIDs => ContIds,
     ipaddr => IpAddr}.

% convert a list of container records into a cont map
cont_map(ContId, ContRecords) ->
    lists:foldl(
        fun(#leviathan_cont{cont = CId, cen = CenId},
                            ContMap = #{cens := Cens}) when CId == ContId ->
            ContMap#{cens := [CenId | Cens]}
        end, #{contID => ContId, cens => []}, ContRecords).

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
get_conts(Cens) ->
    ContIds = contids_from_cens(Cens),
    valid_list(fun(ContId) -> get_cont("host1", ContId) end, ContIds).

get_cont(_Host, ContId) ->
    Fn = fun() ->
        case leviathan_db:read({leviathan_cont, ContId}) of
            [] -> not_found;
            ContRecords -> cont_map(ContId, ContRecords)
        end
    end,
    leviathan_db:transaction(Fn).

% get_wires/1 returns the list of wires per Cen. Flatten
% the list with lists:append/1 rather than with lists:flatten/1 because
% the wires themselves are lists.
get_wiremaps(CenIds) ->
    lists:append([get_wires(CenId) || CenId <- CenIds]).

get_wires(CenId) ->
    Fn = fun() ->
        case leviathan_db:read({leviathan_cen, CenId}) of
            [] -> [];
            CenRecord -> wire_map(CenRecord)
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
