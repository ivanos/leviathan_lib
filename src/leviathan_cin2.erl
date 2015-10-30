-module(leviathan_cin2).

-export([build_cins/1,
         prepare_cins/1]).

-define(LM_EMPTY, ?LM([], [])).
-define(LM(Cins, Conts), #{cins => Cins, conts => Conts}).

-define(MATCH_LM(Cins, Conts), #{cins := Cins, conts := Conts}).
-define(MATCH_EMPTY_LM, ?MATCH_LM([], [])).

-include("leviathan.hrl").

%% -------------------------------------------------------------------------------
%% Types
%% -------------------------------------------------------------------------------

-type cont_id() :: [{HostId :: string(), ContId :: string()}].
-type cin_map() :: #{cinID => string(),
                     cenIDs => [string()],
                     contIDs => [cont_id()] ,
                     ip_b => 0..255,
                     ip => inet:ip4_address()}.
-type cont_map() :: #{contID => cont_id(),
                      cinID => string(),
                      ip => inet:ip4_address()}.
-type cin_lm() :: #{cins => [cin_map()] | [],
                    conts => [cont_map()] | []}.

-export_type([cont_id/0, cin_map/0, cont_map/0, cin_lm/0]).

%% -------------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------------

-spec build_cins(#{CinId :: string() => CenIds :: [string()]}) -> cin_lm().

build_cins(CinToCensMap) ->
    maps:fold(fun build_cin/3, ?LM_EMPTY, CinToCensMap).


-spec prepare_cins(CinIds :: [string()]) -> ok.

prepare_cins(CinIds) ->
    ok.

%% -----------------------------------------------------------------------------
%% Local Functions: building cins
%% -----------------------------------------------------------------------------

build_cin(CinId, CenIds, ?MATCH_LM(Cins, Conts)) ->
    CinMap = cin_map(CinId, CenIds),
    ContsMap = cont_maps(CinMap),
    ?LM([CinMap | Cins], ContsMap ++ Conts).

cin_map(CinId, [CenId] = CenIds) ->
    ContIds = leviathan_dby:get_cen_conts(CenId),
    IpB = next_cin_ip_b(),
    #{cinID => CinId,
      cenIDs => CenIds,
      contIDs => ContIds,
      ip_b => IpB,
      ip => cin_ip_address(IpB)}.

cont_maps(#{cinID := CinId, contIDs := ContIds, ip_b := IpB}) ->
    Fn = fun(ContId, ContCount) ->
                 {#{contID => ContId,
                   cinID => CinId,
                   ip => cont_ip_address(IpB, ContCount)},
                  ContCount + 1}
         end,
    element(1, lists:mapfoldl(Fn, 1, ContIds)).

%% -----------------------------------------------------------------------------
%% Local Functions: addressing
%% -----------------------------------------------------------------------------

next_cin_ip_b() ->
    leviathan_common_store:next_count(cin_ip_b, 10).

cin_ip_address(CenB) ->
    inet_parse:ntoa({10, CenB, 0, 1}).

%% Generate an IP address in the form:
%% 10.NN.C1.C2
%% where NN is in the range 10-250 and C1.C2 is 0.10-255.240
%% NN is derived from the IpB, C1.C2 from ContCount
%% IpB is the "B" part of the address computed by next_cen_ip_b/0
cont_ip_address(IpB, ContCount) when ContCount =< 65511 ->
    C = ContCount + 9, %% offset
    <<C1:8, C2:8>> = <<C:16>>,
    inet_parse:ntoa({10, IpB, C1, C2});
cont_ip_address(IpB, UsedIps) when is_list(UsedIps) andalso length(UsedIps) =< 6551 ->
    cont_ip_address(IpB, UsedIps, (length(UsedIps) + 1) rem 65511).

cont_ip_address(IpB, UsedIps, N) ->
    Ip = cont_ip_address(IpB, N),
    case lists:member(Ip, UsedIps) of
        false ->
            Ip;
        true ->
            cont_ip_address(IpB, UsedIps, N+1)
    end.


