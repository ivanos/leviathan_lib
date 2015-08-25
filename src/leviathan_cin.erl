-module(leviathan_cin).

-export([ip_address/2,prepare_wire_end/1,cen_ip_address/1]).

% Generate an IP address in the form:
% 10.NN.C1.C2
% where NN is in the range 10-250 and C1.C2 is 0.10-255.240
% NN is derived from the NetCount, C1.C2 from ContCount
% CenB is the "B" part of the address computed by cen_ip_address.
ip_address(CenB, ContCount) when ContCount =< 65511 ->
    C = ContCount + 9, %% offset
    <<C1:8, C2:8>> = <<C:16>>,
    list_to_binary(inet_parse:ntoa({10, CenB, C1, C2})).

prepare_wire_end(#{type := cont, id := ContId, alias := Alias, ip_address := IPAddress }) ->
    CmdBundle = leviathan_linux:set_ip_address(ContId, Alias, IPAddress),
    leviathan_linux:eval(CmdBundle).

cen_ip_address(NetCount) when NetCount =< 244 ->
    B = NetCount + 6, %% offset
    list_to_binary(inet_parse:ntoa({10, B, 0, 1})).
