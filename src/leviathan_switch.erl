%% @doc
%% This module starts an OpenFlow switch and publishes the appropriate
%% information to Dobby.
%%
%% Use it like this:
%%
%% ```
%%     leviathan_switch:import_json(
%%       #{<<"type">> => <<"local/linc">>,
%%         <<"interfaces">> => [<<"cen1">>, <<"cen2">>]}).
%% '''
%%
%% The Docker image named in the `type' attribute will be started with
%% the options `--net=host --privileged=true', and the datapath id and
%% the interface names will be passed as command line arguments.  The
%% Docker image should have an `ENTRYPOINT' appropriately set to
%% accept those arguments.
-module(leviathan_switch).

-export(
   [import_binary/1,
    import_json/1,
    wait_for_switch/2]).

-include("leviathan_logger.hrl").

%% @doc Start one or more switches based on a JSON description.
%%
%% This function parses `Binary' as JSON and passes it to {@link
%% import_json/1}.  It returns a list of binaries: the datapath ids of
%% the started switches.
-spec import_binary(binary()) -> [binary()].
import_binary(Binary) ->
    SwitchMap = jiffy:decode(Binary, [return_maps]),
    import_json(SwitchMap).

%% @doc Start one or more switches, and publish info to Dobby.
%%
%% The argument can be a single map, or a list of maps.  Each
%% map should have two keys:
%%
%% The `<<"type">>' key should have a binary value, that names a
%% Docker image.
%%
%% The `<<"interfaces">>' key should have a list of binaries,
%% each naming an interface name that the newly started switch
%% should manage.
%%
%% This function returns a list of binaries: the datapath ids of the
%% started switches.
-spec import_json(Switch | [Switch]) -> [binary()]
  %% XXX: can't specify types for map entries with binary keys.
  when Switch :: map().
import_json(Switches) when is_list(Switches) ->
    lists:append(lists:map(fun import_json/1, Switches));
import_json(#{<<"type">> := CTypeBin,
              <<"interfaces">> := InterfacesBin} = Switch) ->
    CType = binary_to_list(CTypeBin),
    Interfaces = lists:map(fun binary_to_list/1, InterfacesBin),
    %% Create a random datapath id.  In principle, we should base this
    %% on a MAC address, but we currently don't extract MAC addresses
    %% from our interfaces.
    DatapathIdRaw = crypto:rand_bytes(8),
    DatapathId = format_datapath_id(DatapathIdRaw),
    DatapathIdBin = list_to_binary(DatapathId),
    {ok, ContainerId} = run_switch(CType, DatapathId, Interfaces),
    NewSwitch = Switch#{<<"contID">> => ContainerId,
                        <<"datapath_id">> => DatapathIdBin},
    leviathan_dby:import_switch(<<"host1">>, NewSwitch),
    [DatapathIdBin].

%% @doc Start a switch, without publishing anything to Dobby.
run_switch(CType, DatapathId, Interfaces) ->
    CmdBundle = [leviathan_docker:run(CType, "--net=host --privileged=true",
                                      string:join([DatapathId] ++ Interfaces, " "))],
    [ContainerId] = leviathan_linux:eval(CmdBundle, output),
    io:format("switch results:~n~p~n", [ContainerId]),

    {ok, ContainerId}.

format_datapath_id(DatapathId) ->
    string:join([integer_to_hex(D) || <<D>> <= DatapathId],":").

integer_to_hex(I) ->
    case integer_to_list(I, 16) of
        [D] -> [$0, D];
        DD  -> DD
    end.

%% @doc Wait for a connection from a switch with the given `DatapathId'.
%%
%% Wait for `Timeout' milliseconds before giving up.
-spec wait_for_switch(binary(), integer()) -> 'ok' | {'error', 'timeout'}.
wait_for_switch(DatapathId, Timeout) when is_integer(Timeout), Timeout >= 0 ->
    case weave_ofsh:is_connected(DatapathId) of
        true ->
            ok;
        false ->
            Sleep = 100,
            timer:sleep(Sleep),
            wait_for_switch(DatapathId, Timeout - Sleep)
    end;
wait_for_switch(_, Timeout) when is_integer(Timeout), Timeout < 0 ->
    {error, timeout}.
