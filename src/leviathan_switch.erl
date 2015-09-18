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
    import_json/1]).

-include("leviathan_logger.hrl").

%% @doc Start one or more switches based on a JSON description.
%%
%% This function parses `Binary' as JSON and passes it to {@link
%% import_json/1}.
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
import_json(Switches) when is_list(Switches) ->
    lists:foreach(fun import_json/1, Switches);
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
    leviathan_dby:import_switch(<<"host1">>, NewSwitch).

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
