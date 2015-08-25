-module(leviathan_os).

-export([cmd/1]).

-include("leviathan_logger.hrl").

cmd(Cmd) ->
    ?INFO("os:cmd(~s)", [Cmd]),
%   [].
    os:cmd(Cmd).
