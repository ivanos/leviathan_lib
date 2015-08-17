-module(leviathan_ifconfig).

-compile(export_all).

-include("leviathan_logger.hrl").

dev_up(DevName)->
    "ifconfig " ++ DevName ++ " up".
    
dev_down(DevName)->
    "ifconfig " ++ DevName ++ " down".
