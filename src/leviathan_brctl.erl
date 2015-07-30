-module(leviathan_brctl).

-compile(export_all).

%
% example: brctl addif docker0 A
%
addif(BridgeName,DeviceName)->
    "brtcl addif " ++ BridgeName ++ " " ++ DeviceName.
