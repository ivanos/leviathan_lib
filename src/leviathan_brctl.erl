-module(leviathan_brctl).

-compile(export_all).

%
% example: brctl addbr levbr0
%
addbr(BridgeName)->
    "brctl addbr " ++ BridgeName.

%
% example: brctl addbr levbr0
%
delbr(BridgeName)->
    "brctl delbr " ++ BridgeName.

%
% example: brctl addif docker0 A
%
addif(BridgeName,DeviceName)->
    "brtcl addif " ++ BridgeName ++ " " ++ DeviceName.

%
% example: brctl addif docker0 A
%
delif(BridgeName,DeviceName)->
    "brtcl delif " ++ BridgeName ++ " " ++ DeviceName.


%% TODO %%
%
%    show                                    show a list of bridges          (5)
%    showbr          <bridge>                show bridge info                (6)
%    showmacs        <bridge>                show a list of mac addrs        (7)
%
%    setageing       <bridge> <time>         set ageing time                 (8)
%    setbridgeprio   <bridge> <prio>         set bridge priority             (9)
%    setfd           <bridge> <time>         set bridge forward delay        (10)
%    setgcint        <bridge> <time>         set garbage collection interval (11)
%    sethello        <bridge> <time>         set hello time                  (12)
%    setmaxage       <bridge> <time>         set max message age             (13)
%    setpathcost     <bridge> <port> <cost>  set path cost                   (14)
%    setportprio     <bridge> <port> <prio>  set port priority               (15)
%    stp             <bridge> <state>        {dis,en}able stp                (16)
