-module(demo_03_processCreation).
-export([pass/1, pass/0]).
pass(Count) ->
Pid = spawn(?MODULE, pass, []),
Pid ! {self(),Count},
receive
_ -> ok
end.
pass() ->
receive
{ParentPid, Count} when Count > 0 ->
Pid = spawn(?MODULE, pass, []),
Pid ! {self(),Count - 1},
receive
_ -> ParentPid ! ok
end;
{ParentPid,_} -> ParentPid ! ok
end.
