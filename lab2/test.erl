-module(test).
-export([createAndAsk/0, reply/0]).
createAndAsk() ->
Pid = spawn(test, reply, []),
Pid ! {self(), question},
receive
answer -> io:format("Received!")
end.
reply() ->
receive
{Pid, question} -> Pid ! answer
end.
