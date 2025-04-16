%%%-------------------------------------------------------------------
%%% @author user
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Apr 2025 13:27
%%%-------------------------------------------------------------------
-module(ping_pong).
-author("user").

%% API
-export([start/0,ping_loop/0,pong_loop/1]).

start () ->
  register(ping,spawn(?MODULE,ping_loop,[])),
  register(pong,spawn(?MODULE,pong_loop,[0])).

ping_loop() ->
  receive
  0 ->
    io:format("Pinging finished, got 0"),
    ping_loop();
    N ->
      io:format("From ping: ~p~n", [N]),
      timer:sleep(100),
      pong ! N - 1, ping_loop()
    after
      5000 -> io:format("Finished")
  end.

pong_loop(State) ->
  receive
    0 ->
      io:format("Ponging finished, got 0"),
      pong_loop(State+1);
    N ->
      io:format("From pong: ~p~n", [N]),
      timer:sleep(100),
      ping ! N - 1, pong_loop(State+1)
  after
    5000 -> io:format("Finished pong calls: ~p~n",[State])
  end.