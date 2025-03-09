%%%-------------------------------------------------------------------
%%% @author user
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Mar 2025 14:35
%%%-------------------------------------------------------------------
-module(basic).
-author("user").

%% API
%%-export([hello/0,fact/1,print_value/2,external/1,power/2]).
-export([hello/0,fact/1,power/2]).

hello() -> io:format("Henlo~n").

fact(N) when N > 0 ->
  N*fact(N-1);
fact(0) -> 1.

power(_,0) -> 1;
power(Value,Exp) when Exp rem 2 == 0 -> Half = power(Value,Exp div 2),   Half * Half;
power(Value,Exp) when Exp rem 2 == 1 -> Half = power(Value,Exp div 2), Value * Half * Half.

%%print_value(i,func) -> io:format("Value of function ~i~n",func(i)).

%%external(X) ->
%%  print_value(fun fact/1, X).
