%%%-------------------------------------------------------------------
%%% @author user
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Mar 2025 23:28
%%%-------------------------------------------------------------------
-module(myLists).
-author("user").

%% API
-export([contains/2,duplicateElements/1,sumFloats/1]).

contains([],_) -> false;
contains([Value|Rest],Searched) -> Value=:=Searched orelse contains(Rest,Searched).

duplicateElements([]) -> [];
duplicateElements([Value|Rest]) -> [Value] ++ [Value] ++ duplicateElements(Rest).

sumFloats([]) -> 0;
sumFloats([Value | Rest]) when is_float(Value) -> Value + sumFloats(Rest);
sumFloats([_ | Rest]) -> sumFloats(Rest).
