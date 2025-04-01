%%%-------------------------------------------------------------------
%%% @author Hbrtjm
%%% @copyright (C) 2025, AGH
%%% @doc
%%%
%%% @end
%%% Created : 05. Mar 2025 22:26
%%%-------------------------------------------------------------------
-module(pollution).
-author("user").

%% API
-export([number_of_readings/2,calculate_max/2,calculate_min/2,generate_table/1,find_reading/2,mymax/2,mymin/2,pick_a_value_undef/2,pick_a_value/2]).

pick_a_value_undef(Min, Max) when Min =< Max ->
  RandValue = round(rand:uniform() * 2),
  case RandValue rem 2 of
    1 -> rand:uniform(Max - Min + 1) + Min;
    _ -> undefined
  end.


pick_a_value(Min, Max) -> rand:uniform(Max - Min + 1) + Min.


generate_sample() ->
  City = "Cracow",
  DateTime = {{pick_a_value(2021,2025), pick_a_value(1,12), pick_a_value(1,28)}, {pick_a_value(0,23), pick_a_value(0,59), pick_a_value(0,59)} }, %% Example: {Year, Month, Day, Hour, Minute, Second}

  Pollutions = [
    {'pm10', pick_a_value_undef(1,100)},
    {'pm2.5', pick_a_value_undef(1,100)},
    {'pm1', pick_a_value_undef(1,100)},
    {'no3', pick_a_value_undef(1,100)},
    {'so', pick_a_value_undef(1,100)},
    {'co', pick_a_value_undef(1,100)}
  ],

  {City, DateTime, Pollutions}.

generate_table(0) -> [];
generate_table(N) -> [generate_sample() | generate_table(N - 1)].

%%number_of_readings(Readings,Date) -> int.
number_of_readings([],_) -> 0;
number_of_readings([{_,{ReadingDate,_},_}|Rest],Date) when ReadingDate == Date -> 1 + number_of_readings(Rest,Date);
number_of_readings([_|Rest],Date) -> number_of_readings(Rest,Date).

mymax(X,Y) when X > Y -> X;
mymax(X,Y) when Y >= X -> Y. %% So if they are equal, just let it be

mymin(X,Y) when X < Y -> X;
mymin(X,Y) when X >= Y -> Y. %% So if they are equal, just let it be

%% Finds the reading of particular type
find_reading([],_) -> undefined;
find_reading([{ReadingType,Value}|_],Type) when ReadingType =:= Type -> Value;
find_reading([_|Rest],Type) -> find_reading(Rest,Type).

%% Calculate max value for a given reading type
calculate_max([], _) -> undefined;
calculate_max([{_,_,Readings} | Rest], Type) ->
  case find_reading(Readings, Type) of
    undefined -> calculate_max(Rest, Type);
    Value ->
      case calculate_max(Rest, Type) of
        undefined -> Value;
        MaxRest -> mymax(Value, MaxRest)
      end
  end.

%% Calculate min value for a given reading type
calculate_min([], _) -> undefined;
calculate_min([{_,_,Readings} | Rest], Type) ->
  case find_reading(Readings, Type) of
    undefined -> calculate_min(Rest, Type);
    Value ->
      case calculate_min(Rest, Type) of
        undefined -> Value;
        MinRest -> mymin(Value, MinRest)
      end
  end.
