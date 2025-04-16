%%%-------------------------------------------------------------------
%%% @author Hubert Miklas
%%% @copyright (C) 2025, AGH UST
%%% @doc
%%%
%%% @end
%%% Created : 09. Apr 2025 13:37
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("Hbrtjm").

%% API
-export([
  get_rand_locations/0,
  generate_people_list/0,
  dist/2,
  find_for_person_pidless/2,
  find_closest_pidless/2,
  find_for_person/3,
  find_closest_parallel/2,
  pollution_server/1,
  start/0,
  stop/0,
  init/0,
  main/1,
  call/2,
  add_station_server/1,
  add_value_server/1,
  remove_station_server/1,
  remove_value_server/1,
  get_one_value_server/1,
  get_station_min_server/1,
  get_station_mean_server/1,
  get_over_limit_server/1,
  get_nearest_stations_server/1,
  get_daily_mean_server/1,
  get_state_server/0
]).

-import(pollution, [create_monitor/0,
add_station/3,
remove_station/2,
add_value/5,
remove_value/4,
get_one_value/4,
get_station_min/3,
get_over_limit/3,
get_station_mean/3,
get_daily_mean/3,
get_nearest_stations/3]).

get_rand_locations() ->
  [{rand:uniform(10000),rand:uniform(10000)} || _ <- lists:seq(1,1000)].

generate_people_list() ->
  [{rand:uniform(10000),rand:uniform(10000)} || _ <- lists:seq(1,1000)].

dist({X1,Y1},{X2,Y2}) ->
  math:sqrt(math:pow((X1 - X2),2)  + math:pow((Y1 - Y2),2)).

find_for_person_pidless(PersonLocation, SensorLocations) ->
  lists:min([{dist(PersonLocation,SensorLocation),PersonLocation,SensorLocation} || SensorLocation <- SensorLocations]).

find_closest_pidless(PeopleLocations, SensorLocations) ->
  [ find_for_person_pidless(PersonLocation,SensorLocations) || PersonLocation <- PeopleLocations  ].

find_for_person(PersonLocation, SensorLocations, ParentPID) ->
  ParentPID ! lists:min([{dist(PersonLocation,SensorLocation),PersonLocation,SensorLocation} || SensorLocation <- SensorLocations]).

find_closest_parallel(SensorLocations, PeopleLocations) ->
  [ spawn(?MODULE,find_for_person, [Person, SensorLocations, self()]) || Person <- PeopleLocations],
    lists:min([ receive Value -> Value end || _ <- PeopleLocations]).

pollution_server(State) ->
  receive

    {add_station_command, PID, {Name, Coordinates}} ->
      case pollution:add_station(Name, Coordinates, State) of
        {error, Message} ->
          io:format("Error while adding the station (500): ~s~n", [Message]),
          PID ! {error, Message},
          pollution_server(State);
        NewMonitor ->
          PID ! {ok, added},
          pollution_server(NewMonitor)
      end;

    {add_value_command, PID, {CoordinatesOrName, DateTime, Type, Value}} ->
      case pollution:add_value(CoordinatesOrName, DateTime, Type, Value, State) of
        {error, Message} ->
          io:format("Error while adding value (500): ~s~n", [Message]),
          PID ! {error, Message},
          pollution_server(State);
        NewMonitor ->
          PID ! {ok, value_added},
          pollution_server(NewMonitor)
      end;

    {remove_station_command, PID, StationName} ->
      case pollution:remove_station(StationName, State) of
        {error, Message} ->
          io:format("Error while removing station (500): ~s~n", [Message]),
          PID ! {error, Message},
          pollution_server(State);
        NewMonitor ->
          PID ! {ok, removed},
          pollution_server(NewMonitor)
      end;

    {remove_value_command, PID, {CoordinatesOrName, DateTime, Type}} ->
      case pollution:remove_value(CoordinatesOrName, DateTime, Type, State) of
        {error, Message} ->
          io:format("Error while removing value (500): ~s~n", [Message]),
          PID ! {error, Message},
          pollution_server(State);
        NewMonitor ->
          PID ! {ok, value_removed},
          pollution_server(NewMonitor)
      end;

    {get_one_value_command, PID, {CoordinatesOrName, DateTime, Type}} ->
      case pollution:get_one_value(CoordinatesOrName, Type, DateTime, State) of
        {error, Message} ->
          io:format("Error while fetching value (500): ~s~n", [Message]),
          PID ! {error, Message},
          pollution_server(State);
        Result ->
          PID ! {ok, Result},
          pollution_server(State)
      end;

    {get_station_min_command, PID, {CoordinatesOrName, Type}} ->
      case pollution:get_station_min(CoordinatesOrName, Type, State) of
        {error, Message} ->
          io:format("Error getting min reading (500): ~s~n", [Message]),
          PID ! {error, Message},
          pollution_server(State);
        Result ->
          PID ! {ok, Result},
          pollution_server(State)
      end;

    {get_station_mean_command, PID, {CoordinatesOrName, Type}} ->
      case pollution:get_station_mean(CoordinatesOrName, Type, State) of
        {error, Message} ->
          io:format("Error getting mean reading (500): ~s~n", [Message]),
          PID ! {error, Message},
          pollution_server(State);
        Result ->
          PID ! {ok, Result},
          pollution_server(State)
      end;

    {get_over_limit_command, PID, {Limit, Type}} ->
      case pollution:get_over_limit(Limit, Type, State) of
        {error, Message} ->
          io:format("Error getting over-limit stations (500): ~s~n", [Message]),
          PID ! {error, Message},
          pollution_server(State);
        Result ->
          PID ! {ok, Result},
          pollution_server(State)
      end;

    {get_nearest_stations_command, PID, {Amount, Position}} ->
      case pollution:get_nearest_stations(Amount, Position, State) of
        {error, Message} ->
          io:format("Error getting nearest stations (500): ~s~n", [Message]),
          PID ! {error, Message},
          pollution_server(State);
        Result ->
          PID ! {ok, Result},
          pollution_server(State)
      end;

    {get_daily_mean_command, PID, {Type, Date}} ->
      case pollution:get_daily_mean(Type, Date, State) of
        {error, Message} ->
          io:format("Error getting daily mean (500): ~s~n", [Message]),
          PID ! {error, Message},
          pollution_server(State);
        Result ->
          PID ! {ok, Result},
          pollution_server(State)
      end;

    {get_state_command, PID, []} ->
      PID ! {ok, State},
      pollution_server(State);

  %% fallback for unmatched messages
    {_,PID,Unknown} ->
      io:format("Received unknown message: ~p~n", [Unknown]),
      PID ! {error, [Unknown]},
      pollution_server(State)
  end.

%%% =========================
%%% ==== SERVER MGMT =======
%%% =========================

start() ->
  register(pollutionServer, spawn(?MODULE, init, [])).

stop() ->
  unregister(pollutionServer).

init() ->
  pollution_server(pollution:create_monitor()).

call(Action, Params) ->
  pollutionServer ! {Action, self(), Params},
  receive
    Reply -> Reply
  end.

main(Main) ->
  io:format("Main: ~s~n", [Main]),
  start().


add_station_server(Params) -> call(add_station_command, Params).
add_value_server(Params) -> call(add_value_command, Params).
remove_station_server(Params) -> call(remove_station_command, Params).
remove_value_server(Params) -> call(remove_value_command, Params).
get_one_value_server(Params) -> call(get_one_value_command, Params).
get_station_min_server(Params) -> call(get_station_min_command, Params).
get_station_mean_server(Params) -> call(get_station_mean_command, Params).
get_over_limit_server(Params) -> call(get_over_limit_command, Params).
get_nearest_stations_server(Params) -> call(get_nearest_stations_command, Params).
get_daily_mean_server(Params) -> call(get_daily_mean_command, Params).
get_state_server() -> call(get_state_command, []).
