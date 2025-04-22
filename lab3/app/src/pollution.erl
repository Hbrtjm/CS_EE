%%%-------------------------------------------------------------------
%%% @author Hubert Miklas
%%% @copyright (C) 2025, AGH UST
%%% @doc
%%%
%%% @end
%%% Created : 19. Mar 2025 14:15
%%%-------------------------------------------------------------------
-module(pollution).
-author("user").

-export([create_monitor/0, add_station/3, add_value/5, remove_value/4, get_one_value/4, get_station_min/3, get_over_limit/3, get_station_mean/3, get_daily_mean/3, get_nearest_stations/3]).


% Filter matching stations by name or coordinate
filter_stations(CoordinateOrName, Monitor) ->
  lists:filter(fun ({Name, Coordinate, _}) ->
    Coordinate == CoordinateOrName orelse Name == CoordinateOrName
               end, Monitor).

% Partition stations by
partition_stations(CoordinateOrName, Monitor) ->
  lists:partition(fun ({Name, Coordinates, _}) ->
    Coordinates == CoordinateOrName orelse Name == CoordinateOrName
                  end, Monitor).

% Create an empty monitor
create_monitor() -> [].

% Add a new station to the monitor
add_station(Name, Coordinates, Monitor) ->
  case filter_stations(Name,Monitor) ++ filter_stations(Coordinates,Monitor) of
    [] -> Monitor ++ [{Name, Coordinates, []}];
    _  -> {error, "The station already exists in the dataset"}
  end.

% Add a reading to a station
% Add value fail failed
add_value(CoordinatesOrName, DateTime, Type, Value, Monitor) ->
  case partition_stations(CoordinatesOrName, Monitor) of
    {[{Name, Coordinates, Values} | _], OtherStations} ->
      case lists:filter(fun ({TypeName, OldDateTime, _}) ->
        TypeName == Type andalso DateTime == OldDateTime
                        end, Values) of
        [] -> UpdatedStation = {Name, Coordinates, Values ++ [{DateTime, Type, Value}]},
          OtherStations ++ [UpdatedStation];
        _  -> {error, "Such reading already exists"}
      end;
    _ -> {error, "Station does not exist"}
  end.

% Removes value from the readings of given station
remove_value(CoordinateOrName, Time, Type, Monitor) ->
  case partition_stations(CoordinateOrName, Monitor) of
    {[{Name, Coordinates, Values} | _], OtherStations} ->
      case lists:any(fun ({PollutantType, PollutantTime, _}) ->
        PollutantTime == Time andalso PollutantType == Type
                     end, Values) of
        false -> {error, "Value to be deleted does not exist"};
        true ->
          UpdatedValues = lists:filter(fun ({PollutantType, PollutantTime, _}) ->
            not (PollutantTime == Time andalso PollutantType == Type)
                                       end, Values),
          UpdatedStation = {Name, Coordinates, UpdatedValues},
          OtherStations ++ [UpdatedStation]
      end;
    _ -> {error, "Station does not exist"}
  end.


% Gets one value of the reading by date and type from the given station
get_one_value(CoordinatesOrName, Type, DateTime, Monitor) ->
  case filter_stations(CoordinatesOrName, Monitor) of
    [] -> {error, "Station not found"};
    [{_, _, Values} | _] ->
      case lists:filter(fun ({ReadingType, ReadingDateTime, _}) ->
        ReadingType == Type andalso ReadingDateTime == DateTime
                        end, Values) of
        [] -> {error, "Reading not found"};
        [{_, _, Value} | _] -> Value
      end
  end.

% Gets the minimum reading of the station given by name or coordinate
get_station_min(CoordinatesOrName, Type, Monitor) ->
  case filter_stations(CoordinatesOrName,Monitor) of
    [] -> {error, "Station not found"};
    [{_,_,Values}| _] ->
      FilteredValues = [ReadingValue || {ReadingType, _, ReadingValue} <- Values, ReadingType == Type],
      case FilteredValues of
        [] -> {error, "No readings found for this type"};
        _  -> lists:min(FilteredValues)
      end
  end.

% Calculates mean value for one type of pollutant for given station
get_station_mean(CoordinatesOrName, Type, Monitor) ->
  case filter_stations(CoordinatesOrName,Monitor) of
    [] -> {error, "Station not found"};
    [{_,_,Values}| _] ->
      FilteredValues = [ReadingValue || {ReadingType, _, ReadingValue} <- Values, ReadingType == Type],
      case FilteredValues of
        [] -> {error, "No readings found for this type"};
        _ -> lists:sum(FilteredValues) / length(FilteredValues)
      end
  end.


% Gets the daily mean of the station for the given pollutant
get_daily_mean(Type, Date, Monitor) ->
  Values = lists:flatmap(fun({_, _, StationValues}) -> StationValues end, Monitor),
  MatchedValues = [Value || {ReadingType,{ReadingDate, _}, Value} <- Values,
    ReadingType == Type,
    ReadingDate == Date],
  case MatchedValues of
    [] -> {error, "No values found for given type and date"};
    _ -> lists:sum(MatchedValues) / length(MatchedValues)
  end.

% Counts how many stations have detected a breach of norm for the given pollutant
get_over_limit(Limit, Type, Monitor) ->
  length([
    Value || {ReadingType, _, Value} <- lists:flatten([Values || {_, _, Values} <- Monitor]),
    Type == ReadingType andalso Value >= Limit
  ]).

% Gets N stations that are closest to the given point
get_nearest_stations(StationsAmount, {XRequested, YRequested}, Monitor) ->
  SortedStations = lists:keysort(1,
    [ { math:pow((XRequested - X),2) + math:pow((YRequested - Y),2), Station }
      || Station = {_, {X, Y}, _} <- Monitor ]),
  [Station || {_, Station} <- lists:sublist(SortedStations, StationsAmount)].

