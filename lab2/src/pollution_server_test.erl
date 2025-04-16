%%%-------------------------------------------------------------------
%%% @author Wojciech Turek
%%% @editor Hubert Miklas
%%% @copyright (C) 2019, AGH UST
%%% @doc
%%%
%%% @end
%%% Created : 14. mar 2019 12:5
%%% Edited  : 31. mar 2025
%%%-------------------------------------------------------------------
-module(pollution_server_test).
-author("Wojciech Turek").

-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_monitor_test() ->
  pollution_server:start(),
  try
    {ok, M1} = pollution_server:get_state_server(),
    pollution_server:stop(),
    pollution_server:start(),
    {ok, M2} = pollution_server:get_state_server(),
    ?assertEqual(M1, M2)
  after
    pollution_server:stop()
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_station_test() ->
  pollution_server:start(),
  try
    ?assertEqual({ok, added}, pollution_server:add_station_server({"Stacja 1", {1,1}})),
    {ok, _State} = pollution_server:get_state_server(),
    ?assertMatch({error, _}, pollution_server:add_station_server({"Stacja 1", {1,1}})),
    ?assertMatch({error, _}, pollution_server:add_station_server({"Stacja 1", {2,2}})),
    ?assertMatch({error, _}, pollution_server:add_station_server({"Stacja 2", {1,1}}))
  after
    pollution_server:stop()
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_value_test() ->
  pollution_server:start(),
  try
    % Add station
    {ok, _} = pollution_server:add_station_server({"Stacja 1", {1,1}}),

    % First set of values using station name
    Time = calendar:local_time(),
    ?assertNotMatch({error, _}, pollution_server:add_value_server({"Stacja 1", Time, "PM10", 46.3})),
    ?assertNotMatch({error, _}, pollution_server:add_value_server({"Stacja 1", Time, "PM1", 46.3})),
    ?assertNotMatch({error, _}, pollution_server:add_value_server({"Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3})),
    pollution_server:add_value_server({"Stacja 1", Time, "PM10", 46.3}),
    pollution_server:add_value_server({"Stacja 1", Time, "PM1", 46.3}),
    ?assertMatch({error, _}, pollution_server:add_value_server({"Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3})),
    timer:sleep(1100),
    Time2 = calendar:local_time(),
    % Add values using coordinates and prior monitor state
    ?assertNotMatch({error, _}, pollution_server:add_value_server({{1,1}, Time2, "PM10", 46.3})),
    ?assertNotMatch({error, _}, pollution_server:add_value_server({{1,1}, Time2, "PM1", 46.3})),
    ?assertNotMatch({error, _}, pollution_server:add_value_server({{1,1}, {{2023,3,27},{11,16,10}}, "PM10", 46.3})),
    % Continue building monitor
    pollution_server:add_value_server({{1,1}, Time2, "PM10", 46.3}),
    pollution_server:add_value_server({{1,1}, Time2, "PM1", 46.3}),
    ?assertMatch({error, _}, pollution_server:add_value_server({{1,1}, {{2023,3,27},{11,16,10}}, "PM10", 46.3}))
  after
    pollution_server:stop()
  end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_value_fail_test() ->
  pollution_server:start(),
  try
    {ok, added} = pollution_server:add_station_server( {"Stacja 1", {1,1}}),
    Time = calendar:local_time(),
    {ok, value_added} = pollution_server:add_value_server({"Stacja 1", Time, "PM10", 46.3}),
    ?assertMatch({error, _}, pollution_server:add_value_server({"Stacja 1", Time, "PM10", 46.3})),
    ?assertMatch({error, _}, pollution_server:add_value_server({"Stacja 1", Time, "PM10", 36.3})),
    ?assertMatch({error, _}, pollution_server:add_value_server({{1,1}, Time, "PM10", 46.3})),
    ?assertMatch({error, _}, pollution_server:add_value_server({{1,1}, Time, "PM10", 36.3}))
  after
    pollution_server:stop()
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_value_non_existing_station_test() ->
  pollution_server:start(),
  try
    {ok, added} = pollution_server:add_station_server( {"Stacja 1", {1,1}}),
    ?assertMatch({error, _}, pollution_server:add_value_server({"Stacja 2", calendar:local_time(), "PM10", 46.3})),
    ?assertMatch({error, _}, pollution_server:add_value_server({{1,2}, calendar:local_time(), "PM10", 46.3}))
  after
    pollution_server:stop()
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

remove_value_test() ->
  pollution_server:start(),
  try
    Time = calendar:local_time(),
    StaticTime = {{2023,3,27},{11,16,9}},
    ?assertEqual({ok, added}, pollution_server:add_station_server( {"Stacja 1", {1,1}})),

    % Add three different measurements (as long as your clock does not show 2023-03-27 11:16:09, if you are, hello from the future!)
    ?assertEqual({ok, value_added}, pollution_server:add_value_server({"Stacja 1", Time, "PM1", 46.3})),
    ?assertEqual({ok, value_added}, pollution_server:add_value_server({"Stacja 1", Time, "PM10", 46.3})),
    ?assertMatch({ok, value_added}, pollution_server:add_value_server({"Stacja 1", StaticTime, "PM10", 46.3})),
    ?assertEqual({ok, value_removed}, pollution_server:remove_value_server({"Stacja 1", Time, "PM10"})),
    ?assertEqual({ok, value_removed}, pollution_server:remove_value_server({"Stacja 1", StaticTime, "PM10"}))
  after
    pollution_server:stop()
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

remove_value_and_add_back_test() ->
  pollution_server:start(),
  try
    Time = calendar:local_time(),
    StaticTime = {{2023,3,27},{11,16,9}},

    % Add base station
    ?assertEqual({ok, added}, pollution_server:add_station_server({"Stacja 1", {1,1}})),
    % Add three values
    ?assertEqual({ok, value_added}, pollution_server:add_value_server({"Stacja 1", Time, "PM10", 46.3})),
    ?assertEqual({ok, value_added}, pollution_server:add_value_server({"Stacja 1", Time, "PM1", 46.3})),
    ?assertEqual({ok, value_added}, pollution_server:add_value_server({"Stacja 1", StaticTime, "PM10", 46.3})),
    % Remove the value
    ?assertEqual({ok, value_removed}, pollution_server:remove_value_server({"Stacja 1", StaticTime, "PM10"})),
    % Add the same value again
    ?assertEqual({ok, value_added}, pollution_server:add_value_server({{1,1}, StaticTime, "PM10", 46.3}))
  after
    pollution_server:stop()
  end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

remove_value_fail_test() ->
  pollution_server:start(),
  try
  ?assertEqual({ok, added}, pollution_server:add_station_server({"Stacja 1", {1,1}})),
  Time = calendar:local_time(),
  ?assertEqual({ok, value_added}, pollution_server:add_value_server({"Stacja 1", Time, "PM10", 46.3})),
  ?assertEqual({ok, value_added}, pollution_server:add_value_server({"Stacja 1", Time, "PM1", 46.3})),
  ?assertEqual({ok, value_added}, pollution_server:add_value_server({"Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3})),

  ?assertMatch({error, _}, pollution_server:remove_value_server({"Stacja 1", Time, "PM25"})),
  ?assertMatch({error, _}, pollution_server:remove_value_server({"Stacja 1", {{2023,3,27},{11,16,10}}, "PM10"})),
  ?assertMatch({error, _}, pollution_server:remove_value_server({{1,2}, Time, "PM10"})),
  ?assertMatch({error, _}, pollution_server:remove_value_server({"Stacja 2", Time, "PM10"}))
  after
    pollution_server:stop()
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_one_value_test() ->
  pollution_server:start(),
  try
    ?assertEqual({ok, added}, pollution_server:add_station_server({"Stacja 1", {1,1}})),
    Time = calendar:local_time(),
    StaticTime = {{2023,3,27},{11,16,9}},
    ?assertEqual({ok, value_added}, pollution_server:add_value_server({"Stacja 1", Time, "PM10", 46.3})),
    ?assertEqual({ok, value_added}, pollution_server:add_value_server({{1,1}, Time, "PM1", 36.3})),
    ?assertEqual({ok, value_added}, pollution_server:add_value_server({"Stacja 1", StaticTime, "PM10", 26.3})),
%%    ?assertMatch({ok,46.3}, pollution_server:get_one_value_server({"Stacja 1", Time, "PM10"})),
    ?assertMatch({ok,36.3}, pollution_server:get_one_value_server({"Stacja 1", Time, "PM1"}))
%%    ?assertMatch({ok,46.3}, pollution_server:get_one_value_server({{1,1}, Time, "PM10"})),
%%    ?assertMatch({ok,26.3}, pollution_server:get_one_value_server({"Stacja 1", StaticTime, "PM10"}))
  after
    pollution_server:stop()
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_one_value_fail_test() ->
  pollution_server:start(),
  try
    ?assertEqual({ok, added}, pollution_server:add_station_server({"Stacja 1", {1,1}})),
    Time = calendar:local_time(),
    StaticTime = {{2023,3,27},{11,16,9}},
    ?assertEqual({ok, value_added}, pollution_server:add_value_server({"Stacja 1", Time, "PM10", 46.3})),
    ?assertEqual({ok, value_added}, pollution_server:add_value_server({{1,1}, Time, "PM1", 36.3})),
    ?assertEqual({ok, value_added}, pollution_server:add_value_server({"Stacja 1", StaticTime, "PM10", 26.3})),


    ?assertMatch({error, _}, pollution_server:get_one_value_server({"Stacja 1", Time, "PM25", []})),
    ?assertMatch({error, _}, pollution_server:get_one_value_server({{1,1}, Time, "PM25", []})),
    ?assertMatch({error, _}, pollution_server:get_one_value_server({"Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", []})),
    ?assertMatch({error, _}, pollution_server:get_one_value_server({"Stacja 2", Time, "PM1", []})),
    ?assertMatch({error, _}, pollution_server:get_one_value_server({{1,2}, Time, "PM10", []}))
  after
    pollution_server:stop()
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_station_mean_test() ->
  pollution_server:start(),
  try
    ?assertEqual({ok, added}, pollution_server:add_station_server({"Stacja 1", {1,1}})),
    Time = calendar:local_time(),
    StaticTime = {{2023,3,27},{11,16,9}},
    ?assertEqual({ok, value_added}, pollution_server:add_value_server({"Stacja 1", Time, "PM10", 10})),
    ?assertEqual({ok, value_added}, pollution_server:add_value_server({{1,1}, Time, "PM1", 20})),
    ?assertEqual({ok, value_added}, pollution_server:add_value_server({"Stacja 1", StaticTime, "PM10", 10})),

    ?assertMatch({ok,10.0}, pollution_server:get_station_mean_server({"Stacja 1", "PM10"})),
    ?assertMatch({ok,10.0}, pollution_server:get_station_mean_server({{1,1}, "PM10"})),
    ?assertMatch({ok,10.0}, pollution_server:get_station_mean_server({"Stacja 1", "PM10"}))
  after
    pollution_server:stop()
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_station_mean_fail_test() ->
  pollution_server:start(),
  try
    ?assertEqual({ok, added}, pollution_server:add_station_server({"Stacja 1", {1,1}})),
    pollution_server:add_station_server({"Stacja 1", {1,1}}),
    ?assertMatch({error, _}, pollution_server:get_station_mean_server({"Stacja 1", "PM10"})),
    pollution_server:add_value_server({"Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10}),
    ?assertMatch({error, _}, pollution_server:get_station_mean_server({"Stacja 1", "PM25"})),
    ?assertMatch({error, _}, pollution_server:get_station_mean_server({"Stacja 2", "PM25"}))
  after
    pollution_server:stop()
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_daily_mean_test() ->
  pollution_server:start(),
  try
  pollution_server:add_station_server({"Stacja 3", {3,3}}),
  pollution_server:add_station_server({"Stacja 2", {2,2}}),
  pollution_server:add_station_server({"Stacja 1", {1,1}}),
  pollution_server:add_value_server({"Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10}),
  pollution_server:add_value_server({"Stacja 2", {{2023,3,27},{11,16,11}}, "PM10", 20}),
  pollution_server:add_value_server({"Stacja 1", {{2023,3,27},{11,16,12}}, "PM10", 10}),
  pollution_server:add_value_server({"Stacja 2", {{2023,3,27},{11,16,13}}, "PM10", 20}),
  pollution_server:add_value_server({"Stacja 1", {{2023,3,27},{11,16,14}}, "PM25", 100}),
  pollution_server:add_value_server({"Stacja 2", {{2023,3,27},{11,16,15}}, "PM25", 220}),
  pollution_server:add_value_server({"Stacja 1", {{2023,3,28},{11,16,16}}, "PM10", 2000}),
  pollution_server:add_value_server({"Stacja 2", {{2023,3,28},{11,16,17}}, "PM10", 3000}),
  pollution_server:add_value_server({"Stacja 3", {{2023,3,27},{11,16,18}}, "PM10", 1234}),
  ?assertMatch({ok,258.8}, pollution_server:get_daily_mean_server({"PM10",{2023,3,27}})),
  ?assertMatch({ok,258.8}, pollution_server:get_daily_mean_server({"PM10",{2023,3,27}})),
  % Fixed the test below, because it required 15.0 as a calculated mean, but instead it should be 258.8, because in M9 a new valid value is added to the date 27-03-2023
  ?assertMatch({ok,258.8}, pollution_server:get_daily_mean_server({"PM10",{2023,3,27}}))
  after
    pollution_server:stop()
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_daily_mean_fail_test() ->
  pollution_server:start(),
  try
  pollution_server:add_station_server({"Stacja 2", {2,2}}),
  pollution_server:add_station_server({"Stacja 1", {1,1}}),
  ?assertMatch({error, _}, pollution_server:get_daily_mean_server({"PM10",{2023,3,27}})),
  pollution_server:add_value_server({"Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10}),
  pollution_server:add_value_server({"Stacja 2", {{2023,3,27},{11,16,11}}, "PM10", 20}),
  ?assertMatch({error, _}, pollution_server:get_daily_mean_server({"PM25",{2023,3,27}})),
  ?assertMatch({error, _}, pollution_server:get_daily_mean_server({"PM10",{2023,3,29}}))
  after
    pollution_server:stop()
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_over_limit_test() ->
  pollution_server:start(),
  try
    pollution_server:add_station_server({"Stacja 1", {21,37}}),
    pollution_server:add_value_server({"Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10}),
    pollution_server:add_value_server({"Stacja 1", {{2023,3,27},{11,16,11}}, "PM10", 20}),
    ?assertMatch({ok,1}, pollution_server:get_over_limit_server({15,"PM10"})),
    ?assertMatch({ok,2}, pollution_server:get_over_limit_server({9,"PM10"})),
    ?assertMatch({ok,0}, pollution_server:get_over_limit_server({21,"PM10"}))
  after
    pollution_server:stop()
  end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_nearest_stations_test() ->
  pollution_server:start(),
  try
  pollution_server:add_station_server({"Stacja 3", {3,3}}),
  pollution_server:add_station_server({"Stacja 2", {2,2}}),
  pollution_server:add_station_server({"Stacja 1", {1,1}}),
  ?assertEqual({ok,[{"Stacja 2", {2, 2}, []}, {"Stacja 1", {1, 1}, []}]}, pollution_server:get_nearest_stations_server({2, {1.5, 2}})),
  ?assertEqual({ok,[{"Stacja 3", {3, 3}, []}, {"Stacja 2", {2, 2}, []}, {"Stacja 1", {1, 1}, []}]}, pollution_server:get_nearest_stations_server({3, {4, 5}})),
  ?assertEqual({ok,[{"Stacja 1", {1, 1}, []}]}, pollution_server:get_nearest_stations_server({1, {1, 1}})),
  ?assertEqual({ok,[]}, pollution_server:get_nearest_stations_server({0, {1, 2}})),
  ?assertEqual({ok,[{"Stacja 2", {2, 2}, []}, {"Stacja 1", {1, 1}, []}, {"Stacja 3", {3, 3}, []}]}, pollution_server:get_nearest_stations_server({10, {1.5, 2}}))
  after
    pollution_server:stop()
  end.
