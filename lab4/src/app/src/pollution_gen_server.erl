-module(pollution_gen_server).
-behaviour(gen_server).

%% Public API
-export([
  start_link/0,
  close/0,
  add_station/2,
  add_value/4,
  remove_value/3,
  get_one_value/3,
  get_station_min/2,
  get_over_limit/2,
  get_station_mean/2,
  get_daily_mean/2,
  get_nearest_stations/2,
  get_state/0,
  crash/0	
]).

-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  terminate/2
]).


%% Dependencies
-import(pollution, [
  create_monitor/0, 
  add_station/3,
  add_value/5,
  remove_value/4,
  get_one_value/4,
  get_station_min/3,
  get_over_limit/3,
  get_station_mean/3,
  get_daily_mean/3,
  get_nearest_stations/3
]).


%%% ================
%%% Client API
%%% ================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

close() ->
  gen_server:call(?MODULE, terminate).

add_station(Name, Coordinates) ->
  gen_server:call(?MODULE, {add_station, Name, Coordinates}).

%% remove_station(CoordinatesOrName) -> 
%%  gen_server:call(?MODULE, {remove_station, CoordinatesOrName}).

add_value(CoordinatesOrName, DateTime, Type, Value) ->
  gen_server:call(?MODULE, {add_value, CoordinatesOrName, DateTime, Type, Value}).

remove_value(CoordinatesOrName, DateTime, Type) ->
  gen_server:call(?MODULE, {remove_value, CoordinatesOrName, DateTime, Type}).

get_one_value(CoordinatesOrName, DateTime, Type) -> 
  gen_server:call(?MODULE, {get_one_value, CoordinatesOrName, DateTime, Type}).

get_station_min(CoordinatesOrName, Type) ->
  gen_server:call(?MODULE, {get_station_min, CoordinatesOrName, Type}).

get_over_limit(Limit, Type) ->
  gen_server:call(?MODULE, {get_over_limit, Limit, Type}).

get_station_mean(CoordinatesOrName, Type) ->
  gen_server:call(?MODULE, {get_station_mean, CoordinatesOrName, Type}).

get_daily_mean(Date, Type) ->
  gen_server:call(?MODULE, {get_daily_mean, Date, Type}).

get_nearest_stations(Amount, Position) ->
  gen_server:call(?MODULE, {get_nearest_stations, Amount, Position}).

get_state() ->
  gen_server:call(?MODULE, get_state).

%%% ===================
%%% Server Callbacks
%%% ===================

init([]) ->
  {ok, create_monitor()}.

crash() -> gen_server:call(?MODULE, crash).

handle_call(crash, _From, State) -> 
	exit(panic),
	{noreply, State};

handle_call(get_state, _From, State) ->
  {reply, {ok, State}, State};

handle_call({add_station, Name, Coordinates}, _From, State) ->
  case add_station(Name, Coordinates, State) of
    {error, Msg} ->
      {reply, {error, Msg}, State};
    NewState ->
      {reply, {ok, added}, NewState}
  end;

%% handle_call({remove_station, CoordinatesOrName}, _From, State) ->
%% 	case remove_station(CoordinatesOrName, State) of
%% 		{error, Msg} ->
%% 			{reply, {error, Msg}, State};
%% 		NewState ->
%% 			{reply, {ok, removed}, NewState}
%% 	end;

handle_call({add_value, CoordinatesOrName, DateTime, Type, Value}, _From, State) ->
	case add_value(CoordinatesOrName, DateTime, Type, Value, State) of
		{error, Msg} ->
			{reply, {error, Msg}, State};
		NewState ->
			{reply, {ok, added}, NewState}
	end;

handle_call({remove_value, CoordinatesOrName, DateTime, Type}, _From, State) ->
	case remove_value(CoordinatesOrName, DateTime, Type, State) of
		{error, Msg} ->
			{reply, {error, Msg}, State};
		NewState ->
			{reply, {ok, removed}, NewState}
	end;

handle_call({get_one_value, CoordinatesOrName, DateTime, Type}, _From, State) ->
	case get_one_value(CoordinatesOrName, DateTime, Type, State) of
		{error, Msg} ->
			{reply, {error, Msg}, State};
		Value ->
			{reply, {ok, Value}, State}
	end;



handle_call({get_station_min, CoordinatesOrName, Type}, _From, State) ->
	case get_station_min(CoordinatesOrName, Type, State) of
		{error, Msg} ->
			{reply, {error, Msg}, State};
		Value ->
			{reply, {ok, Value}, State}
	end;

handle_call({get_over_limit, Limit, Type}, _From, State) ->
	case get_over_limit(Limit, Type, State) of
		{error, Msg} ->
			{reply, {error, Msg}, State};
		Values ->
			{reply, {ok, Values}, State}
	end;

handle_call({get_nearest_stations, Amount, Position}, _From, State) ->
	case get_nearest_stations(Amount, Position, State) of
		{error, Msg} ->
			{reply, {error, Msg}, State};
		Stations->
			{reply, {ok, Stations}, State}
	end;

handle_call({get_station_mean, CoordinatesOrName, Type}, _From, State) ->
	case get_station_mean(CoordinatesOrName, Type, State) of
		{error, Msg} ->
			{reply, {error, Msg}, State};
		Mean ->
			{reply, {ok, Mean}, State}
	end;

handle_call({get_daily_mean, Date, Type}, _From, State) ->
	case get_daily_mean(Date, Type, State) of
		{error, Msg} ->
			{reply, {error, Msg}, State};
		Mean ->
			{reply, {ok, Mean}, State}
	end;

handle_call(terminate, _From, State) ->
  {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
  {reply, {error, unknown_command}, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  io:format("Server terminated.~n"),
  ok.
