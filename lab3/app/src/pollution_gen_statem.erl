-module(pollution_value_collector_gen_statem).
-behaviour(gen_statem).

%% API
-export([start_link/0, stop/0, set_station/1, add_value/3, store_data/0]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, handle_event/4, terminate/3]).

-record(state, {
  station = undefined :: term(),
  buffer = []      :: [{Type, DateTime, Value}]
}).

%%% Public API
start_link() ->
  gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_statem:stop(?MODULE).

set_station(Station) ->
  gen_statem:call(?MODULE, {set_station, Station}).

add_value(Type, DateTime, Value) ->
  gen_statem:call(?MODULE, {add, Type, DateTime, Value}).

store_data() ->
  gen_statem:call(?MODULE, store).

%%% gen_statem impl.
callback_mode() -> state_functions.

init([]) ->
  {ok, idle, #state{}}.

%% -- stan IDLE ------------------------------------------------
idle({call, From}, {set_station, Station}, StateRecord) ->
  NewState = StateRecord#state{station = Station, buffer = []},
  {next_state, collecting, NewState, [{reply, From, ok}]}.

idle({call, From}, _, State) ->
  {keep_state_and_data, [{reply, From, {error, not_ready}}]}.

%% -- stan COLLECTING -----------------------------------------
collecting({call, From}, {add, Type, Dt, Val}, StateRecord) ->
  NewBuf = [{Type, Dt, Val} | StateRecord#state.buffer],
  NewState = StateRecord#state{buffer = NewBuf},
  {keep_state, NewState, [{reply, From, ok}]}.

collecting({call, From}, store, #state{station = S, buffer = Buf} = SR) ->
  %% Wyślij batch do gen_server:
  lists:foreach(fun({T,D,V}) ->
    pollution_gen_server:add_value(S, T, D, V)
  end, lists:reverse(Buf)),
  %% wyczyść i wróć do IDLE
  NewState = SR#state{station = undefined, buffer = []},
  {next_state, idle, NewState, [{reply, From, ok}]}.

collecting({call, From}, {set_station, _}, State) ->
  {keep_state_and_data, [{reply, From, {error, already_collecting}}]}.

collecting({call, From}, store, State) ->
  {keep_state_and_data, [{reply, From, {error, no_station_set}}]}.

%% -- domyślny handler --------------------------------------
handle_event(_Type, _Event, _StateName, StateData) ->
  {keep_state_and_data, []}.

terminate(_Reason, _StateName, _StateData) ->
  ok.

