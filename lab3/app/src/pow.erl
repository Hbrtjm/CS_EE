%%%-------------------------------------------------------------------
%%% @author user
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Apr 2025 12:24
%%%-------------------------------------------------------------------
%%%

-module(pow).
-behaviour(gen_server).

%% API
-export([start_link/0, step/0, read/0, close/0, crash/0, set/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

%% START %%
start_link() ->   gen_server:start_link({local, ?MODULE}, ?MODULE, 2, []).

init(N) ->
    {ok, N}.

%% CLIENT INTERFACE %%
step() 		->	gen_server:cast(?MODULE, step).
read() 		->	gen_server:call(?MODULE, read).
close() 	-> 	gen_server:call(?MODULE, terminate).
crash() 	-> 	gen_server:cast(?MODULE, crash).
set(N) 		->      gen_server:cast(?MODULE, {set, N}).

%% SERVER CALLBACKS %%
handle_cast(step, N) 		->	{noreply, N * N};
handle_cast(crash, N) 		->	erlang:error(no_exist), {noreply, N};
handle_cast({set, N}, _OldN) 	-> 	{noreply, N}.

handle_call(read, _From, N) 	->	{reply, N, N};
handle_call(terminate, _From, N) -> 	{stop, normal, ok, N};
handle_call(_Message, _From, N)  -> 	{reply, unknown_call, N}.

terminate(normal, N) ->	io:format("The number is: ~B~nBye.~n", [N]), ok.

