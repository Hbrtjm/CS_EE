%%%-------------------------------------------------------------------
%%% @author user
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Mar 2025 13:28
%%%-------------------------------------------------------------------
-module(quicksort).
-author("user").

%% API
-export([qs/1,random_elems/3,compare_speeds/3,ordered_list/1,reverse/1]).

less_than(List, Arg) -> [ Value || Value <- List, Value < Arg ].

grt_eq_than(List, Arg) -> [Value || Value <- List, Value >= Arg].

qs([Elem]) -> [Elem];
qs([Pivot|Tail]) -> qs(less_than(Tail,Pivot)) ++ [Pivot] ++ qs(grt_eq_than(Tail,Pivot));
qs([]) -> [].

%%merge([],List) -> List;
%%merge(List,[]) -> List;
%%merge([First|FRest],[Second|SRest]) ->
%%  case First > Second of
%%      true  -> [Second] ++ merge(FRest,[Second|SRest]);
%%      false -> [First] ++ merge([First|FRest],SRest)
%%  end
%%.

%%merge_sort([]) -> [];
%%merge_sort([Elem]) -> [Elem];
%%merge_sort([Left,Right]) -> ;
%%merge_sort([Left|Right]) -> merge_sort() ++ merge_sort().


pick_a_value_undef(Min, Max) when Min =< Max -> rand:uniform(Max - Min + 1) + Min.

random_elems(0,_,_) -> [];
random_elems(N,Min,Max) -> [pick_a_value_undef(Min,Max)|random_elems(N-1,Min,Max)].

%random_elems(N,Min,Max) -> [ pick_a_value_undef(Min,Max) || _ <- lists:seq(N) ]

reverse([]) -> [];
reverse([Elem]) -> [Elem];
reverse([H|T]) -> reverse(T) ++ [H].

ordered_list(N) -> reverse([ Value || Value <- lists:seq(1,N) ]).

compare_speeds(List, Fun1, Fun2) ->
  {A,_} = timer:tc(Fun1,[List]),
  {B,_} = timer:tc(Fun2,[List]),
  {A,B, A/B}.

