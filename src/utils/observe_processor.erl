-module(observe_processor).
-export([observe/3, stop_observe/2]).

% =====================================================================
% Public API
% =====================================================================

observe(Observation, Step, State) ->
  R = append_observation(Observation, Step, State),
  check_solution(R).

stop_observe(0, _) ->
  {error, not_enough_data};
  
stop_observe(Step, #{nums := Started, err1 := Err1, err2 := Err2}) ->
  case lists:any(fun(X) -> X =:= Step end, Started) of
    true -> {success, #{nums=>[Step], err1 => Err1, err2 => Err2}};
    false -> {error, not_found}
  end.

% =====================================================================
% Private
% =====================================================================

% First observation
append_observation([First, Second], 0, #{nums := [], err1 := [], err2 := []}) ->
  {Nums, Err1, Err2} = intersection(get_set(First, Second), [], 0),
  #{nums => Nums, err1 => Err1, err2 => Err2};

append_observation([First, Second], Step, #{nums := Started, err1 := Err1, err2 := Err2}) ->
  {Nums, NErr1, NErr2} = intersection(get_set(First, Second), Started, Step),
  FilterdStarted = filter_started(Started, Nums, Step),
  AppendedErr1 = append_errors(Err1, NErr1),
  AppendedErr2 = append_errors(Err2, NErr2),
  #{nums => FilterdStarted, err1 => AppendedErr1, err2 => AppendedErr2}.


check_solution(I = #{nums := Nums}) ->
  case length(Nums) of
    0 -> {error, not_found};
    1 -> {success, I};
    _ -> {continue, I}
  end.

intersection(Values, [], 0) ->
  join(Values);

intersection(Values, Origin, Step) ->
  Res = lists:filter(fun({V, _, _}) -> lists:any(fun(X) -> X-Step > 0 andalso X-Step =:= V end, Origin) end, Values),
  join(Res).

filter_started(Started, New, Step) ->
  lists:filter(fun(S) -> lists:any(fun(N)-> S-Step =:= N end, New) end, Started).

get_set(First, Second) ->
  P1 = numbers_comparer:is_maybe(First),
  P2 = numbers_comparer:is_maybe(Second),
  [{10*X+Y, E1, E2} || {X, E1} <- P1, {Y, E2} <- P2].

join(N) ->
  {E1, E2} = merge_errors(N),
  {lists:map(fun({X, _, _}) -> X end, N), E1, E2}.

append_errors(Origin, Append) ->
  lists:zipwith(fun append_zip/2, Origin, Append).

append_zip(X,Y) ->
  case {X,Y} of
    {0,0} -> 0;
    _ -> 1
  end.

merge_errors(Errors) ->
  lists:foldl(fun({_, Err1, Err2}, {Acc1, Acc2})-> {lists:zipwith(fun merge_zip/2, Err1, Acc1), lists:zipwith(fun merge_zip/2, Err2, Acc2)} end, {[1,1,1,1,1,1,1], [1,1,1,1,1,1,1]}, Errors).

merge_zip(X,Y) ->
  case {X,Y} of
    {A,A} -> A;
    _ -> 0
  end.
