-module(numbers_comparer).

-export([is_maybe/1]).

% Compare number with patterns
is_maybe(Number) ->
  Nums = [[1,1,1,0,1,1,1],
          [0,0,1,0,0,1,0],
          [1,0,1,1,1,0,1],
          [1,0,1,1,0,1,1],
          [0,1,1,1,0,1,0],
          [1,1,0,1,0,1,1],
          [1,1,0,1,1,1,1],
          [1,0,1,0,0,1,0],
          [1,1,1,1,1,1,1],
          [1,1,1,1,0,1,1]],
  iterate(Number, Nums, 0, []).

iterate(_, [], _, Res) -> Res;
iterate(Num, [Current|Tail], Index, Res) ->
  case is_maybe(Num, Current) of
    false -> iterate(Num, Tail, Index+1, Res);
    Mask -> iterate(Num, Tail, Index+1, Res ++ [{Index, Mask}])
  end.

is_maybe(Number, Pattern) ->
  compare(Number, Pattern, []).

compare([],[], Res) -> Res;
compare([CurrentDigit | NumberTail], [CurrentPatternDigit | PatternTail], Res) ->
  case compare(CurrentDigit, CurrentPatternDigit) of
    {ok, Code} -> compare(NumberTail, PatternTail, Res ++ [Code]);
    false -> false
  end.

compare(Digit, PatternDigit) ->
  case {Digit, PatternDigit} of
    {0,1} -> {ok, 1};
    {D,D} -> {ok, 0};
    _ -> false
  end.
