-module(numbers_comparer_test).

-include_lib("eunit/include/eunit.hrl").

is_maybe_test() ->
  ?assertEqual([{0, [0,0,0,0,0,0,0]}, {8, [0,0,0,1,0,0,0]}], numbers_comparer:is_maybe([1,1,1,0,1,1,1])),
  ?assertEqual([{6, [0,0,0,0,0,1,0]}, {8, [0,0,1,0,0,1,0]}], numbers_comparer:is_maybe([1,1,0,1,1,0,1])),
  ?assertEqual([{8, [0,0,0,0,1,1,1]}, {9, [0,0,0,0,0,1,1]}], numbers_comparer:is_maybe([1,1,1,1,0,0,0])),
  ?assertEqual([{2, [0,0,1,0,1,0,0]},
                {3, [0,0,1,0,0,1,0]},
                {5, [0,1,0,0,0,1,0]},
                {6, [0,1,0,0,1,1,0]},
                {8, [0,1,1,0,1,1,0]},
                {9, [0,1,1,0,0,1,0]}], numbers_comparer:is_maybe([1,0,0,1,0,0,1])),
  ok.
