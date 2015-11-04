-module(observe_processor_test).

-include_lib("eunit/include/eunit.hrl").

-define(INITIAL_STATE, #{nums=>[], err1=>[], err2=>[]}).

single_correct_version_test() ->
  ?assertEqual({success, #{nums => [88], err1 => [0,0,0,0,0,0,0], err2 => [0,0,0,0,0,0,0]}},
                  observe_processor:observe([[1,1,1,1,1,1,1], [1,1,1,1,1,1,1]], 0, ?INITIAL_STATE)),
  ok.

single_correct_version_with_errors_test() ->
  ?assertEqual({success, #{nums => [88], err1 => [0,1,0,0,0,0,0], err2 => [0,1,0,0,0,0,0]}},
                  observe_processor:observe([[1,0,1,1,1,1,1], [1,0,1,1,1,1,1]], 0, ?INITIAL_STATE)),
  ok.

multiple_steps_test() ->
  {Res1, State1} = observe_processor:observe([[1,1,1,0,1,1,1], [0,0,1,1,1,0,1]], 0, ?INITIAL_STATE),
  ?assertEqual({continue, #{nums=>[2,8,82,88], err1=>[0,0,0,0,0,0,0], err2=>[1,0,0,0,0,0,0]}}, {Res1, State1}),
  {Res2, State2} = observe_processor:observe([[1,1,1,0,1,1,1], [0,0,1,0,0,0,0]], 1, State1),
  ?assertEqual({continue, #{nums=>[2,8,82,88], err1=>[0,0,0,0,0,0,0], err2=>[1,0,0,0,0,1,0]}}, {Res2, State2}),
  {Res3, State3} = observe_processor:stop_observe(2, State2),
  ?assertEqual({success, #{nums=>[2], err1=>[0,0,0,0,0,0,0], err2=>[1,0,0,0,0,1,0]}}, {Res3, State3}),
  ok.

no_solution_test() ->
  {Res1, State1} = observe_processor:observe([[1,1,1,0,1,1,1], [0,0,1,1,1,0,1]], 0, ?INITIAL_STATE),
  ?assertEqual({continue, #{nums => [2,8,82,88], err1 => [0,0,0,0,0,0,0], err2 => [1,0,0,0,0,0,0]}}, {Res1, State1}),
  {Res2, State2} = observe_processor:stop_observe(1, State1),
  ?assertEqual({error, not_found}, {Res2, State2}),
  ok.
