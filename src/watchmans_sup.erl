-module(watchmans_sup).

-export([start_link/0]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, transient, infinity, Type, [I]}).

start_link() ->
  supervisor:start_link({local, watchmans}, ?MODULE, []).

init(_) ->
  AChild = ?CHILD(watchman, worker),
  Strategy = {simple_one_for_one, 10, 100},
  {ok, {Strategy, [AChild]}}.
