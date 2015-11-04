-module(server).

-export([start_link/0]).
-export([init/1, terminate/2]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/clear", watchman_handler, clear},
      {"/sequence/create", watchman_handler, alloc},
      {"/observation/add", watchman_handler, observe},
      {"/ping", watchman_handler, ping}
    ]}
  ]),

  cowboy:start_http(trafficLight, 100,
    [{port, application:get_env(trafficLight, app_port, 8090)}],
    [{env, [{dispatch, Dispatch}]}]),

  {ok, []}.

terminate(normal, _) ->
  ok.
