-module(controller).

-export([start_link/0]).
-export([alloc_watchman/0, clear/0, add_observation/3, state/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-include("../include/colors.hrl").

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  restart_watchmans(dal:get_all(), maps:new()).

alloc_watchman() ->
  gen_server:call(?MODULE, alloc).

clear() ->
  gen_server:cast(?MODULE, clear).

add_observation(Uid, Color, Numbers) ->
  gen_server:call(?MODULE, {observe, Uid, Color, Numbers}).

state() ->
  gen_server:call(?MODULE, state).


handle_call(alloc, _From, State) ->
  Uid = uuid:to_string(uuid:uuid1()),
  {ok, Pid} = supervisor:start_child(watchmans, [[Uid]]),
  State1 = maps:put(Uid, Pid, State),
  {reply, {ok, Uid}, State1};

handle_call({observe, Uid, ?GREEN, Numbers}, _From, State) ->
  case maps:find(Uid, State) of
    {ok, Pid} -> handle_watch(watchman:watch(Pid, ?GREEN, Numbers), Uid, Pid, State);
    error -> {reply, {error, sequence_not_found}, State}
  end;

handle_call({observe, Uid, ?RED, _}, _From, State) ->
  case maps:find(Uid, State) of
    {ok, Pid} -> handle_watch(watchman:watch(Pid, ?RED), Uid, Pid, State);
    error -> {reply, {error, sequence_not_found}, State}
  end;

handle_call(state, _From, State) ->
  {reply, State, State}.

handle_cast(clear, State) ->
  dal:clear(),
  lists:foreach(fun({_, Pid}) -> watchman:stop(Pid) end, maps:to_list(State)),
  {reply, ok, maps:new()}.

terminate(normal, _) ->
  ok.


restart_watchmans([], Map) -> {ok, Map};
restart_watchmans([{Uid, State} | Tail], Map) ->
  {ok, Pid} = supervisor:start_child(watchmans, [[Uid, State]]),
  Map1 = maps:put(Uid, Pid, Map),
  restart_watchmans(Tail, Map1).

handle_watch(Watch, Uid, Pid, State) ->
  case Watch of
    {continue, Result} ->
      {reply, {success, Result}, State};

    %Response = success || error
    {Response, Result} ->
      State1 = terminate_watchman(Uid, Pid, State),
      {reply, {Response, Result}, State1}
  end.

terminate_watchman(Uid, Pid, State) ->
  watchman:stop(Pid),
  maps:remove(Uid, State).
