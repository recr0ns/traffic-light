-module(watchman).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([watch/3, watch/2, stop/1]).

-include("../include/colors.hrl").

-define(EMPTY_STATE, #{step => 0, data => #{nums => [], err1 => [], err2 => []}}).

start_link(Opts) ->
  gen_server:start_link(?MODULE, Opts, []).

init([Uid, State]) ->
  {ok, {Uid, State}};

init([Uid]) ->
  State = {Uid, ?EMPTY_STATE},
  dal:store(State),
  {ok, State}.

watch(Pid, ?GREEN, Numbers) ->
  gen_server:call(Pid, {watch, ?GREEN, Numbers}).

watch(Pid, ?RED) ->
  gen_server:call(Pid, {watch, ?RED, []}).

stop(Pid) ->
  gen_server:cast(Pid, close).


handle_call({watch, ?GREEN, Numbers}, _From, {Uid, #{step := Step, data := Data}}) ->
  Result = observe_processor:observe(Numbers, Step, Data),
  case Result of
    {error, Type} -> {reply, {error, Type}, {Uid, #{}}};
    {_, S} ->
      NewState = {Uid, #{step => Step+1, data => S}},
      dal:store({Uid, NewState}),
      {reply, Result, NewState}
  end;

handle_call({watch, ?RED, []}, _From, {Uid, #{step := Step, data := Data}}) ->
  Result = observe_processor:stop_observe(Step, Data),
  case Result of
    {error, Type} -> {reply, {error, Type}, {Uid, #{}}};
    {success, State} -> {reply, {success, State}, {Uid, #{}} }
  end.

handle_cast(close, State) ->
  {stop, normal, State}.

terminate(normal, _) ->
  ok.
