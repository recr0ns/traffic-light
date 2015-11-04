-module(dal).

-export([start_link/0]).
-export([init/1, close/0, store/1, remove/1, get_all/0, clear/0]).
-export([handle_cast/2, handle_call/3, terminate/2]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  %{ok, Path} = application:get_env(trafficLight, <<"data.dat">>),
  {ok, Ref} = dets:open_file(<<"data.dat">>, [{repair, true}]),
  {ok, Ref}.

close() ->
  gen_server:cast(?MODULE, close).

store(Data) ->
  gen_server:cast(?MODULE, {store, Data}).

remove(Key) ->
  gen_server:cast(?MODULE, {remove, Key}).

get_all() ->
  gen_server:call(?MODULE, get_all).

clear() ->
  gen_server:cast(?MODULE, clear).

handle_cast(close, State) ->
  dets:close(State),
  {stop, normal, State};

handle_cast({store, Data}, State) ->
  dets:insert(State, Data),
  {noreply, State};

handle_cast({remove, Key}, State) ->
  dets:delete(State, Key),
  {noreply, State};

handle_cast(clear, State) ->
  dets:delete_all_objects(State),
  {noreply, State}.

handle_call(get_all, _From, State) ->
  All = dets:foldl(fun(X, Acc) -> [X] ++ Acc end, [], State),
  {reply, All, State}.

terminate(normal, _) ->
  ok.
