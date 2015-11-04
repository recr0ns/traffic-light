-module(trafficLight_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(APPS, [crypto, ranch, cowlib, cowboy]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_, _) ->
  ok = ensure_started(?APPS),
  ok = sync:go(),
  trafficLight_sup:start_link().

stop(_) ->
  %sync:stop(),
  dal:close(),
	%ok = stop_apps(lists:reverse(?APPS)),
  ok.

ensure_started([]) -> ok;
ensure_started([App | Apps]) ->
	case application:start(App) of
		ok -> ensure_started(Apps);
		{error, {already_started, App}} -> ensure_started(Apps);
    Res ->
      io:format("~p : ~p~n", [App, Res]),
      ensure_started(Apps)
	end.

stop_apps([]) -> ok;
stop_apps([App | Apps]) ->
	application:stop(App),
	stop_apps(Apps).
