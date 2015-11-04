-module(watchman_handler).

-export([init/2]).
-export([allowed_methods/2]).

-include("../../include/html_formats.hrl").

allowed_methods(Req, State)->
  {[?POST], Req, State}.

init(Req, clear) ->
  controller:clear(),
  response_processor:success(clear, Req);

init(Req, alloc) ->
  {ok, Uid} = controller:alloc_watchman(),
  response_processor:success({alloc, Uid}, Req);

init(Req, observe) ->
  {ok, Body, _} = cowboy_req:body(Req),
  case json_decoder:parse_observation(Body) of
    {error, Data} -> response_processor:error(Data, Req);
    {ok, #{sequence := Uid, observation := #{color := Color, numbers := Numbers}}} ->
      Result = controller:add_observation(binary_to_list(Uid), Color, Numbers),
      response_processor:observe(Result, Req)
  end;

init(Req, ping) ->
  response_processor:success(pong, Req).
