-module(response_processor).

-export([success/2, error/2, observe/2, array_to_num/1]).

-include("../include/errors.hrl").
-include("../include/html_formats.hrl").

%% Public API

success(clear, Req) ->
  success(ok, Req);

success({alloc, Uid}, Req) ->
  success(#{sequence => list_to_binary(Uid)}, Req);

success(Data, Req) ->
  wrap_body(wrap_success(Data), Req).

error(Message, Req) ->
  wrap_body(wrap_error(Message), Req).

observe({error, sequence_not_found}, Req) ->
  error(?NO_SEQUENCE, Req);

observe({error, not_enough_data}, Req) ->
  error(?NOT_ENOUGH_DATA, Req);

observe({success, #{nums := Nums, err1 := Err1, err2 := Err2}}, Req) ->
  Result = #{start => Nums, missing => [list_to_binary(array_to_num(Err1)), list_to_binary(array_to_num(Err2))]},
  io:format("~p~n", [Result]),
  success(Result, Req).


%% Private API

wrap_success(Data) ->
  SuccessResponse = #{status => ok, response => Data},
  jiffy:encode(SuccessResponse).

wrap_error(Message) ->
  ErrorResponse = #{status => error, msg => Message},
  jiffy:encode(ErrorResponse).

wrap_body(Body, Req) ->
  {ok, cowboy_req:reply(200, [{?CONTENT_TYPE, ?JSON}], Body, Req), []}.

array_to_num(Arr) ->
  string:join(io_lib:format("~p~p~p~p~p~p~p", Arr), "").
