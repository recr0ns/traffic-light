-module(json_decoder).

-export([parse_observation/1]).

-include("../include/errors.hrl").
-include("../include/html_formats.hrl").

parse_observation(Json) ->
  Data = jiffy:decode(Json, [return_maps]),
  try map_request(Data, observe) of
    Request ->
      io:format("Req ~p~n", [Request]),
      {ok, Request}
  catch
    _:_ -> {error, ?INCORRECT_FORMAT}
  end.

map_request(Data, observe) ->
  #{
    sequence => maps:get(?SEQUENCE, Data),
    observation => map_observation(maps:get(?OBSERVATION, Data))
  }.

map_observation(Obs) ->
  #{ color => maps:get(?COLOR, Obs), numbers => prepare_numbers(maps:get(?NUMBERS, Obs, [])) }.

prepare_numbers(Nums) ->
  lists:map(fun num_to_array/1, Nums).

num_to_array(Num) ->
  Array = lists:map(fun transform/1, binary_to_list(Num)),
  case length(Array) of
    7 -> Array;
    _ -> throw(incorrect_format)
  end.

transform(N) ->
  case N of
    48 -> 0;
    49 -> 1;
    _ -> throw(incorrect_format)
  end.
