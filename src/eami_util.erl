-module(eami_util).

-export([encode_message/1, decode_message/1, get_value/2]).
-export([to_list/1]).

-define(EOL, "\r\n").

encode_message(Message) ->
    lists:foldr(
      fun({Key, Value}, Acc) ->
              [to_list(Key), ":", to_list(Value), ?EOL | Acc]
      end, [], Message).

decode_message(Raw) ->
    Lines = string:tokens(Raw, ?EOL),
    lists:filtermap(
      fun(Line) ->
              case string:tokens(Line, ": ") of
                  [Key, Value] -> {true, {Key, Value}};
                  _ -> false
              end
      end, Lines).

get_value(Key, Message) ->
    proplists:get_value(Key, Message).


-spec to_list(atom() | list() | binary() | integer() | float()) -> list().
to_list(X) when is_list(X) -> X;
to_list(X) when is_binary(X) -> binary_to_list(X);
to_list(X) when is_atom(X) -> atom_to_list(X);
to_list(X) when is_float(X) -> mochinum:digits(X);
to_list(X) when is_integer(X) -> integer_to_list(X).
