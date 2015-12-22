-module(eami_util).

-include("eami.hrl").

-export([encode_message/1, decode_message/1, get_value/2, parse/2]).
-export([to_list/1, to_binary/1]).

-define(EOL, <<"\r\n">>).
-define(EOM, <<"\r\n\r\n">>).

encode_message(Message) ->
    lists:foldr(
      fun({Key, Value}, Acc) ->
              [to_binary(Key), <<":">>, to_binary(Value), ?EOL | Acc]
      end, [?EOL], Message).

decode_message(Data) ->
    Lines = tokens(to_binary(Data), [?EOL]),
    lists:filtermap(
      fun(Line) ->
              case tokens(Line, [<<":">>, <<" ">>]) of
                  [Key, Value] -> {true, {Key, Value}};
                  _ -> false
              end
      end, Lines).

get_value(Key, Message) ->
    proplists:get_value(Key, Message).

-spec parse(binary(), binary()) -> {list(), binary()}.
-spec parse(binary(), binary(), list()) -> {list(), binary()}.
parse(Data, Fragment) ->
    parse(Data, Fragment, []).

parse(Data, Fragment, Messages) ->
    case binary:split(<<Fragment/binary, Data/binary>>, [?EOM]) of
        [RawMsg, Rest] ->
            parse(Rest, <<>>, [RawMsg | Messages]);
        [NewFragment] ->
            {Messages, NewFragment}
    end.

-spec to_list(atom() | list() | binary() | integer() | float()) -> list().
to_list(X) when is_list(X) -> X;
to_list(X) when is_binary(X) -> binary_to_list(X);
to_list(X) when is_atom(X) -> atom_to_list(X);
to_list(X) when is_integer(X) -> integer_to_list(X);
to_list(X) when is_float(X) -> mochinum:digits(X).

-spec to_binary(atom() | string() | binary() | integer() | float() | pid() | iolist()) -> binary().
to_binary(X) when is_binary(X) -> X;
to_binary(X) when is_list(X) -> iolist_to_binary(X);
to_binary(X) when is_atom(X) -> list_to_binary(atom_to_list(X));
to_binary(X) when is_pid(X) -> to_binary(pid_to_list(X));
to_binary(X) when is_integer(X) -> list_to_binary(integer_to_list(X));
to_binary(X) when is_float(X) -> to_binary(mochinum:digits(X)).

tokens(Bin, Pattern) ->
    binary:split(Bin, Pattern, [global, trim_all]).
