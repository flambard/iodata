-module(prop_iodata).

-include_lib("proper/include/proper.hrl").

-export([
         prop_split_like_binary_naive/0,
         prop_split_like_binary/0,
         prop_measure_time_difference/0
        ]).

prop_split_like_binary_naive() ->
    ?FORALL(Data, iodata(),
            ?FORALL(N, integer(0, iolist_size(Data)),
                    begin
                        <<A:N/binary, B/binary>> = iolist_to_binary(Data),
                        {X, Y} = naive_split(N, Data),
                        {A, B} =:= {iolist_to_binary(X), iolist_to_binary(Y)}
                    end)).

prop_split_like_binary() ->
    ?FORALL(Data, iodata(),
            ?FORALL(N, integer(0, iolist_size(Data)),
                    begin
                        <<A:N/binary, B/binary>> = iolist_to_binary(Data),
                        {X, Y} = iodata:split(N, Data),
                        {A, B} =:= {iolist_to_binary(X), iolist_to_binary(Y)}
                    end)).


prop_measure_time_difference() ->
    ?FORALL(Data, iodata(),
            ?FORALL(N, integer(0, iolist_size(Data)),
                    begin
                        {SlowTime, _} =
                            timer:tc(fun() -> naive_split(N, Data) end),
                        {FastTime, _} =
                            timer:tc(fun() -> iodata:split(N, Data) end),
                        Diff = SlowTime - FastTime,
                        Class = if Diff < 0   -> slower;
                                   Diff =:= 0 -> same;
                                   true       -> faster
                                end,
                        collect(Class, true)
                    end)).


naive_split(N, IOData) ->
    case N > iolist_size(IOData) of
        true  -> erlang:error(badarg);
        false ->
            <<Before:N/binary, After/binary>> = iolist_to_binary(IOData),
            {Before, After}
    end.
