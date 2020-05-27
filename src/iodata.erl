-module(iodata).

-export([
         split/2
        ]).


%%
%% iodata() = iolist() | binary()
%%
%% iolist() = maybe_improper_list(byte() | binary() | iolist(), binary() | [])
%%


split(N, Bin) when is_binary(Bin) ->
    case split_bin(N, Bin) of
        {_, _} = Split -> Split;
        _Size -> erlang:error(badarg)
    end;

split(N, IOList) ->
    case split_iolist(N, IOList, {0, []}) of
        {_, _} = Split -> Split;
        _Size -> erlang:error(badarg)
    end.



split_iolist(0, IOList, {_AccLength, Acc}) ->
    {lists:reverse(Acc), IOList};

split_iolist(_N, [], {AccLength, _Acc}) ->
    AccLength;

split_iolist(N, [Element | Tail], {AccLength, Acc}) ->
    case split_iolist_element(N, Element) of
        {Before, After} ->
            {lists:reverse(Acc, Before), [After | Tail]};
        Size ->
            case split_iolist_tail(N - Size, Tail) of
                {Before, After} ->
                    {lists:reverse(Acc, [Element, Before]), After};
                TSize ->
                    Size + TSize + AccLength
            end
    end.



split_iolist_element(N, Bin) when is_binary(Bin) ->
    split_bin(N, Bin);

split_iolist_element(N, Byte) when is_integer(Byte) ->
    case N of
        0 -> {[], [Byte]};
        1 -> {[Byte], []};
        _ -> 1
    end;

split_iolist_element(N, IOList) ->
    split_iolist(N, IOList, {0, []}).



split_iolist_tail(N, Bin) when is_binary(Bin) ->
    split_bin(N, Bin);

split_iolist_tail(N, IOList) ->
    split_iolist(N, IOList, {0, []}).



split_bin(N, Bin) ->
    Size = byte_size(Bin),
    case N > Size of
        true  -> Size;
        false ->
            <<Before:N/binary, After/binary>> = Bin,
            {Before, After}
    end.
