-module(bs_simple_bm).
-export([main/1,lists/1,bs/1,bs_bif/1]).

main([]) ->
    Iter = 100,
    lists(Iter),
    bs(Iter),
    bs_bif(Iter).


lists(Iter) ->
    lists(Iter, data()).

lists(0, _Data) -> ok;
lists(Iter, Data) ->
    lists1(Data, []),
    lists(Iter-1, Data).

lists1(0, Acc) -> list_to_binary(Acc);
lists1(Int, Acc) -> lists1(Int bsr 8, [Int band 255|Acc]).

bs(Iter) ->
    bs(Iter, data()).

bs(0, _Data) -> ok;
bs(Iter, Data) ->
    bs1(Data, <<>>),
    bs(Iter-1, Data).

bs1(0, Acc) -> Acc;
bs1(Int, Acc) ->
    bs1(Int bsr 8, <<Int,Acc/binary>>).

bs_bif(Iter) ->
    bs_bif(Iter, data()).

bs_bif(0, _Data) -> ok;
bs_bif(Iter, Data) ->
    bs_bif1(Data),
    bs_bif(Iter-1, Data).

bs_bif1(Data) ->
    N = bytes_needed(Data),
    <<Data:N/unit:8>>.

bytes_needed(N) when is_integer(N) ->
%    33.
    4.

data() ->
    16#ffeeddcc.

% data() ->
%     378797428387894872987432987176554383987249723498274928347298473444444444444444.
