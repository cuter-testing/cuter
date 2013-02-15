-module(bs_bm).
-export([main/1,lists/1,bs/1,partly_bs/1]).

main([]) ->
    Iter = 100,
    lists(Iter),
    bs(Iter),
    partly_bs(Iter).

lists(Iter) ->
    lists(Iter, data()).

lists(0, _List) -> ok;
lists(Iter, List) ->
    lists1(List, []),
    lists(Iter-1, List).

lists1([{X,Y,Z}|T], Acc) ->
    lists1(T, [i32(X),i32(Y),i32(Z)|Acc]);
lists1([], Acc) -> list_to_binary(lists:reverse(Acc)).

i32(Int)->
    [(Int bsr 24) band 255,
     (Int bsr 16) band 255,
     (Int bsr  8) band 255,
     Int band 255].

bs(Iter) ->
    bs(Iter, data()).

bs(0, _Data) -> ok;
bs(Iter, Data) ->
    bs1(Data, <<>>),
    bs(Iter-1, Data).

bs1([{X,Y,Z}|T], Acc) ->
    bs1(T, <<Acc/binary,X:32,Y:32,Z:32>>);
bs1([], Acc) -> Acc.

partly_bs(Iter) ->
    partly_bs(Iter, data()).

partly_bs(0, _Data) -> ok;
partly_bs(Iter, Data) ->
    partly_bs1(Data, []),
    partly_bs(Iter-1, Data).

partly_bs1([{X,Y,Z}|T], Acc) ->
    partly_bs1(T, [<<X:32,Y:32,Z:32>>|Acc]);
partly_bs1([], Acc) -> list_to_binary(lists:reverse(Acc)).

data() ->
    lists:duplicate(64, {3455,133,88888}).


