-module(bin_to_term_bm).
-export([main/1,t_binary_to_term/1]).

main([]) ->
    t_binary_to_term(100).


t_binary_to_term(Iter) ->
    Term = {a,{nested,tuple,is},nice,lists:seq(-1, 10),33,self()},
    t_binary_to_term(Iter, term_to_binary(Term), lists:duplicate(1024, 255)).

t_binary_to_term(0, _Bin, _T) -> ok;
t_binary_to_term(Iter, Bin, T) ->
    binary_to_term(Bin),
    t_binary_to_term(Iter-1, Bin, T).
