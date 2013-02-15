%%% $Id: float_bm.erl,v 1.5 2007/09/23 10:50:17 kostis Exp $
%%% from bjorn@erix.ericsson.se

-module(float_bm).
-export([main/1, compile/1]).

-compile([no_copt]).	%% shut off warnings about unused term constructions

main([]) ->
    Iter = 100,
    float_add(Iter),
    float_sub(Iter),
    float_mul(Iter),
    float_div(Iter).


compile(Options) ->
    hipe:c(?MODULE, Options).


%% benchmarks() ->
%%    {200000,[float_add,float_sub,float_mul,float_div]}.

float_add(Iter) ->
    float_add(Iter, 1.1, 3.1416).

float_add(0, _, _) -> ok;
float_add(Iter, A, B) when is_float(A), is_float(B) ->
    A+B,
    A+B,
    A+B,
    A+B,
    A+B,
    A+B,
    A+B,
    A+B,
    A+B,
    A+B,
    A+B,
    A+B,
    float_add(Iter-1, A, B).
    
float_sub(Iter) ->
    float_sub(Iter, 1.1, 3.1416).

float_sub(0, _, _) -> ok;
float_sub(Iter, A, B) when is_float(A), is_float(B) ->
    A-B,
    A-B,
    A-B,
    A-B,
    A-B,
    A-B,
    A-B,
    A-B,
    A-B,
    A-B,
    A-B,
    A-B,
    float_sub(Iter-1, A, B).

float_mul(Iter) ->
    float_mul(Iter, 1.1, 3.1416).

float_mul(0, _, _) -> ok;
float_mul(Iter, A, B) when is_float(A), is_float(B) ->
    A*B,
    A*B,
    A*B,
    A*B,
    A*B,
    A*B,
    A*B,
    A*B,
    A*B,
    A*B,
    A*B,
    A*B,
    float_mul(Iter-1, A, B).

float_div(Iter) ->
    float_div(Iter, 1.1, 3.1416).

float_div(0, _, _) -> ok;
float_div(Iter, A, B) when is_float(A), is_float(B) ->
    A/B,
    A/B,
    A/B,
    A/B,
    A/B,
    A/B,
    A/B,
    A/B,
    A/B,
    A/B,
    A/B,
    A/B,
    float_div(Iter-1, A, B).
