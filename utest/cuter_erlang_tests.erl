%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_erlang_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_config.hrl").

-spec test() -> ok | {error | term()}. %% Silence dialyzer warning

-spec props_test_() -> any().
props_test_() ->
  Props = [
    {"erlang:atom_to_list/1 => cuter_erlang:atom_to_list/1", prop_atom_to_list(), 1000}
  , {"erlang:'<'/2 => cuter_erlang:'<'/2", prop_lt(), 2000}
  , {"erlang:'=<'/2 => cuter_erlang:'=<'/2", prop_lteq(), 2000}
  , {"erlang:'>'/2 => cuter_erlang:'>'/2", prop_gt(), 2000}
  , {"erlang:'>='/2 => cuter_erlang:'>='/2", prop_gteq(), 2000}
  , {"erlang:'++'/2 => cuter_erlang:'++'/2", prop_lappend(), 1000}
  , {"erlang:'--'/2 => cuter_erlang:'--'/2", prop_lsubtract(), 1000}
  , {"lists:reverse/2 => cuter_erlang:reverse/2", prop_lreverse(), 1000}
  , {"lists:member/2 => cuter_erlang:member/2", prop_lmember(), 1000}
  ],
  [{Descr, {timeout, 10000, ?_assert(proper:quickcheck(Prop, [{to_file, user}, {numtests, N}]))}} || {Descr, Prop, N} <- Props].

-spec prop_atom_to_list() -> proper:outer_test().
prop_atom_to_list() ->
  ?FORALL(X, atom(), erlang:atom_to_list(X) =:= cuter_erlang:atom_to_list(X)).

-spec prop_lt() -> proper:outer_test().
prop_lt() ->
  ?FORALL({X,Y}, {any(),any()}, (X < Y) =:= cuter_erlang:'<'(X, Y)).

-spec prop_lteq() -> proper:outer_test().
prop_lteq() ->
  ?FORALL({X,Y}, {any(),any()}, (X =< Y) =:= cuter_erlang:'=<'(X, Y)).

-spec prop_gt() -> proper:outer_test().
prop_gt() ->
  ?FORALL({X,Y}, {any(),any()}, (X > Y) =:= cuter_erlang:'>'(X, Y)).

-spec prop_gteq() -> proper:outer_test().
prop_gteq() ->
  ?FORALL({X,Y}, {any(),any()}, (X >= Y) =:= cuter_erlang:'>='(X, Y)).

-spec prop_lappend() -> proper:outer_test().
prop_lappend() ->
  ?FORALL({X,Y}, {list(),list()}, (X ++ Y) =:= cuter_erlang:'++'(X, Y)).

-spec prop_lsubtract() -> proper:outer_test().
prop_lsubtract() ->
  ?FORALL({X,Y}, {list(),list()}, (X -- Y) =:= cuter_erlang:'--'(X, Y)).

-spec prop_lreverse() -> proper:outer_test().
prop_lreverse() ->
  ?FORALL({X,Y}, {list(),list()}, lists:reverse(X, Y) =:= cuter_erlang:reverse(X, Y)).

-spec prop_lmember() -> proper:outer_test().
prop_lmember() ->
  ?FORALL({X,Y}, {any(),list()}, lists:member(X, Y) =:= cuter_erlang:member(X, Y)).

-spec reversible_bifs_test_() -> any().
reversible_bifs_test_() ->
  Props = [
    {"erlang:'+'/2 => cuter_erlang:'+'/2", prop_plus(), 1000}
  , {"erlang:'-'/2 => cuter_erlang:'-'/2", prop_minus(), 1000}
  ],
  [{Descr, {timeout, 10000, ?_assert(proper:quickcheck(Prop, [{to_file, user}, {numtests, N}]))}} || {Descr, Prop, N} <- Props].

-spec prop_plus() -> proper:outer_test().
prop_plus() ->
  ?FORALL({X,Y}, {number(),number()}, (X + Y) =:= cuter_erlang:'+'(X, Y)).

-spec prop_minus() -> proper:outer_test().
prop_minus() ->
  ?FORALL({X,Y}, {number(),number()}, (X - Y) =:= cuter_erlang:'-'(X, Y)).
