%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_erlang_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_config.hrl").

-spec test() -> ok | {error | term()}. %% Silence dialyzer warning

%% Test cuter_erlang:atom_to_list/1.
-spec atom_to_list_test() -> boolean().
atom_to_list_test() ->
  proper:quickcheck(prop_atom_to_list(), [{to_file, user}, {numtests, 1000}]).

-spec prop_atom_to_list() -> proper:outer_test().
prop_atom_to_list() ->
  ?FORALL(X, atom(), erlang:atom_to_list(X) =:= cuter_erlang:atom_to_list(X)).

%% Test cuter_erlang:'<'/2.
-spec lt_test() -> boolean().
lt_test() ->
  proper:quickcheck(prop_lt(), [{to_file, user}, {numtests, 1000}]).

-spec prop_lt() -> proper:outer_test().
prop_lt() ->
  ?FORALL({X,Y}, {any(),any()}, (X < Y) =:= cuter_erlang:'<'(X, Y)).

%% Test cuter_erlang:'=<'/2.
-spec lteq_test() -> boolean().
lteq_test() ->
  proper:quickcheck(prop_lteq(), [{to_file, user}, {numtests, 1000}]).

-spec prop_lteq() -> proper:outer_test().
prop_lteq() ->
  ?FORALL({X,Y}, {any(),any()}, (X =< Y) =:= cuter_erlang:'=<'(X, Y)).

%% Test cuter_erlang:'>'/2.
-spec gt_test() -> boolean().
gt_test() ->
  proper:quickcheck(prop_gt(), [{to_file, user}, {numtests, 1000}]).

-spec prop_gt() -> proper:outer_test().
prop_gt() ->
  ?FORALL({X,Y}, {any(),any()}, (X > Y) =:= cuter_erlang:'>'(X, Y)).

%% Test cuter_erlang:'>='/2.
-spec gteq_test() -> boolean().
gteq_test() ->
  proper:quickcheck(prop_gteq(), [{to_file, user}, {numtests, 1000}]).

-spec prop_gteq() -> proper:outer_test().
prop_gteq() ->
  ?FORALL({X,Y}, {any(),any()}, (X >= Y) =:= cuter_erlang:'>='(X, Y)).

%% Test cuter_erlang:'++'/2.
-spec append_test() -> boolean().
append_test() ->
  {timeout,10000,
  proper:quickcheck(prop_append(), [{to_file, user}, {numtests, 1000}])}.

-spec prop_append() -> proper:outer_test().
prop_append() ->
  ?FORALL({X,Y}, {list(),list()}, (X ++ Y) =:= cuter_erlang:'++'(X, Y)).

%% Test cuter_erlang:reverse/2.
-spec lreverse_test() -> boolean().
lreverse_test() ->
  {timeout,10000,
  proper:quickcheck(prop_lreverse(), [{to_file, user}, {numtests, 1000}])}.

-spec prop_lreverse() -> proper:outer_test().
prop_lreverse() ->
  ?FORALL({X,Y}, {list(),list()}, lists:reverse(X, Y) =:= cuter_erlang:reverse(X, Y)).
