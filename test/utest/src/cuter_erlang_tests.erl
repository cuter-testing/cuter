%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_erlang_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_config.hrl").

-define(TIMEOUT, 10000).

-type descr()    :: nonempty_string().
-type test_gen() :: [{descr(), {'timeout', ?TIMEOUT, _}}].

-spec test() -> 'ok' | {'error', term()}.  %% This should be provided by EUnit

-spec props_test_() -> test_gen().
props_test_() ->
  Props = [
    {"erlang:'<'/2 => cuter_erlang:'<'/2", prop_lt(), 2000}
  , {"erlang:'<'/2 (bitstring) => cuter_erlang:'<'/2", prop_lt_bitstring(), 2000}
  , {"erlang:'=<'/2 => cuter_erlang:'=<'/2", prop_lteq(), 2000}
  , {"erlang:'>'/2 => cuter_erlang:'>'/2", prop_gt(), 2000}
  , {"erlang:'>='/2 => cuter_erlang:'>='/2", prop_gteq(), 2000}
  , {"erlang:'++'/2 => cuter_erlang:'++'/2", prop_lappend(), 1000}
  , {"erlang:'--'/2 => cuter_erlang:'--'/2", prop_lsubtract(), 1000}
  , {"erlang:'=:='/2 => cuter_erlang:'=:='/2", prop_eq(), 4000}
  , {"erlang:'=/='/2 => cuter_erlang:'=/='/2", prop_neq(), 4000}
  , {"lists:reverse/2 => cuter_erlang:reverse/2", prop_lreverse(), 1000}
  , {"lists:member/2 => cuter_erlang:member/2", prop_lmember(), 1000}
  ],
  [form_test_gen(Descr, Prop, N) || {Descr, Prop, N} <- Props].

-spec prop_lt() -> proper:test().
prop_lt() ->
  ?FORALL({X,Y}, {any(),any()}, (X < Y) =:= cuter_erlang:'<'(X, Y)).

-spec prop_lt_bitstring() -> proper:test().
prop_lt_bitstring() ->
  ?FORALL({X,Y}, {bitstring(),bitstring()}, (X < Y) =:= cuter_erlang:'<'(X, Y)).

-spec prop_lteq() -> proper:test().
prop_lteq() ->
  ?FORALL({X,Y}, {any(),any()}, (X =< Y) =:= cuter_erlang:'=<'(X, Y)).

-spec prop_gt() -> proper:test().
prop_gt() ->
  ?FORALL({X,Y}, {any(),any()}, (X > Y) =:= cuter_erlang:'>'(X, Y)).

-spec prop_gteq() -> proper:test().
prop_gteq() ->
  ?FORALL({X,Y}, {any(),any()}, (X >= Y) =:= cuter_erlang:'>='(X, Y)).

-spec prop_lappend() -> proper:test().
prop_lappend() ->
  ?FORALL({X,Y}, {list(),list()}, (X ++ Y) =:= cuter_erlang:'++'(X, Y)).

-spec prop_lsubtract() -> proper:test().
prop_lsubtract() ->
  ?FORALL({X,Y}, {list(),list()}, (X -- Y) =:= cuter_erlang:'--'(X, Y)).

-spec prop_eq() -> proper:test().
prop_eq() ->
  ?FORALL({X,Y}, {any(),any()}, (X =:= Y) =:= cuter_erlang:'=:='(X, Y)).

-spec prop_neq() -> proper:test().
prop_neq() ->
  ?FORALL({X,Y}, {any(),any()}, (X =/= Y) =:= cuter_erlang:'=/='(X, Y)).

-spec prop_lreverse() -> proper:test().
prop_lreverse() ->
  ?FORALL({X,Y}, {list(),list()}, lists:reverse(X, Y) =:= cuter_erlang:reverse(X, Y)).

-spec prop_lmember() -> proper:test().
prop_lmember() ->
  ?FORALL({X,Y}, {any(),list()}, lists:member(X, Y) =:= cuter_erlang:member(X, Y)).


-spec reversible_bifs_test_() -> test_gen().
reversible_bifs_test_() ->
  Props = [
    {"erlang:'+'/2 => cuter_erlang:'+'/2", prop_plus(), 1000}
  , {"erlang:'-'/2 => cuter_erlang:'-'/2", prop_minus(), 1000}
  , {"erlang:'*'/2 => cuter_erlang:'*'/2", prop_times(), 1000}
  , {"erlang:'/'/2 => cuter_erlang:'/'/2", prop_rdiv(), 1000}
  , {"erlang:div/2 => cuter_erlang:div/2", prop_idiv(), 1000}
  , {"erlang:'rem'/2 => cuter_erlang:'rem'/2", prop_rem(), 1000}
  , {"erlang:float/1 => cuter_erlang:float/1", prop_float(), 1000}
  , {"erlang:list_to_tuple/1 => cuter_erlang:list_to_tuple/1", prop_list_to_tuple(), 1000}
  , {"erlang:tuple_to_list/1 => cuter_erlang:tuple_to_list/1", prop_tuple_to_list(), 1000}
  , {"erlang:atom_to_list/1 => cuter_erlang:atom_to_list/1", prop_atom_to_list(), 1000}
  , {"erlang:byte_size/1 => cuter_erlang:byte_size/1", prop_byte_size(), 4000}
  , {"erlang:bit_size/1 => cuter_erlang:bit_size/1", prop_bit_size(), 4000}
  , {"erlang:'bsl'/2 => cuter_erlang:'bsl'/2", prop_bsl(), 4000}
  , {"erlang:'bsr'/2 => cuter_erlang:'bsr'/2", prop_bsr(), 4000}
  , {"erlang:'bnot'/1 => cuter_erlang:'bnot'/1", prop_bnot(), 4000}
  ],
  [form_test_gen(Descr, Prop, N) || {Descr, Prop, N} <- Props].

-spec prop_plus() -> proper:test().
prop_plus() ->
  ?FORALL({X,Y}, {number(),number()}, (X + Y) =:= cuter_erlang:'+'(X, Y)).

-spec prop_minus() -> proper:test().
prop_minus() ->
  ?FORALL({X,Y}, {number(),number()}, (X - Y) =:= cuter_erlang:'-'(X, Y)).

-spec prop_times() -> proper:test().
prop_times() ->
  ?FORALL({X,Y}, {number(),number()}, (X * Y) =:= cuter_erlang:'*'(X, Y)).

-spec prop_rdiv() -> proper:test().
prop_rdiv() ->
  TY = ?SUCHTHAT(Y, number(), Y /= 0),
  ?FORALL({X,Y}, {number(),TY}, (X / Y) =:= cuter_erlang:'/'(X, Y)).

-spec prop_idiv() -> proper:test().
prop_idiv() ->
  TY = ?SUCHTHAT(Y, integer(), Y =/= 0),
  ?FORALL({X,Y}, {integer(),TY}, (X div Y) =:= cuter_erlang:'div'(X, Y)).

-spec prop_rem() -> proper:test().
prop_rem() ->
  TY = ?SUCHTHAT(Y, integer(), Y =/= 0),
  ?FORALL({X,Y}, {integer(),TY}, (X rem Y) =:= cuter_erlang:'rem'(X, Y)).

-spec prop_float() -> proper:test().
prop_float() ->
  ?FORALL(X, number(), float(X) =:= cuter_erlang:float(X)).

-spec prop_list_to_tuple() -> proper:test().
prop_list_to_tuple() ->
  ?FORALL(X, list(), list_to_tuple(X) =:= cuter_erlang:list_to_tuple(X)).

-spec prop_tuple_to_list() -> proper:test().
prop_tuple_to_list() ->
  ?FORALL(X, tuple(), tuple_to_list(X) =:= cuter_erlang:tuple_to_list(X)).

-spec prop_atom_to_list() -> proper:test().
prop_atom_to_list() ->
  ?FORALL(X, atom(), atom_to_list(X) =:= cuter_erlang:atom_to_list(X)).

-spec prop_byte_size() -> proper:test().
prop_byte_size() ->
  ?FORALL(X, bitstring(), erlang:byte_size(X) =:= cuter_erlang:byte_size(X)).

-spec prop_bit_size() -> proper:test().
prop_bit_size() ->
  ?FORALL(X, bitstring(), erlang:bit_size(X) =:= cuter_erlang:bit_size(X)).

-spec prop_bsl() -> proper:test().
prop_bsl() ->
  ?FORALL({X, Y}, {integer(), integer()}, (X bsl Y) =:= cuter_erlang:'bsl'(X, Y)).

-spec prop_bsr() -> proper:test().
prop_bsr() ->
  ?FORALL({X, Y}, {integer(), integer()}, (X bsr Y) =:= cuter_erlang:'bsr'(X, Y)).

-spec prop_bnot() -> proper:test().
prop_bnot() ->
  ?FORALL(X, integer(), erlang:'bnot'(X) =:= cuter_erlang:'bnot'(X)).


%%----------------------------------------------------------------------
%% Auxiliary functions below
%%----------------------------------------------------------------------

form_test_gen(Descr, Prop, N) ->
  OuterProp = proper:test_to_outer_test(Prop),
  Options   = [{to_file, user}, {numtests, N}],
  {Descr, {timeout, ?TIMEOUT, ?_assert(proper:quickcheck(OuterProp, Options))}}.
