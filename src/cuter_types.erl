%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_types).

-export([parse_spec/1]).

-export_type([erl_type/0, erl_spec_clause/0, erl_spec/0]).

-include("include/cuter_macros.hrl").

%% Unified representation of Erlang types and specs
-type erl_type() :: any
                  | atom
                  | {atom, atom()}
                  | boolean
                  | float
                  | integer
                  | {integer, integer()}
                  | {list, erl_type()}
                  | nil
                  | tuple
                  | {tuple, [erl_type()]}
                  | {union, [erl_type()]}.

-type erl_spec_clause() :: {[erl_type()], erl_type()}.
-type erl_spec() :: [erl_spec_clause(), ...].

%% Parse the spec of an MFA.

-spec parse_spec(cuter_cerl:cerl_spec()) -> erl_spec().
parse_spec(Spec) ->
  [parse_clause(S, orddict:new()) || S <- Spec].

-spec parse_clause(cuter_cerl:cerl_func() | cuter_cerl:cerl_bounded_func(), orddict:orddict()) -> erl_spec_clause().
parse_clause({type, _, 'fun', [Product, Ret]}, Bound) ->
  {type, _, product, Params} = Product,
  {[parse_type(P, Bound) || P <- Params], parse_type(Ret, Bound)};
parse_clause({type, _, bounded_fun, [Fun, Constraints]}, Bound) ->
  Bound1 = bind_constraints(Constraints, Bound),
  parse_clause(Fun, Bound1).

%% Store the constraints of a bounded fun.
bind_constraints(Constraints, Bound) ->
  lists:foldl(fun(Cst, Bnd) -> parse_constraint(Cst, Bnd) end, Bound, Constraints).

%% Store the type of a constraint (in a bounded fun).
parse_constraint({type, _, constraint, [{atom, _, is_subtype}, [Var, Type]]}, Bound) ->
  {var, _, V} = Var,
  orddict:store({var, V}, fun(Bnd) -> parse_type(Type, Bnd) end, Bound).


%% Parse an Erlang type.
-spec parse_type(cuter_cerl:cerl_type(), orddict:orddict()) -> erl_type().
parse_type({atom, _, Atom}, _Bound) ->
  {atom, Atom};
parse_type({integer, _, Integer}, _Bound) ->
  {integer, Integer};
parse_type({type, _, nil, []}, _Bound) ->
  nil;
parse_type({type, _, any, []}, _Bound) ->
  any;
parse_type({type, _, term, []}, _Bound) ->
  any;
parse_type({type, _, atom, []}, _Bound) ->
  atom;
parse_type({type, _, boolean, []}, _Bound) ->
  boolean;
parse_type({type, _, integer, []}, _Bound) ->
  integer;
parse_type({type, _, float, []}, _Bound) ->
  float;
parse_type({type, _, list, []}, _Bound) ->
  {list, any};
parse_type({type, _, list, [Type]}, Bound) ->
  {list, parse_type(Type, Bound)};
parse_type({type, _, tuple, any}, _Bound) ->
  tuple;
parse_type({type, _, tuple, Types}, Bound) ->
  {tuple, [parse_type(T, Bound) || T <- Types]};
parse_type({type, _, union, Types}, Bound) ->
  {union, [parse_type(T, Bound) || T <- Types]};
parse_type({var, _, Var}, Bound) ->
  case orddict:find({var, Var}, Bound) of
    {ok, V} -> V(Bound);
    error -> any
  end;
%% Unsupported type.
parse_type(Type, _Bound) -> throw({unsupported_type, Type}).








