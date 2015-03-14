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
  [parse_clause(S) || S <- Spec].

-spec parse_clause(cuter_cerl:cerl_func()) -> erl_spec_clause().
parse_clause({type, _, 'fun', [Product, Ret]}) ->
  {type, _, product, Params} = Product,
  {[parse_type(P) || P <- Params], parse_type(Ret)}.

%% Parse an Erlang type.
-spec parse_type(cuter_cerl:cerl_type()) -> erl_type().
parse_type({atom, _, Atom}) ->
  {atom, Atom};
parse_type({integer, _, Integer}) ->
  {integer, Integer};
parse_type({type, _, nil, []}) ->
  nil;
parse_type({type, _, any, []}) ->
  any;
parse_type({type, _, term, []}) ->
  any;
parse_type({type, _, atom, []}) ->
  atom;
parse_type({type, _, boolean, []}) ->
  boolean;
parse_type({type, _, integer, []}) ->
  integer;
parse_type({type, _, float, []}) ->
  float;
parse_type({type, _, list, []}) ->
  {list, any};
parse_type({type, _, list, [Type]}) ->
  {list, parse_type(Type)};
parse_type({type, _, tuple, any}) ->
  tuple;
parse_type({type, _, tuple, Types}) ->
  {tuple, [parse_type(T) || T <- Types]};
parse_type({type, _, union, Types}) ->
  {union, [parse_type(T) || T <- Types]}.








