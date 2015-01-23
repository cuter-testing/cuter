%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_symbolic).

-export([
  fresh_symbolic_var/0, abstract/1, evaluate_mfa/5, generate_new_input/2,
  is_symbolic/1, serialize/1, deserialize/1,
  ensure_list/3, tpl_to_list/3, head/2, tail/2,
  append_segments/3, make_bitstring/4, match_bitstring_const/5, match_bitstring_var/5
]).

-include("include/cuter_macros.hrl").

-export_type([mapping/0, symbolic/0]).

-type symbolic() :: {?SYMBOLIC_PREFIX, nonempty_string()}.  %% Symbolic Variable
-type mapping()  :: {symbolic(), any()}.

-type maybe_s(X) :: symbolic() | X.
-type bencoding() :: {maybe_s(cuter_binlib:bsize()), cuter_binlib:bunit(), cuter_binlib:btype(), [cuter_binlib:bflag()]}.

%% =============================================================
%% Basic operations on symbolic variables and values
%% =============================================================

%% Create a fresh symbolic variable
-spec fresh_symbolic_var() -> symbolic().
fresh_symbolic_var() -> {?SYMBOLIC_PREFIX, cuter_lib:unique_string()}.

%% Check whether a term represents a symbolic variable
-spec is_symbolic(any()) -> boolean().
is_symbolic({?SYMBOLIC_PREFIX, SymbVar}) when is_list(SymbVar) -> true;
is_symbolic(_) -> false.

%% Abstract a list of concrete values
-spec abstract([any()]) -> {[symbolic()], [mapping()]}.
abstract(Vs) ->
  Symbs = [fresh_symbolic_var() || _ <- lists:seq(1, erlang:length(Vs))],
  Maps = lists:zip(Symbs, Vs),
  {Symbs, Maps}.

%% Extract new concrete input from the symbolic mapping and the solver's result
-spec generate_new_input([mapping()], [{symbolic(), cuter_solver:model()}]) -> [any()].
generate_new_input(Maps, Model) ->
  F = fun({X, V}) ->
    case cuter_solver:lookup_in_model(X, Model) of  %% Do not expect an exception to be raised
      ?UNBOUND_VAR_PREFIX -> V;
      NV -> NV
    end
  end,
  [F(M) || M <- Maps].

%% Serialize the representation of a symbolic value
-spec serialize(symbolic()) -> list().
serialize({?SYMBOLIC_PREFIX, SymbVar}) when is_list(SymbVar) -> SymbVar.

%% Create a symbolic value from a List representation
-spec deserialize(list()) -> symbolic().
deserialize(L) when is_list(L) -> {?SYMBOLIC_PREFIX, L}.

%% =============================================================
%% Symbolic evaluation of MFAs
%% =============================================================

%% The MFAs the are supported for symbolic evaluation
-spec is_supported_mfa(mfa()) -> boolean().
is_supported_mfa(MFA) ->
  gb_sets:is_member(MFA, ?SUPPORTED_MFAS).

-spec evaluate_mfa(mfa(), [maybe_s(any())], any(), pid(), file:io_device()) -> maybe_s(any()).
evaluate_mfa(MFA, SAs, Cv, CodeServer, Fd) ->
  case is_supported_mfa(MFA) of
    false ->
      cuter_codeserver:unsupported_mfa(CodeServer, MFA),
      Cv;
    true  ->
      case lists:any(fun cuter_symbolic:is_symbolic/1, SAs) of
        false -> Cv;
        true  -> evaluate_supported_mfa(MFA, SAs, Fd)
      end
  end.

-spec evaluate_supported_mfa(mfa(), [maybe_s(any())], file:io_device()) -> maybe_s(any()).
evaluate_supported_mfa(MFA, SAs, Fd) ->
  X = fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, MFA, SAs, X),
  X.

%% =============================================================
%% Symbolic representations of term operations
%% =============================================================

%% Create a list of N elements from a symbolic term that represents a tuple with size N
%% (Used when matching tuples)
-spec tpl_to_list(maybe_s(tuple()), non_neg_integer(), file:io_device()) -> [maybe_s(any())].
tpl_to_list(Sv, Ne, Fd) ->
  case is_symbolic(Sv) of
    true  -> break_term(break_tuple, Sv, Ne, Fd);
    false -> erlang:tuple_to_list(Sv)
  end.

%% Ensures that a symbolic term is a list of N elements
%% (Used before evaluating a Cerl function)
-spec ensure_list(maybe_s([any()]), pos_integer(), file:io_device()) -> [maybe_s(any())].
ensure_list(SVs, N, Fd) ->
  case is_symbolic(SVs) of
    false -> SVs;
    true  -> break_term(break_list, SVs, N, Fd)
  end.

%% Return the head of a symbolic term that represents a list
%% (Used when matching lists)
-spec head(maybe_s([any()]), file:io_device()) -> maybe_s(any()).
head(Sv, _Fd) when is_list(Sv) -> erlang:hd(Sv);
head(Sv, Fd) -> evaluate_supported_mfa({erlang, hd, 1}, [Sv], Fd).

%% Return the tail of a symbolic term that represents a list
%% (Used when matching lists)
-spec tail(maybe_s([any()]), file:io_device()) -> maybe_s([any()]).
tail(Sv, _Fd) when is_list(Sv) -> erlang:tl(Sv);
tail(Sv, Fd) -> evaluate_supported_mfa({erlang, tl, 1}, [Sv], Fd).

%% Create a list of N elements from a symbolic variable
%% that represents a list or a tuple with size N
-spec break_term(break_tuple | break_list, symbolic(), pos_integer(), file:io_device()) -> [symbolic()].
break_term(M, Sv, N, Fd) when M =:= break_tuple; M =:= break_list ->
  Vs = [fresh_symbolic_var() || _ <- lists:seq(1, N)],
  cuter_log:log_unfold_symbolic(Fd, M, Sv, Vs),
  Vs.

%% =============================================================
%% Symbolic representation of binaries and binary operations
%% TODO
%% =============================================================

%% Symbolically represent bitstring concatenation
-spec append_segments(bitstring(), [maybe_s(bitstring())], file:io_device()) -> maybe_s(bitstring()).
append_segments(Cv, _Segs, _Fd) ->
  Cv.

%% Encode a symbolic term into a bitstring
-spec make_bitstring(maybe_s(bitstring()), bencoding(), bitstring(), file:io_device()) -> maybe_s(bitstring()).
make_bitstring(_Sv, {_Size, _Unit, _Type, _Flags}, Cv, _Fd) ->
  Cv.

%% Symbolic representation of pattern matching a symbolic bitstring
%% to an encoded term and return the rest of the symbolic bitstring
-spec match_bitstring_const(any(), bencoding(), maybe_s(bitstring()), bitstring(), file:io_device()) ->
  {maybe_s(any()), maybe_s(bitstring())}.
match_bitstring_const(Cnst, {_Size, _Unit, _Type, _Flags}, _Sv, Rest_c, _Fd) ->
  {Cnst, Rest_c}.

%% Symbolic representation of pattern matching a symbolic bitstring
%% to an encoded term and return the matched value and the
%% rest of the symbolic bitstring
-spec match_bitstring_var(bencoding(), maybe_s(bitstring()), bitstring(), bitstring(), file:io_device()) -> 
  {maybe_s(bitstring()), maybe_s(bitstring())}.
match_bitstring_var(_Enc, _Sv, X_c, Rest_c, _Fd) ->
  {X_c, Rest_c}.
