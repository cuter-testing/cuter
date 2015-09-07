%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_symbolic).

-export([
  fresh_symbolic_var/0, abstract/1, evaluate_mfa/6, generate_new_input/2,
  is_symbolic/1, serialize/1, deserialize/1, is_supported_mfa/1,
  ensure_list/3, tpl_to_list/3, head/2, tail/2, cons/4, make_tuple/3,
  make_bitstring/4, match_bitstring_const_true/6, match_bitstring_var_true/6,
  non_empty_binary/2, concat_segments/3, match_bitstring_const_false/5,
  match_bitstring_var_false/4
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

-spec evaluate_mfa(mfa(), [maybe_s(any())], any(), pid(), cuter_cerl:tag(), file:io_device()) -> maybe_s(any()).
evaluate_mfa(MFA, SAs, Cv, CodeServer, Tag, Fd) ->
  case is_supported_mfa(MFA) of
    false ->
      cuter_codeserver:unsupported_mfa(CodeServer, MFA),
      Cv;
    true  ->
      case lists:any(fun cuter_symbolic:is_symbolic/1, SAs) of
        false -> Cv;
        true  -> evaluate_supported_mfa(MFA, SAs, Tag, Fd)
      end
  end.

-spec evaluate_supported_mfa(mfa(), [maybe_s(any())], cuter_cerl:tag(), file:io_device()) -> symbolic().
evaluate_supported_mfa(MFA, SAs, Tag, Fd) ->
  X = fresh_symbolic_var(),
  cuter_log:log_mfa(Fd, MFA, SAs, X, Tag),
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
%% (Used before evaluating a cerl function)
-spec ensure_list(maybe_s([any()]), non_neg_integer(), file:io_device()) -> [maybe_s(any())].
ensure_list(SVs, N, Fd) ->
  case is_symbolic(SVs) of
    false -> SVs;
    true  -> break_term(break_list, SVs, N, Fd)
  end.

%% Return the head of a symbolic term that represents a list
%% (Used when matching lists)
-spec head(maybe_s([any()]), file:io_device()) -> maybe_s(any()).
head(Sv, _Fd) when is_list(Sv) -> erlang:hd(Sv);
head(Sv, Fd) ->
  evaluate_supported_mfa({erlang, hd, 1}, [Sv], cuter_cerl:empty_tag(), Fd).

%% Return the tail of a symbolic term that represents a list
%% (Used when matching lists)
-spec tail(maybe_s([any()]), file:io_device()) -> maybe_s([any()]).
tail(Sv, _Fd) when is_list(Sv) -> erlang:tl(Sv);
tail(Sv, Fd) ->
  evaluate_supported_mfa({erlang, tl, 1}, [Sv], cuter_cerl:empty_tag(), Fd).

%% Returns the cons of two terms. Either can be a symbolic value.
%% (Used when creating lists).
-spec cons(maybe_s(any()), maybe_s([any()]), list(), file:io_device()) -> maybe_s([list()]).
cons(V1, V2, Cv, Fd) ->
  case is_symbolic(V1) orelse is_symbolic(V2) of
    false -> Cv;
    true ->  evaluate_supported_mfa({bogus_erlang, cons, 2}, [V1, V2], cuter_cerl:empty_tag(), Fd)
  end.

%% Create a list of N elements from a symbolic variable
%% that represents a list or a tuple with size N
-spec break_term(break_tuple | break_list, symbolic(), pos_integer(), file:io_device()) -> [symbolic()].
break_term(M, Sv, N, Fd) when M =:= break_tuple; M =:= break_list ->
  Vs = [fresh_symbolic_var() || _ <- lists:seq(1, N)],
  cuter_log:log_unfold_symbolic(Fd, M, Sv, Vs),
  Vs.

%% Creates a tuple.
%% (Used when creating tuples).
-spec make_tuple([maybe_s(any())], tuple(), file:io_device()) -> maybe_s(tuple()).
make_tuple(Xs, Cv, Fd) ->
  case lists:any(fun is_symbolic/1, Xs) of
    false -> Cv;
    true ->
      Sv = fresh_symbolic_var(),
      cuter_log:log_make_tuple(Fd, Sv, Xs),
      Sv
  end.

%% =============================================================
%% Symbolic representation of binaries and binary operations
%% TODO
%% =============================================================

%% Encode a symbolic term into a bitstring.
%% TODO For now, ignore the case where the size is a symbolic variable.
%% Also, ignoring Unit, Type and Flags.
-spec make_bitstring(maybe_s(bitstring()), bencoding(), bitstring(), file:io_device()) -> maybe_s(bitstring()).
make_bitstring(Sv, {Size, _Unit, _Type, _Flags}, Cv, Fd) ->
  case is_symbolic(Size) of
    true -> Cv;
    false ->
      case Size of
        all ->
          Sv;
        _ ->
          FreshSv = fresh_symbolic_var(),
          cuter_log:log_make_bitstring(Fd, FreshSv, Sv, Size),
          FreshSv
      end
  end.

-spec non_empty_binary(symbolic(), file:io_device()) -> {symbolic(), symbolic()}.
non_empty_binary(Sv, Fd) ->
  H = fresh_symbolic_var(),
  T = fresh_symbolic_var(),
  cuter_log:log_nonempty_bitstring(Fd, H, T, Sv),
  {H, T}.

-spec concat_segments([maybe_s(integer())], maybe_s(bitstring()), file:io_device()) -> symbolic().
concat_segments(Bits, Sv, Fd) ->
  Sv1 = fresh_symbolic_var(),
  cuter_log:log_concat_segments(Fd, Sv1, Bits, Sv),
  Sv1.

%% Symbolic representation of pattern matching a symbolic bitstring
%% to an encoded term and return the rest of the symbolic bitstring
%% TODO For now, ignore the case where the size is a symbolic variable.
%% Also, ignoring Unit, Type and Flags.

%% Match succeeded.
-spec match_bitstring_const_true(any(), bencoding(), maybe_s(bitstring()), bitstring(), cuter_cerl:tag(), file:io_device()) -> maybe_s(bitstring()).
match_bitstring_const_true(Cnst, {Size, _Unit, _Type, _Flags}, Sv, Rest_c, Tag, Fd) ->
  case is_symbolic(Size) of
    true -> Rest_c;
    false ->
      case not is_symbolic(Cnst) andalso not is_symbolic(Sv) of
        true -> Rest_c;
        false ->
          Sv1 = fresh_symbolic_var(),
          cuter_log:log_bitmatch_const_true(Fd, Cnst, Size, Sv, Sv1, Tag),
          Sv1
      end
  end.

%% Match failed.
-spec match_bitstring_const_false(any(), bencoding(), maybe_s(bitstring()), cuter_cerl:tag(), file:io_device()) -> ok.
match_bitstring_const_false(Cnst, {Size, _Unit, _Type, _Flags}, Sv, Tag, Fd) ->
  case is_symbolic(Size) of
    true -> ok;
    false ->
      case not is_symbolic(Cnst) andalso not is_symbolic(Sv) of
        true -> ok;
        false ->
          cuter_log:log_bitmatch_const_false(Fd, Cnst, Size, Sv, Tag),
          ok
      end
  end.

%% Symbolic representation of pattern matching a symbolic bitstring
%% to an encoded term and return the matched value and the
%% rest of the symbolic bitstring.
%% TODO For now, ignore the case where the size is a symbolic variable.
%% Also, ignoring Unit, Type and Flags.

%% Match succeeded.
-spec match_bitstring_var_true(bencoding(), maybe_s(bitstring()), bitstring(), bitstring(), cuter_cerl:tag(), file:io_device()) -> 
  {maybe_s(bitstring()), maybe_s(bitstring())}.
match_bitstring_var_true({Size, _Unit, _Type, _Flags}, Sv, X_c, Rest_c, Tag, Fd) ->
  case is_symbolic(Size) of
    true -> {X_c, Rest_c};
    false ->
      case Size of
        all ->
          {Sv, <<>>};
        _ ->
          case is_symbolic(Sv) of
            false -> {X_c, Rest_c};
            true ->
              Sv1 = fresh_symbolic_var(),
              Sv2 = fresh_symbolic_var(),
              cuter_log:log_bitmatch_var_true(Fd, Sv1, Sv2, Size, Sv, Tag),
              {Sv1, Sv2}
          end
      end
  end.

-spec match_bitstring_var_false(bencoding(), maybe_s(bitstring()), cuter_cerl:tag(), file:io_device()) -> ok.
match_bitstring_var_false({Size, _Unit, _Type, _Flags}, Sv, Tag, Fd) ->
  case is_symbolic(Size) of
    true -> ok;
    false ->
      case is_symbolic(Sv) of
        false -> ok;
        true ->
          cuter_log:log_bitmatch_var_false(Fd, Size, Sv, Tag),
          ok
      end
  end.
