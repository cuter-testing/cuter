%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_serial).

-export([from_term/1, to_term/1]).
-export([solver_command/1, solver_command/2]).

-include("include/cuter_macros.hrl").
-include("include/cuter_types.hrl").
-include("include/erlang_term.hrl").
-include("include/solver_command.hrl").

-type erlang_term() :: #'ErlangTerm'{}.
-type supported_term() :: atom() | bitstring() | pid() | reference() | [any()]
                        | number() | tuple() | map().

-type commands_no_opts() :: solve | get_model | add_axioms | reset_solver | stop.

%% ============================================================================
%% Public API.
%% ============================================================================

%% Serialize an Erlang term to binary data.
-spec from_term(supported_term()) -> binary().
from_term(Term) ->
  Msg = to_erlang_term(Term),
  erlang_term:encode_msg(Msg).

%% De-serialize an Erlang term from binary data.
-spec to_term(binary()) -> supported_term().
to_term(Msg) ->
  ErlangTerm = erlang_term:decode_msg(Msg, 'ErlangTerm'),
  from_erlang_term(ErlangTerm).

-spec solver_command(commands_no_opts()) -> binary().
solver_command(Command) -> solver_command(Command, none).

-spec solver_command(load_trace_file, {file:name(), integer()}) -> binary()
                  ; (fix_variable, {cuter_symbolic:symbolic(), any()}) -> binary()
                  ; (commands_no_opts(), none) -> binary().
solver_command(Command, Options) ->
  Msg = to_solver_command(Command, Options),
  io:format("~p~n", [Msg]),
  solver_command:encode_msg(Msg).

%% ============================================================================
%% Encode Terms to ErlangTerm messages.
%% ============================================================================

-spec to_erlang_term(supported_term()) -> erlang_term().
to_erlang_term(Term) ->
  case cuter_symbolic:is_symbolic(Term) of
    true  -> encode_symbolic_term(Term);
    false -> encode_concrete_term(Term)
  end.

encode_symbolic_term(Term) ->
  #'ErlangTerm'{type='SYMBOLIC_VARIABLE',
                value=cuter_symbolic:serialize(Term)}.

encode_concrete_term(Term) ->
  %% Find the shared subterms.
  Seen = ets:new(?MODULE, [set, protected]),
  Shared = ets:new(?MODULE, [set, protected]),
  scan_term(Term, Seen, Shared),
  %% Encode the structure of the term to an ErlangTerm.
  ErlangTerm = encode_term(Term, Seen),
  %% Encode the common subterms.
  SharedEncoded = encode_shared(Shared, Seen),
  lists:foreach(fun ets:delete/1, [Seen, Shared]),
  ErlangTerm#'ErlangTerm'{shared=SharedEncoded}.

%% 1st Pass of a Term to locate the common subterms.
scan_term([], _Seen, _Shared) -> ok;  %% Never remember the empty list
scan_term([H|T]=Term, Seen, Shared) ->
  case remember_term(Term, Seen, Shared) of
    true  -> ok;
    false ->
      scan_term(H, Seen, Shared),
      scan_term(T, Seen, Shared)
  end;
scan_term(Term, Seen, Shared) when is_tuple(Term) ->
  case remember_term(Term, Seen, Shared) of
    true  -> ok;
    false ->
      Ts = tuple_to_list(Term),
      lists:foreach(fun(T) -> scan_term(T, Seen, Shared) end, Ts)
  end;
scan_term(Term, Seen, Shared) when is_map(Term) ->
  case remember_term(Term, Seen, Shared) of
    true  -> ok;
    false ->
      Keys = maps:keys(Term),
      Values = [maps:get(Key, Term) || Key <- Keys],
      Ts = Keys ++ Values,
      lists:foreach(fun(T) -> scan_term(T, Seen, Shared) end, Ts)
  end;
scan_term(Term, Seen, Shared) ->
  case remember_term(Term, Seen, Shared) of
    true  -> ok;
    false -> ok
  end.

%% Update Seen and Shared dictionaries.
remember_term(Term, Seen, Shared) ->
  case ets:lookup(Seen, Term) of
    %% 1st time of encountering a term.
    [] ->
      ets:insert(Seen, {Term, init}),
      false;
    %% 2nd time of encountering a term.
    [{Term, init}] ->
      R = erlang:ref_to_list(erlang:make_ref()) -- "#Ref<>",
      ets:insert(Seen, {Term, R}),
      ets:insert(Shared, {R, Term}),
      true;
    %% Nth time of encountering a term.
    [{Term, _R}] -> true
  end.

%% 2nd Pass of a Term to encode the term structure & the shared subterms

%% integer
encode_term(I, _Seen) when is_integer(I) ->
  #'ErlangTerm'{type='INTEGER', value=integer_to_list(I)};
%% float
encode_term(F, _Seen) when is_float(F) ->
  #'ErlangTerm'{type='FLOAT',
                value=float_to_list(F, [{decimals, 10}, compact])};
%% atom
encode_term(A, _Seen) when is_atom(A) ->
  #'ErlangTerm'{type='ATOM', atom_chars=atom_to_list(A)};
%% list
encode_term(L, Seen) when is_list(L) ->
  case cuter_lib:is_improper_list(L) of
    false ->
      Subterms = [encode_maybe_shared_term(T, Seen) || T <- L],
      #'ErlangTerm'{type='LIST', subterms=Subterms};
    true ->
      {Xs, Trm} = cuter_lib:get_parts_of_list(L),
      Subterms = [encode_maybe_shared_term(T, Seen) || T <- Xs],
      NilTerm = encode_maybe_shared_term(Trm, Seen),
      #'ErlangTerm'{type='IMPROPER_LIST',
                    subterms=Subterms,
                    improper_list_nil=NilTerm}
    end;
%% tuple
encode_term(T, Seen) when is_tuple(T) ->
  Ts = tuple_to_list(T),
  Subterms = [encode_maybe_shared_term(X, Seen) || X <- Ts],
  #'ErlangTerm'{type='TUPLE', subterms=Subterms};
%% pid
encode_term(Pid, _Seen) when is_pid(Pid) ->
  #'ErlangTerm'{type='PID', value=pid_to_list(Pid)};
%% reference
encode_term(Ref, _Seen) when is_reference(Ref) ->
  #'ErlangTerm'{type='REFERENCE', value=erlang:ref_to_list(Ref)};
%% bitstring & binary
encode_term(Ref, _Seen) when is_bitstring(Ref) ->
  Bits = encode_bitstring(Ref),
  #'ErlangTerm'{type='BITSTRING', bits=Bits};
%% map
encode_term(M, Seen) when is_map(M) ->
  Fn = fun(K) ->
      V = maps:get(K, M),
      Key = encode_maybe_shared_term(K, Seen),
      Value = encode_maybe_shared_term(V, Seen),
      #'ErlangTerm.MapEntry'{key=Key, value=Value}
    end,
  MapEntries = lists:map(Fn, maps:keys(M)),
  #'ErlangTerm'{type='MAP', map_entries=MapEntries};
encode_term(Term, _Seen) ->
  throw({unsupported_term, Term}).

encode_maybe_shared_term(T, Seen) when is_integer(T); is_float(T); is_atom(T); is_bitstring(T);
                                       is_list(T); is_tuple(T); is_pid(T); is_reference(T); is_map(T) ->
  case is_shared(T, Seen) of
    false -> encode_term(T, Seen);
    {true, R} -> encode_term_alias(R)
  end;
encode_maybe_shared_term(Term, _Seen) ->
  throw({unsupported_term, Term}).

encode_term_alias(R) ->
  #'ErlangTerm'{type='SUBTERM', value=R}.

is_shared([], _Seen) -> false;  %% Never remember the empty list
is_shared(Term, Seen) ->
  case ets:lookup(Seen, Term) of
    [{Term, init}] -> false;
    [{Term, R}] -> {true, R};
    [] -> throw({assert_term_seen, Term})
  end.

%% Encode a bitstring to a list of boolean values.
%% i.e. <<1:2>> will become [false,true].
encode_bitstring(Bin) ->
  encode_bitstring(Bin, []).

encode_bitstring(<<>>, []) ->
  [];
encode_bitstring(<<>>, Bits) ->
  lists:reverse(Bits);
encode_bitstring(<<B:1, Rest/bitstring>>, Bits) ->
  encode_bitstring(Rest, [B | Bits]).

encode_shared(Shared, Seen) ->
  F = fun({K, V}, Acc) -> [{K, encode_term(V, Seen)} | Acc] end,
  ets:foldl(F, [], Shared).

%% ============================================================================
%% Decode ErlangTerm messages to Terms.
%% ============================================================================

from_erlang_term(ErlangTerm) ->
  Shared = dict:from_list(ErlangTerm#'ErlangTerm'.shared),
  from_erlang_term(ErlangTerm, Shared).

%% symbolic variable
from_erlang_term(T=#'ErlangTerm'{type=Type}, _Shared) when Type =:= 'SYMBOLIC_VARIABLE' ->
  cuter_symbolic:deserialize(T#'ErlangTerm'.value);
%% alias
from_erlang_term(T=#'ErlangTerm'{type=Type}, Shared) when Type =:= 'SUBTERM' ->
  Alias = dict:fetch(T#'ErlangTerm'.value, Shared),
  from_erlang_term(Alias, Shared);
%% integer
from_erlang_term(T=#'ErlangTerm'{type=Type}, _Shared) when Type =:= 'INTEGER' ->
  list_to_integer(T#'ErlangTerm'.value);
%% float
from_erlang_term(T=#'ErlangTerm'{type=Type}, _Shared) when Type =:= 'FLOAT' ->
  list_to_float(T#'ErlangTerm'.value);
%% atom
from_erlang_term(T=#'ErlangTerm'{type=Type}, _Shared) when Type =:= 'ATOM' ->
  list_to_atom(T#'ErlangTerm'.atom_chars);
%% list
from_erlang_term(T=#'ErlangTerm'{type=Type}, Shared) when Type =:= 'LIST' ->
  [from_erlang_term(X, Shared) || X <- T#'ErlangTerm'.subterms];
%% improper list
from_erlang_term(T=#'ErlangTerm'{type=Type}, Shared) when Type =:= 'IMPROPER_LIST' ->
  Nil = from_erlang_term(T#'ErlangTerm'.improper_list_nil, Shared),
  Ts = [from_erlang_term(X, Shared) || X <- T#'ErlangTerm'.subterms],
  cuter_lib:create_improper_list(Ts, Nil);
%% tuple
from_erlang_term(T=#'ErlangTerm'{type=Type}, Shared) when Type =:= 'TUPLE' ->
  list_to_tuple([from_erlang_term(X, Shared) || X <- T#'ErlangTerm'.subterms]);
%% pid
from_erlang_term(T=#'ErlangTerm'{type=Type}, _Shared) when Type =:= 'PID' ->
  list_to_pid(T#'ErlangTerm'.value);
%% reference
from_erlang_term(T=#'ErlangTerm'{type=Type}, _Shared) when Type =:= 'REFERENCE' ->
  %% FIXME Currently returns the list representation of the reference.
  T#'ErlangTerm'.value;
%% bitstring
from_erlang_term(T=#'ErlangTerm'{type=Type}, _Shared) when Type =:= 'BITSTRING' ->
  Bits = [if X =:= true -> 1; X =:= false -> 0 end || X <- T#'ErlangTerm'.bits],
  F = fun(A, AccIn) -> <<A:1, AccIn/bitstring>> end,
  lists:foldl(F, <<>>, lists:reverse(Bits));
%% map
from_erlang_term(T=#'ErlangTerm'{type=Type}, Shared) when Type =:= 'MAP' ->
  MapEntries = T#'ErlangTerm'.map_entries,
  KVs = [from_map_entry(E, Shared) || E <- MapEntries],
  maps:from_list(KVs).

from_map_entry(#'ErlangTerm.MapEntry'{key=K, value=V}, Shared) ->
  {from_erlang_term(K, Shared), from_erlang_term(V, Shared)}.

%% ============================================================================
%% Encode commands to the solver as SolverCommand messages.
%% ============================================================================

to_solver_command(load_trace_file, {File, To}) ->
  #'SolverCommand'{type='LOAD_TRACE_FILE', filename=File, to_constraint=To};
to_solver_command(solve, none) ->
  #'SolverCommand'{type='SOLVE'};
to_solver_command(get_model, none) ->
  #'SolverCommand'{type='GET_MODEL'};
to_solver_command(add_axioms, none) ->
  #'SolverCommand'{type='ADD_AXIOMS'};
to_solver_command(fix_variable, {SymbVar, Value}) ->
  #'SolverCommand'{type='FIX_VARIABLE',
                   symbvar=to_erlang_term(SymbVar),
                   symbvar_value=to_erlang_term(Value)};
to_solver_command(reset_solver, none) ->
  #'SolverCommand'{type='RESET_SOLVER'};
to_solver_command(stop, none) ->
  #'SolverCommand'{type='stop'}.
