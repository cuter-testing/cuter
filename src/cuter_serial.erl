%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_serial).

-export([from_term/1, to_term/1]).
-export([solver_command/1, solver_command/2, from_solver_response/1]).
-export([to_log_entry/4, from_log_entry/1]).

-include("include/cuter_macros.hrl").
-include("include/cuter_types.hrl").
-include("include/cuter_proto_erlang_term.hrl").
-include("include/cuter_proto_solver_command.hrl").
-include("include/cuter_proto_solver_response.hrl").
-include("include/cuter_proto_log_entry.hrl").
-include("include/cuter_proto_spec.hrl").

-type erlang_term() :: #'ErlangTerm'{}.
-type supported_term() :: atom() | bitstring() | pid() | reference() | list()
                        | number() | tuple() | map() | cuter_lib:lambda().

-type commands_no_opts() :: solve | get_model | add_axioms | reset_solver | stop.

%% ============================================================================
%% Public API.
%% ============================================================================

%% Serialize an Erlang term to binary data.
-spec from_term(supported_term()) -> binary().
from_term(Term) ->
  Msg = to_erlang_term(Term),
  cuter_proto_erlang_term:encode_msg(Msg).

%% De-serialize an Erlang term from binary data.
-spec to_term(binary()) -> supported_term().
to_term(Msg) ->
  ErlangTerm = cuter_proto_erlang_term:decode_msg(Msg, 'ErlangTerm'),
  from_erlang_term(ErlangTerm).

-spec solver_command(commands_no_opts()) -> binary().
solver_command(Command) -> solver_command(Command, none).

-spec solver_command(load_trace_file, {file:name(), integer()}) -> binary()
                  ; (fix_variable, {cuter_symbolic:symbolic(), any()}) -> binary()
                  ; (commands_no_opts(), none) -> binary().
solver_command(Command, Options) ->
  Msg = to_solver_command(Command, Options),
  cuter_proto_solver_command:encode_msg(Msg).

-spec to_log_entry(cuter_log:opcode(), [any()], boolean(), cuter_cerl:tagID()) -> binary().
to_log_entry(OpCode='OP_SPEC', [Spec], IsConstraint, _TagID) ->
  Msg = #'LogEntry'{ type = OpCode
                   , spec = to_spec(Spec)
                   , is_constraint = IsConstraint },
  cuter_proto_log_entry:encode_msg(Msg);
to_log_entry(OpCode, Arguments, IsConstraint, TagID) ->
  Parts = [to_erlang_term(A) || A <- Arguments],
  Msg = #'LogEntry'{ type = OpCode
                   , arguments = Parts
                   , is_constraint = IsConstraint
                   , tag = TagID },
  cuter_proto_log_entry:encode_msg(Msg).

-spec from_log_entry(binary()) -> {cuter_log:opcode(), [any()], boolean(), cuter_cerl:tagID()}.
from_log_entry(Msg) ->
  LogEntry = cuter_proto_log_entry:decode_msg(Msg, 'LogEntry'),
  Arguments = [from_erlang_term(A) || A <- LogEntry#'LogEntry'.arguments],
  {LogEntry#'LogEntry'.type, Arguments, LogEntry#'LogEntry'.is_constraint, LogEntry#'LogEntry'.tag}.

-spec from_solver_response(binary()) -> {'status', cuter_solver:solver_status()}
                                      | {'model', cuter_solver:model()}.
from_solver_response(Msg) ->
  SolverResponse = cuter_proto_solver_response:decode_msg(Msg, 'SolverResponse'),
  case SolverResponse#'SolverResponse'.type of
    'MODEL_STATUS' ->
      {'status', Status} = SolverResponse#'SolverResponse'.data,
      {'status', Status};
    'MODEL_DATA' ->
      {'model', Model} = SolverResponse#'SolverResponse'.data,
      {'model', from_model(Model)}
  end.

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
%%%
%%% TODO: Uncomment out these clauses when it is decided how to
%%% properly handle pids and refs.  Also, make sure that the
%%% corresponding unit tests are re-enabled.
%%%
%% %% pid
%% encode_term(Pid, _Seen) when is_pid(Pid) ->
%%   #'ErlangTerm'{type='PID', value=pid_to_list(Pid)};
%% %% reference
%% encode_term(Ref, _Seen) when is_reference(Ref) ->
%%   #'ErlangTerm'{type='REFERENCE', value=erlang:ref_to_list(Ref)};
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
  case cuter_lib:is_lambda(Term) of
    %% fun (cuter_lib:lambda())
    %% TODO: Use the same Seen set.
    true ->
      Points = cuter_lib:lambda_kvs(Term),
      Otherwise = cuter_lib:lambda_default(Term),
      Arity = cuter_lib:lambda_arity(Term),
      encode_fun(Points, Otherwise, Arity);
    %% unknown type
    false -> throw({unsupported_term, Term})
  end.

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

encode_fun(Points, Otherwise, Arity) ->
  Ps = [encode_fun_entry(As, V) || {As, V} <- Points],
  O = to_erlang_term(Otherwise),
  #'ErlangTerm'{type='FUN',
                points=Ps,
                arity=Arity,
                otherwise=O}.

encode_fun_entry(Args, Value) ->
  As = [to_erlang_term(A) || A <- Args],
  V = to_erlang_term(Value),
  #'ErlangTerm.FunEntry'{arguments=As, value=V}.

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
  maps:from_list(KVs);
%% any
from_erlang_term(#'ErlangTerm'{type=Type}, _Shared) when Type =:= 'ANY' ->
  ?UNBOUND_VAR_PREFIX;
%% fun
from_erlang_term(T=#'ErlangTerm'{type=Type}, Shared) when Type =:= 'FUN' ->
  Arity = T#'ErlangTerm'.arity,
  Points = [from_fun_entry(E, Shared) || E <- T#'ErlangTerm'.points],
  Otherwise = from_erlang_term(T#'ErlangTerm'.otherwise, Shared),
  cuter_lib:mk_lambda(Points, Otherwise, Arity).

from_map_entry(#'ErlangTerm.MapEntry'{key=K, value=V}, Shared) ->
  {from_erlang_term(K, Shared), from_erlang_term(V, Shared)}.

from_fun_entry(#'ErlangTerm.FunEntry'{arguments=As, value=V}, Shared) ->
  {[from_erlang_term(A, Shared) || A <- As], from_erlang_term(V, Shared)}.

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
  #'SolverCommand'{type='STOP'}.

%% ============================================================================
%% Encode mfa specs to Spec messages.
%% ============================================================================

to_spec({Spec, TypeDeps}) ->
  Clauses = [to_typesig(C) || C <- Spec],
  Ts = cuter_types:erl_type_deps_map(fun to_typedef/1, TypeDeps),
  #'Spec'{clauses = Clauses, typedefs = Ts}.

to_typedef(T) ->
  Name = cuter_types:get_type_name_from_type_dep(T),
  Def = cuter_types:get_type_from_type_dep(T),
  #'Spec.TypeDef'{name = Name, definition = to_type(Def)}.

to_typesig(Fun) ->
  Ret = to_type(cuter_types:ret_of_t_function(Fun)),
  case cuter_types:is_generic_function(Fun) of
    true ->
      #'Spec.FunSig'{signature = {'just_return', Ret}};
    false ->
      Params = [to_type(P) || P <- cuter_types:params_of_t_function_det(Fun)],
      FunDet = #'Spec.FunDet'{parameters = Params, return_value = Ret},
      #'Spec.FunSig'{signature = {'complete', FunDet}}
  end.

to_type(Type) ->
  case cuter_types:get_kind(Type) of
    ?any_tag ->
      #'Spec.Type'{type = 'ANY'};
    ?atom_tag ->
      #'Spec.Type'{type = 'ATOM'};
    ?atom_lit_tag ->
      Atom = to_erlang_term(cuter_types:atom_of_t_atom_lit(Type)),
      #'Spec.Type'{type = 'ATOM_LITERAL', arg = {'literal', Atom}};
    ?float_tag ->
      #'Spec.Type'{type = 'FLOAT'};
    ?integer_tag ->
      #'Spec.Type'{type = 'INTEGER'};
    ?integer_lit_tag ->
      Integer = to_erlang_term(cuter_types:integer_of_t_integer_lit(Type)),
      #'Spec.Type'{type = 'INTEGER_LITERAL', arg = {'literal', Integer}};
    ?list_tag ->
      InnerType = to_type(cuter_types:elements_type_of_t_list(Type)),
      #'Spec.Type'{type = 'LIST', arg = {'inner_type', InnerType}};
    ?nonempty_list_tag ->
      InnerType = to_type(cuter_types:elements_type_of_t_nonempty_list(Type)),
      #'Spec.Type'{type = 'NONEMPTY_LIST', arg = {'inner_type', InnerType}};
    ?nil_tag ->
      #'Spec.Type'{type = 'NIL'};
    ?bitstring_tag ->
      {M, N} = cuter_types:segment_size_of_bitstring(Type),
      SegSz = #'Spec.SegmentSize'{m = integer_to_list(M), n = integer_to_list(N)},
      #'Spec.Type'{type = 'BITSTRING', arg = {'segment_size', SegSz}};
    ?tuple_tag ->
      %% TODO Distinguish between tuple() and {}.
      case cuter_types:elements_types_of_t_tuple(Type) of
        [] ->
          #'Spec.Type'{type = 'TUPLE'};
        Ts ->
          InnerTypes = #'Spec.TypeList'{types = [to_type(T) || T <- Ts]},
          #'Spec.Type'{type = 'TUPLEDET', arg = {'inner_types', InnerTypes}}
      end;
    ?union_tag ->
      Ts = [to_type(T) || T <- cuter_types:elements_types_of_t_union(Type)],
      InnerTypes = #'Spec.TypeList'{types = Ts},
      #'Spec.Type'{type = 'UNION', arg = {'inner_types', InnerTypes}};
    ?range_tag ->
      {Lower, Upper} = cuter_types:bounds_of_t_range(Type),
      Bounds = #'Spec.RangeBounds'{ lower_bound = to_range_bound(Lower)
                                  , upper_bound = to_range_bound(Upper)},
      #'Spec.Type'{type = 'RANGE', arg = {'range_bounds', Bounds}};
    ?function_tag ->
      #'Spec.Type'{type = 'FUN', arg = {'fun', to_typesig(Type)}};
    ?userdef_tag ->
      #'Spec.Type'{type = 'USERDEF', arg = {'type_name', cuter_types:name_of_t_userdef(Type)}}
  end.

to_range_bound(Limit) ->
  case cuter_types:get_kind(Limit) of
    ?integer_lit_tag ->
      Integer = cuter_types:integer_of_t_integer_lit(Limit),
      integer_to_list(Integer);
    Kind when Kind =:= ?pos_inf orelse Kind =:= ?neg_inf ->
      'undefined'
  end.

%% ============================================================================
%% Decode SolverResponse.Model messages.
%% ============================================================================

from_model(Model) ->
  Entries = Model#'SolverResponse.Model'.entries,
  Fn = fun(E) -> { from_erlang_term(E#'SolverResponse.ModelEntry'.var)
                 , from_erlang_term(E#'SolverResponse.ModelEntry'.value) } end,
  maps:from_list([Fn(E) || E <- Entries]).
