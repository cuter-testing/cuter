%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(concolic_symbolic).

%% exports appear alphabetically
-export([abstract/1, append_segments/2, ensure_list/4, hd/3,
         make_bitstring/4, match_bitstring_const/5, match_bitstring_var/5,
         mock_bif/4, tl/3, tuple_to_list/4, is_symbolic/1]).

-export_type([mapping/0, symbolic/0]).

%% Macros for code abstractions
-define(CONCAT_BITSTR, 'concat_bitstr').
-define(MAKE_BITSTR, 'make_bitstr').
-define(MATCH_BITSTR_C, 'match_bitstr_c').
-define(MATCH_BITSTR_X, 'match_bitstr_x').
-define(MATCH_BITSTR_R, 'match_bitstr_r').
-define(SYMBOLIC_PREFIX, '__s').
-define(SYMBOLIC_VAR, '__symbvar').
-define(BITOP, '__bitop').

-type bitop()    :: ?CONCAT_BITSTR
                  | ?MAKE_BITSTR
                  | ?MATCH_BITSTR_C
                  | ?MATCH_BITSTR_X
                  | ?MATCH_BITSTR_R.
-type encoding() :: {bin_lib:bsize(), bin_lib:bunit(), bin_lib:btype(), [bin_lib:bflag()]}.
-type mapping()  :: {atom(), term()}.
-type maybe(X)   :: {'some', X} | 'none'.
-type maybe_s(X) :: symbolic() | X.
-type symbolic() :: {?SYMBOLIC_PREFIX, atom()}             %% Symbolic Variable
                  | {?SYMBOLIC_PREFIX, atom(), [term()]}.  %% Symbolic Value

%% =============================================================
%% Basic operations on symbolic variables and values
%% =============================================================

%% Create a fresh symbolic variable
-spec fresh_symbolic_var() -> symbolic().

fresh_symbolic_var() ->
  Id = erlang:ref_to_list(erlang:make_ref()) -- "#Ref<>",
  X = erlang:atom_to_list(?SYMBOLIC_VAR) ++ Id,
  {?SYMBOLIC_PREFIX, erlang:list_to_atom(X)}.

%% Abstract a list of concrete values and return
%% the symbolic variables and the mapping
-spec abstract([term()]) -> {[symbolic()], [mapping()]}.

abstract(Vs) ->
  Symbs = [fresh_symbolic_var() || _ <- lists:seq(1, erlang:length(Vs))],
  Maps = lists:zip(Symbs, Vs),
  {Symbs, Maps}.
  
%% Check whether a term represents a symbolic value or not
-spec is_symbolic(term()) -> boolean().

is_symbolic({?SYMBOLIC_PREFIX, BIF, As}) when is_atom(BIF), is_list(As) -> true;
is_symbolic({?SYMBOLIC_PREFIX, SymbVar}) when is_atom(SymbVar) -> true;
is_symbolic(_V) -> false.

%% Check whether a term is a symbolic variable or not
is_symbolic_var({?SYMBOLIC_PREFIX, SymbVar}) when is_atom(SymbVar) -> true;
is_symbolic_var(_V) -> false.

%% =============================================================
%% Symbolic representations of term operations
%% =============================================================

%% Symbolically represent the call of an erlang BIF
-spec mock_bif(mfa(), [term()], term(), file:io_device()) -> maybe_s(term()).

%% BIFs that always return 'true'
mock_bif({erlang, demonitor, 1}, _Args, true, _Fd) -> true;
mock_bif({erlang, display, 1}, _Args, true, _Fd) -> true;
mock_bif({erlang, exit, 2}, _Args, true, _Fd) -> true;
mock_bif({erlang, garbage_collect, 0}, _Args, true, _Fd) -> true;
mock_bif({erlang, group_leader, 2}, _Args, true, _Fd) -> true;
mock_bif({erlang, link, 1}, _Args, true, _Fd) -> true;
mock_bif({erlang, monitor_node, 2}, _Args, true, _Fd) -> true;
mock_bif({erlang, monitor_node, 3}, _Args, true, _Fd) -> true;
mock_bif({erlang, port_close, 1}, _Args, true, _Fd) -> true;
mock_bif({erlang, port_command, 2}, _Args, true, _Fd) -> true;
mock_bif({erlang, port_connect, 2}, _Args, true, _Fd) -> true;
mock_bif({erlang, register, 2}, _Args, true, _Fd) -> true;
mock_bif({erlang, resume_process, 1}, _Args, true, _Fd) -> true;
mock_bif({erlang, set_cookie, 2}, _Args, true, _Fd) -> true;
mock_bif({erlang, unlink, 1}, _Args, true, _Fd) -> true;
mock_bif({erlang, unregister, 1}, _Args, true, _Fd) -> true;
mock_bif({erlang, yield, 0}, _Args, true, _Fd) -> true;
%% BIFs with arity 0 return the concrete result
mock_bif({_M, _F, 0}, _Args, Cv, _Fd) -> Cv;
%% Symbolic execution of a BIF
mock_bif(BIF, Args, Cv, Fd) ->  
  case lists:any(fun is_symbolic/1, Args) of
    true  -> abstract_mfa_call(BIF, Args, Fd);
    false -> Cv
  end.

%% Abstract an MFA call
-spec abstract_mfa_call(mfa(), [term()], file:io_device()) -> symbolic().

abstract_mfa_call(MFA, Args, Fd) ->
  Ns = lists:seq(1, length(Args)),
  L = lists:zip(Ns, Args),
  Args_n = add_vars(L, orddict:from_list(L), Fd),
  %% Mock the call
  MFA_a = mfa_to_atom(MFA),
  {?SYMBOLIC_PREFIX, MFA_a, Args_n}.

%% Return an atom representing an erlang MFA
-spec mfa_to_atom(mfa()) -> atom().

mfa_to_atom({M, F, A}) when is_atom(M), is_atom(F), is_integer(A) ->
  X = atom_to_list(M) ++ ":" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A),
  list_to_atom(X).
  
%% Substitute all the same terms in the Args of an MFA call
%% so as to manually preserve sharing
-spec add_vars([{integer(), term()}], orddict:orddict(), file:io_device()) -> [term()].

add_vars([], Dict, _Fd) ->
  L = lists:sort(orddict:to_list(Dict)),
  {_Ns, Args} = lists:unzip(L),
  Args;
add_vars([{N, A}|As], Dict, Fd) ->
  %% Find the same terms
  Same = lists:filter(fun({_I, Arg}) -> erts_debug:same(A, Arg) end, As),
  case Same of
    [] ->
      add_vars(As, Dict, Fd);
    _ ->
      case is_symbolic_var(A) of
        true -> 
          add_vars(As -- Same, Dict, Fd);
        %% Substitute if the term is not a symbolic variable
        false ->
          SVar = add_symbolic_var(A, Fd),
          NDict = 
            lists:foldl(fun({I, _Arg}, D) -> orddict:store(I, SVar, D) end,
              Dict, [{N, A}|Same]),
          add_vars(As -- Same, NDict, Fd)
      end
  end.
  
%% Create a new symbolic variable to substitute a term
-spec add_symbolic_var(term(), file:io_device()) -> symbolic().
  
add_symbolic_var(S, Fd) ->
  SVar = fresh_symbolic_var(),
  'ok' = concolic_encdec:log_eq(Fd, 'eq', SVar, S),
  SVar.
  
%% Create a list of N elements from a symbolic term
%% that represents a tuple with size N
-spec tuple_to_list(maybe_s(tuple()), non_neg_integer(), [term()], file:io_device()) ->
  [maybe_s(term())].
  
tuple_to_list(S, N, Cv, Fd) ->
  case is_symbolic(S) of
    true  -> break_term('tuple', S, N, Cv, Fd);
    false -> erlang:tuple_to_list(S)
  end.

%% Return the head of a symbolic term that represents a list
-spec hd(maybe_s([term()]), term(), file:io_device()) -> term().
  
hd(S, _Cv, _Fd) when is_list(S) ->
  erlang:hd(S);
hd(S, Cv, Fd) ->
  mock_bif({erlang, hd, 1}, [S], Cv, Fd).

%% Return the tail of a symbolic term that represents a list
-spec tl(maybe_s([term()]), term(), file:io_device()) -> term().
  
tl(S, _Cv, _Fd) when is_list(S) ->
  erlang:tl(S);
tl(S, Cv, Fd) ->
  mock_bif({erlang, tl, 1}, [S], Cv, Fd).
  
%% Ensures that a symbolic term is a list of N elements
-spec ensure_list(maybe_s([term()]), pos_integer(), term(), file:io_device()) -> [maybe_s(term())].
  
ensure_list(S, N, Cv, Fd) ->
  case is_symbolic(S) of
    true  -> break_term('list', S, N, Cv, Fd);
    false -> S
  end.
  
%% Create a list of N elements from a symbolic term that represents
%% a list or a tuple with size N
-spec break_term('tuple' | 'list', symbolic(), pos_integer(), term(), file:io_device()) -> [symbolic()].

break_term(M, S, N, Cv, Fd) when M =:= 'tuple'; M =:= 'list'->
  BIF =
    case M of
      'tuple' -> {erlang, element, 2};
      'list'  -> {lists, nth, 2}
    end,
  SV = 
    case is_symbolic_var(S) of
      true  -> S;
      false -> add_symbolic_var(S, Fd)
    end,  
  [mock_bif(BIF, [X, SV], Cv, Fd) || X <- lists:seq(1, N)].
  
%% =============================================================
%% Symbolic representation of binaries and binary operations
%% =============================================================

%% Wrap a symbolic bitstring operation as an mfa
-spec bitop_to_mfa(bitop()) -> mfa().

bitop_to_mfa(Op) -> {?BITOP, Op, 0}.

%% Symbolically represent bitstring concatenation
-spec append_segments([maybe_s(bitstring())], file:io_device()) -> maybe_s(bitstring()).

append_segments(Segs, Fd) ->
  append_segments(lists:reverse(Segs), [], Fd).

append_segments([], Acc, Fd) ->
  Op = bitop_to_mfa(?CONCAT_BITSTR),
  abstract_mfa_call(Op, Acc, Fd);
append_segments([S], [], _Fd) ->
  S;
append_segments([S], Acc, Fd) ->
  append_segments([], [S|Acc], Fd);
append_segments([S1,S2|Ss], Acc, Fd) ->
  case {is_symbolic(S1), is_symbolic(S2)} of
    {false, false} ->
      Bin = <<S2/bitstring, S1/bitstring>>,
      append_segments([Bin|Ss], Acc, Fd);
    {true, false} ->
      append_segments([S2|Ss], [S1|Acc], Fd);
    _ ->
      append_segments(Ss, [S2,S1|Acc], Fd)
  end.
  
%% Encode a term into a bitstring
-spec make_bitstring(term(), encoding(), maybe(bitstring()), file:io_device()) ->
  maybe_s(bitstring()).
  
make_bitstring(Sv, {Size, Unit, Type, Flags}, Cv, Fd) ->
  case is_symbolic(Sv) orelse is_symbolic(Size) of
    true ->
      Sign = concolic_lib:get_signedness(Flags),
      End = concolic_lib:get_endianess(Flags),
      Op = bitop_to_mfa(?MAKE_BITSTR),
      abstract_mfa_call(Op, [Sv, Size, Unit, Type, Sign, End], Fd);
    false ->
      case Cv of
        'none' ->
          try
            bin_lib:make_bitstring(Sv, Size, Unit, Type, Flags)
          catch 
            error:badarg -> exit({1, Sv})
          end;
        {'some', Bin} -> 
          Bin
      end
  end.
  
%% Symbolic representation of pattern matching a symbolic bitstring
%% to an encoded term and return the rest of the symbolic bitstring
-spec match_bitstring_const(term(), encoding(), maybe_s(term()), bitstring(), file:io_device()) ->
  {maybe_s(bitstring()), maybe_s(bitstring())}.

match_bitstring_const(Cnst, {Size, Unit, Type, Flags}, Sv, Cv, Fd) ->
  CnstBin = make_bitstring(Cnst, {Size, Unit, Type, Flags}, 'none', Fd),
  case is_symbolic(CnstBin) orelse is_symbolic(Sv) of
    true  -> 
      Op = bitop_to_mfa(?MATCH_BITSTR_C),
      {Cnst, abstract_mfa_call(Op, [CnstBin, Sv], Fd)};
    false ->
      {CnstBin, Cv}
  end.

%% Symbolic representation of pattern matching a symbolic bitstring
%% to an encoded term and return the matched value and the
%% rest of the symbolic bitstring
-spec match_bitstring_var(encoding(), maybe_s(bitstring()), maybe_s(bitstring()), maybe_s(bitstring()), file:io_device()) ->
  {maybe_s(bitstring()), maybe_s(bitstring())}.

match_bitstring_var(Enc, Sv, CX, CRest, Fd) ->
  case is_symbolic(Sv) of
    true ->
      X = abstract_mfa_call(bitop_to_mfa(?MATCH_BITSTR_X), [Enc, Sv], Fd),
      R = abstract_mfa_call(bitop_to_mfa(?MATCH_BITSTR_R), [Enc, Sv], Fd),
      {X, R};
    false ->
      {CX, CRest}
  end.
  
  
