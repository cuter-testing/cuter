%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(concolic_symbolic).

%% exports appear alphabetically
-export([abstract/1, append_binary/2, empty_binary/0, ensure_list/4, hd/3,
         make_bitstring/5, match_bitstring_const/2, match_bitstring_var/2,
         mock_bif/4, tl/3, tuple_to_list/4, is_symbolic/1]).

-export_type([mapping/0, sbitstring/0, symbolic/0]).

%% Macros for code abstractions
-define(SYMBOLIC_PREFIX, '__s').
-define(SYMBOLIC_VAR, '__symbvar').

-type sbitstring() :: {'bitstr', [term()]}.
-type encoding()   :: {bin_lib:bsize(), bin_lib:bunit(), bin_lib:btype(), [bin_lib:bflag()]}.
-type mapping()    :: {atom(), term()}.
-type symbolic()   :: {?SYMBOLIC_PREFIX, atom()}             %% Symbolic Variable
                    | {?SYMBOLIC_PREFIX, atom(), [term()]}.  %% Symbolic Value

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

is_symbolic({?SYMBOLIC_PREFIX, BIF, As}) when is_atom(BIF), is_list(As) -> 'true';
is_symbolic({?SYMBOLIC_PREFIX, SymbVar}) when is_atom(SymbVar) -> 'true';
is_symbolic(_V) -> 'false'.

%% Check whether a term is a symbolic variable or not
is_symbolic_var({?SYMBOLIC_PREFIX, SymbVar}) when is_atom(SymbVar) -> 'true';
is_symbolic_var(_V) -> 'false'.

%% ------------------------------------------------------------------------
%% TODO This function needs refining
%% mock_bif/2
%% Mocks the execution of an erlang bif and returns a symbolic 
%% represenation of its result
%% (the concrete result is given as parameter to the function)
%% ------------------------------------------------------------------------
-spec mock_bif(mfa(), [term()], term(), file:io_device()) -> term() | symbolic().

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
%% Symbolic abstraction of a BIF
mock_bif(BIF, Args, Cv, Fd) ->  
  case lists:any(fun is_symbolic/1, Args) of
    true  -> abstract_bif_call(BIF, Args, Fd);
    false -> Cv
  end.  

%% Abstract a BIF call
-spec abstract_bif_call(mfa(), [term()], file:io_device()) -> symbolic().

abstract_bif_call(BIF, Args, Fd) ->
  Ns = lists:seq(1, length(Args)),
  L = lists:zip(Ns, Args),
  Args_n = add_vars(L, orddict:from_list(L), Fd),
  %% Mock the call
  BIF_a = bif_to_atom(BIF),
  {?SYMBOLIC_PREFIX, BIF_a, Args_n}.

%% Return an atom representing an erlang BIF
-spec bif_to_atom(mfa()) -> atom().

bif_to_atom({M, F, A}) when is_atom(M), is_atom(F), is_integer(A) ->
  X = atom_to_list(M) ++ ":" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A),
  list_to_atom(X).
  
%% Substiture all the same terms in the Args of a bif call
%% so as to manually preserver sharing
-spec add_vars([{integer(), term()}], orddict:orddict(), file:io_device()) -> [term()].

add_vars([], Dict, _Fd) ->
  L = lists:sort(orddict:to_list(Dict)),
  {_Ns, Args} = lists:unzip(L),
  Args;
add_vars([{N, A}|As], Dict, Fd) ->
  Same = lists:filter(fun({_I, Arg}) -> erts_debug:same(A, Arg) end, As),
  case Same of
    [] ->
      add_vars(As, Dict, Fd);
    _ ->
      case is_symbolic_var(A) of
        true -> 
          add_vars(As -- Same, Dict, Fd);
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
  
%% ------------------------------------------------------------------------
%% tuple_to_list
%% To create a list of N elements from a symbolic term
%% that represents a tuple (N is user defined)
%% ------------------------------------------------------------------------
-spec tuple_to_list(term(), non_neg_integer(), term(), file:io_device()) -> [symbolic() | term()].
tuple_to_list(S, N, Cv, Fd) when is_tuple(S) ->
  case is_symbolic(S) of
    true ->
      break_term('tuple', S, N, Cv, Fd);
    false ->
      case erlang:size(S) =:= N of
        true  -> erlang:tuple_to_list(S);
        false -> break_term('tuple', S, N, Cv, Fd)
      end
  end;
tuple_to_list(S, N, Cv, Fd) ->
  break_term('tuple', S, N, Cv, Fd).

%% ------------------------------------------------------------------------
%% hd
%% Get the head of a symbolic term that represents a list
%% ------------------------------------------------------------------------
-spec hd(term(), term(), file:io_device()) -> term().
  
hd(S, _Cv, _Fd) when is_list(S) ->
  erlang:hd(S);
hd(S, Cv, Fd) ->
  mock_bif({erlang, hd, 1}, [S], Cv, Fd).

%% ------------------------------------------------------------------------
%% tl
%% Get the tail of a symbolic term that represents a list
%% ------------------------------------------------------------------------
-spec tl(term(), term(), file:io_device()) -> term().
  
tl(S, _Cv, _Fd) when is_list(S) ->
  erlang:tl(S);
tl(S, Cv, Fd) ->
  mock_bif({erlang, tl, 1}, [S], Cv, Fd).
  
%% ------------------------------------------------------------------------
%% ensure_list
%% Ensures that a symbolic term is a list of N elements
%% (N is user defined)
%% ------------------------------------------------------------------------
-spec ensure_list(term(), pos_integer(), term(), file:io_device()) -> [term() | symbolic()].
  
ensure_list(S, N, Cv, Fd) when is_list(S) ->
  case length(S) of
    N -> S;
    _ -> break_term('list', S, N, Cv, Fd)
  end;
ensure_list(S, N, Cv, Fd) ->
  break_term('list', S, N, Cv, Fd).
  
%% ------------------------------------------------------------------------
%% break_term
%% Creates a list of N elements from a symbolic term that represents
%% a list or a tuple (N is user defined)
%% ------------------------------------------------------------------------
-spec break_term('tuple' | 'list', term(), pos_integer(), term(), file:io_device()) -> [symbolic()].

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
  
%% ========================
%% for use in binaries
%% TODO Needs Revision !!!
%% ========================

%% Symbolic representation of an empty binary
-spec empty_binary() -> sbitstring().

empty_binary() ->
  {'bitstr', []}.
  
%% Append a symbolic bitstring to a symbolic binary
-spec append_binary(term(), sbitstring()) -> sbitstring().
  
append_binary(Sv, {'bitstr', Acc}) when is_list(Acc) ->
  {'bitstr', [Sv|Acc]}.
  
%% Create a symbolic bitstring from a term with a specific encoding
-spec make_bitstring(term(), bin_lib:bsize(), bin_lib:bunit(), bin_lib:btype(), [bin_lib:bflag()]) -> sbitstring().
make_bitstring(Sv, Size, Unit, Type, Flags) ->
  %%  Sign = concolic_lib:get_signedness(Flags),
  %%  End = concolic_lib:get_endianess(Flags),
  {'bitstr', [{Sv, [Size, Unit, Type, Flags]}]}.
  
%% Symbolic representation of pattern matching a symbolic binary
%% to an symbolic bitstring and return the rest of the symbolic binary
-spec match_bitstring_const(term(), sbitstring()) -> {'bitstr_c_rest', {term(), sbitstring()}}.

match_bitstring_const(Cnst, Sv) ->
  {'bitstr_c_rest', {Cnst, Sv}}.
  
%% Symbolic representation of pattern matching a symbolic binary
%% to an encoded symbolic bitstring and return 
%% the matched value and the rest of the symbolic binary
-spec match_bitstring_var(encoding(), sbitstring()) ->
  {{'bitstr_v_x', {term(), sbitstring()}}, {'bitstr_v_rest', {term(), sbitstring()}}}.

match_bitstring_var(SEnc, Sv) ->
  X = {'bitstr_v_x', {SEnc, Sv}},
  Rest = {'bitstr_v_rest', {SEnc, Sv}},
  {X, Rest}.
  

