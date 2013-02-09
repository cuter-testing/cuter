%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(concolic_symbolic).

%% exports appear alphabetically
-export([abstract/1, append_binary/2, empty_binary/0, ensure_list/2, hd/1,
         make_bitstring/5, match_bitstring_const/2, match_bitstring_var/2,
         mock_bif/3, tl/1, tuple_to_list/2]).

-export_type([mapping/0, sbitstring/0, symbolic/0]).

%% Macros for code abstractions
-define(UNDEF, '_undefined').
-define(SYMBOLIC_PREFIX, '__s').
-define(SYMBOLIC_VAR, '__symbvar').

-type bif() :: {atom(), atom(), non_neg_integer()}.
-type sbitstring() :: {'bitstr', [term()]}.
-type encoding()   :: {bin_lib:bsize(), bin_lib:bunit(), bin_lib:btype(), [bin_lib:bflag()]}.
-type mapping()    :: {atom(), term()}.
-type symbolic()   :: {?SYMBOLIC_PREFIX, atom()}
                    | {?SYMBOLIC_PREFIX, atom(), [term(), ...]}.

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
is_symbolic({?SYMBOLIC_PREFIX, BIF, As}) when is_atom(BIF), is_list(As) -> 'true';
is_symbolic({?SYMBOLIC_PREFIX, SymbVar}) when is_atom(SymbVar) -> 'true';
is_symbolic(_V) -> 'false'.

%% ------------------------------------------------------------------------
%% TODO This function needs refining
%% mock_bif/2
%% Mocks the execution of an erlang bif and returns a symbolic 
%% represenation of its result
%% (the concrete result is given as parameter to the function)
%% ------------------------------------------------------------------------
-spec mock_bif(bif(), [term()], term()) -> term() | symbolic().

%% BIFs that always return 'true'
mock_bif({erlang, demonitor, 1}, _Args, true) -> true;
mock_bif({erlang, display, 1}, _Args, true) -> true;
mock_bif({erlang, exit, 2}, _Args, true) -> true;
mock_bif({erlang, garbage_collect, 0}, _Args, true) -> true;
mock_bif({erlang, group_leader, 2}, _Args, true) -> true;
mock_bif({erlang, link, 1}, _Args, true) -> true;
mock_bif({erlang, monitor_node, 2}, _Args, true) -> true;
mock_bif({erlang, monitor_node, 3}, _Args, true) -> true;
mock_bif({erlang, port_close, 1}, _Args, true) -> true;
mock_bif({erlang, port_command, 2}, _Args, true) -> true;
mock_bif({erlang, port_connect, 2}, _Args, true) -> true;
mock_bif({erlang, register, 2}, _Args, true) -> true;
mock_bif({erlang, resume_process, 1}, _Args, true) -> true;
mock_bif({erlang, set_cookie, 2}, _Args, true) -> true;
mock_bif({erlang, unlink, 1}, _Args, true) -> true;
mock_bif({erlang, unregister, 1}, _Args, true) -> true;
mock_bif({erlang, yield, 0}, _Args, true) -> true;
%% BIFs with arity 0 return the concrete result
mock_bif({_M, _F, 0}, _Args, Cv) -> Cv;
%% Symbolic abstraction of a BIF
mock_bif({M, F, A}, Args,  Cv) ->
  case lists:any(fun is_symbolic/1, Args) orelse Cv =:= ?UNDEF of
    true ->
      X = atom_to_list(M) ++ ":" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A),
      {?SYMBOLIC_PREFIX, list_to_atom(X), Args};
    false ->
      Cv
  end.
  
%% ------------------------------------------------------------------------
%% tuple_to_list/2
%% To create a list of N elements from a symbolic term
%% that represents a tuple (N is user defined)
%% ------------------------------------------------------------------------
-spec tuple_to_list(term(), non_neg_integer()) -> [symbolic() | term()].
tuple_to_list(S, N) when is_tuple(S) ->
  case is_symbolic(S) of
    true ->
      break_term('tuple', S, N);
    false ->
      L = erlang:tuple_to_list(S),
      case length(L) =:= N of
        true  -> L;
        false -> break_term('tuple', S, N)
      end
  end.

%% ------------------------------------------------------------------------
%% hd/1
%% Get the head of a symbolic term that represents a list
%% ------------------------------------------------------------------------
-spec hd(term()) -> term().
  
hd(S) when is_list(S) ->
  erlang:hd(S);
hd(S) ->
  mock_bif({erlang, hd, 1}, [S], ?UNDEF).

%% ------------------------------------------------------------------------
%% tl/1
%% Get the tail of a symbolic term that represents a list
%% ------------------------------------------------------------------------
-spec tl(term()) -> term().
  
tl(S) when is_list(S) ->
  erlang:tl(S);
tl(S) ->
  mock_bif({erlang, tl, 1}, [S], ?UNDEF).
  
%% ------------------------------------------------------------------------
%% ensure_list/2
%% Ensures that a symbolic term is a list of N elements
%% (N is user defined)
%% ------------------------------------------------------------------------
-spec ensure_list(term(), pos_integer()) -> [term() | symbolic()].
  
ensure_list(S, N) when is_list(S) ->
  case length(S) of
    N -> S;
    _ -> break_term('list', S, N)
  end;
ensure_list(S, N) ->
  break_term('list', S, N).
  
%% ------------------------------------------------------------------------
%% break_term/2
%% Creates a list of N elements from a symbolic term that represents
%% a list or a tuple (N is user defined)
%% ------------------------------------------------------------------------
-spec break_term('tuple' | 'list', term(), pos_integer()) -> [symbolic()].

break_term('tuple', S, N) ->
  [mock_bif({erlang, element, 2}, [X, S], ?UNDEF) || X <- lists:seq(1, N)];
break_term('list', S, N) ->
  [mock_bif({lists, nth, 2}, [X, S], ?UNDEF) || X <- lists:seq(1, N)].

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
  

