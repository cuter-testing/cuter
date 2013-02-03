%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(conc_symb).
%% exports appear alphabetically
-export([abstract/1, append_binary/2, empty_binary/0, ensure_list/2,
	 generate_mapping/2, hd/1,
	 make_bitstring/5, match_bitstring_const/2, match_bitstring_var/2,
	 mock_bif/2, tl/1, tuple_to_list/2]).

%% Abstract a list of concrete values
-spec abstract(Vs :: [term()]) -> Ss :: [atom()].
       
abstract(Vs) ->
    abstract(Vs, [], 1).

abstract([], Acc, _Id) ->
  lists:reverse(Acc);
abstract([_A|As], Acc, Id) ->
  X = "_symb" ++ (erlang:integer_to_list(Id)),
  SymbA = erlang:list_to_atom(X),
  abstract(As, [SymbA|Acc], Id+1).


%% Generate a mapping between the concrete values
%% and their symbolic abstraction
-spec generate_mapping([atom()], [term()]) -> [{atom(), term()}].

generate_mapping(Ss, Vs) ->
  lists:zip(Ss, Vs).

%% ------------------------------------------------------------------------
%% mock_bif/2
%% Mocks the execution of an erlang bif and returns a symbolic 
%% represenation of its result
%% ------------------------------------------------------------------------
-spec mock_bif({M :: atom(), F :: atom(), A :: non_neg_integer()}, Args) ->
	 'true' | {atom(), Args} when Args :: [term()].

mock_bif({erlang, demonitor, 1}, _Args) -> true;
mock_bif({erlang, display, 1}, _Args) -> true;
mock_bif({erlang, exit, 2}, _Args) -> true;
mock_bif({erlang, garbage_collect, 0}, _Args) -> true;
mock_bif({erlang, group_leader, 2}, _Args) -> true;
mock_bif({erlang, link, 1}, _Args) -> true;
mock_bif({erlang, monitor_node, 2}, _Args) -> true;
mock_bif({erlang, monitor_node, 3}, _Args) -> true;
mock_bif({erlang, port_close, 1}, _Args) -> true;
mock_bif({erlang, port_command, 2}, _Args) -> true;
mock_bif({erlang, port_connect, 2}, _Args) -> true;
mock_bif({erlang, register, 2}, _Args) -> true;
mock_bif({erlang, resume_process, 1}, _Args) -> true;
mock_bif({erlang, set_cookie, 2}, _Args) -> true;
mock_bif({erlang, unlink, 1}, _Args) -> true;
mock_bif({erlang, unregister, 1}, _Args) -> true;
mock_bif({erlang, yield, 0}, _Args) -> true;
mock_bif({M, F, A}, Args) ->
  X = atom_to_list(M) ++ ":" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A),
  {list_to_atom(X), Args}.
  
%% ------------------------------------------------------------------------
%% tuple_to_list/2
%% tuple_to_list/3 (helper function)
%% To create a list of N elements from a symbolic term
%% that represents a tuple (N is user defined)
%% ------------------------------------------------------------------------
-spec tuple_to_list(S, N) -> L
  when S :: term(),
       N :: non_neg_integer(),
       L :: [term()].

tuple_to_list(S, N) ->
  tuple_to_list(S, N, []).
  
tuple_to_list(_S, 0, Acc) ->
  Acc;
tuple_to_list(S, N, Acc) ->
  X = mock_bif({erlang, element, 2}, [N, S]),
  tuple_to_list(S, N-1, [X|Acc]).

%% ------------------------------------------------------------------------
%% hd/1
%% Get the head of a symbolic term that represents a list
%% ------------------------------------------------------------------------
-spec hd(S) -> Hd
  when S  :: term(),
       Hd :: term().
       
hd(S) when is_list(S) ->
  erlang:hd(S);
hd(S) ->
  mock_bif({erlang, hd, 1}, [S]).

%% ------------------------------------------------------------------------
%% tl/1
%% Get the tail of a symbolic term that represents a list
%% ------------------------------------------------------------------------
-spec tl(S) -> Tl
  when S  :: term(),
       Tl :: term().
       
tl(S) when is_list(S) ->
  erlang:tl(S);
tl(S) ->
  mock_bif({erlang, tl, 1}, [S]).
  
%% ------------------------------------------------------------------------
%% ensure_list/2
%% Ensures that a symbolic term is a list of N elements
%% (N is user defined)
%% ------------------------------------------------------------------------
-spec ensure_list(S, N) -> L
  when S :: term(),
       N :: pos_integer(),
       L :: [term()].
       
ensure_list(S, N) when is_list(S) ->
  case length(S) of
    N -> S;
    _ -> listify(S, N)
  end;
ensure_list(S, N) ->
  listify(S, N).
  
%% ------------------------------------------------------------------------
%% listify/2
%% Creates a list of N elements from a symbolic term
%% (N is user defined)
%% ------------------------------------------------------------------------
-spec listify(S :: term(), N :: pos_integer()) -> L :: [term()].
       
listify(S, N) ->
  [mock_bif({lists, nth, 2}, [X, S]) || X <- lists:seq(1, N)].

%% ========================
%% for use in binaries
%% TODO Needs Revision !!!
%% ========================
empty_binary() ->
  {binary, []}.
  
append_binary(Sv, {binary, Acc}) when is_list(Acc) ->
  {binary, [Sv|Acc]}.
  
make_bitstring(Sv, Size, Unit, Type, Flags) ->
  Sign = conc_lib:get_signedness(Flags),
  End = conc_lib:get_endianess(Flags),
  {bitstring, {Sv, Size, Unit, Type, Sign, End}}.
  
match_bitstring_const(Cnst, Sv) ->
  {bitstr_const_match_rest, {Cnst, Sv}}.
  
match_bitstring_var(SEnc, Sv) ->
  X = {bitstr_var_match_x, {SEnc, Sv}},
  Rest = {bitstr_var_match_rest, {SEnc, Sv}},
  {X, Rest}.
