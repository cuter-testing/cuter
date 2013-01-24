-module(conc_symb).
-compile(export_all).

generate_mapping (Vs, Ss) ->
  generate_mapping (Vs, Ss, []).
  
generate_mapping ([], [], Acc) ->
  lists:reverse(Acc);
generate_mapping ([V|Vs], [S|Ss], Acc) ->
  generate_mapping (Vs, Ss, [{S, V}|Acc]).

abstract(As) -> abstract(As, [], 1).

abstract([], Acc, _Id) ->
  lists:reverse(Acc);
abstract([_A|As], Acc, Id) ->
  X = "_symb" ++ (erlang:integer_to_list(Id)),
  SymbA = erlang:list_to_atom(X),
  abstract(As, [SymbA|Acc], Id+1).
  
  
%% ===============
%% mock_bif
%% ===============
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
%  {{M, F, A}, Args}.
%  X = erlang:hd(atom_to_list(M)),
  {F,Args}.
  
%% ===============
%% tuple_to_list
%% ===============
tuple_to_list(S, N) when is_tuple(S) ->
  Ss = tuple_to_list(S),
  case length(Ss) of
    N ->
      Ss;
    _ ->
      tuple_to_list(S, N, [])
  end;
tuple_to_list(S, N) ->
  tuple_to_list(S, N, []).
  
tuple_to_list(_S, 0, Acc) ->
  Acc;
tuple_to_list(S, N, Acc) ->
  X = mock_bif({erlang, element, 2}, [N, S]),
  tuple_to_list(S, N-1, [X|Acc]).

%% ===============
%% hd
%% ===============
hd(S) when is_list(S) ->
  erlang:hd(S);
hd(S) ->
  mock_bif({erlang, hd, 1}, [S]).

%% ===============
%% tl
%% ===============
tl(S) when is_list(S) ->
  erlang:tl(S);
tl(S) ->
  mock_bif({erlang, tl, 1}, [S]).
  
%% ===============
%% ensure_list
%% ===============
ensure_list(S, N) when is_list(S) ->
  case length(S) of
    N -> S;
    _ -> listify(S, N)
  end;
ensure_list(S, N) ->
  listify(S, N).
  
%% ===============
%% listify
%% ===============
listify(S, N) ->
  L = lists:seq(1,N),
  lists:map(
    fun(X) -> {{lists,nth,X}, [S]} end,
    L
  ).

%% ===============
%% binaries
%% ===============
  
empty_binary() ->
  {binary, []}.
  
append_binary(Sv, {binary, Acc}) when is_list(Acc) ->
  {binary, [Sv|Acc]}.

get_signedness([unsigned | _Fls]) -> unsigned;
get_signedness([signed | _Fls]) -> signed;
get_signedness([_Fl | Fls]) -> get_signedness(Fls).

get_endianess([big | _Fls]) -> big;
get_endianess([little | _Fls]) -> little;
get_endianess([native | _Fls]) -> native;
get_endianess([_Fl | Fls]) -> get_endianess(Fls).
  
make_bitstring(Sv, Size, Unit, Type, Flags) ->
  Sign = get_signedness(Flags),
  End = get_endianess(Flags),
  {bitstring, {Sv, Size, Unit, Type, Sign, End}}.
  
match_bitstring_const(Cnst, Sv) ->
  {bitstr_const_match_rest, {Cnst, Sv}}.
  
match_bitstring_var(SEnc, Sv) ->
  X = {bitstr_var_match_x, {SEnc, Sv}},
  Rest = {bitstr_var_match_rest, {SEnc, Sv}},
  {X, Rest}.

