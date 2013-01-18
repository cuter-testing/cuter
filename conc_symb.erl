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
mock_bif({M, F, A}, Args) ->
  {{M, F, A}, Args}.
  
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
  X = {{erlang, element, 2}, [N, S]},
  tuple_to_list(S, N-1, [X|Acc]).

%% ===============
%% hd
%% ===============
hd(S) when is_list(S) ->
  erlang:hd(S);
hd(S) ->
  {{erlang, hd, 1}, [S]}.

%% ===============
%% tl
%% ===============
tl(S) when is_list(S) ->
  erlang:tl(S);
tl(S) ->
  {{erlang, tl, 1}, [S]}.

