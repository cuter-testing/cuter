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
  X = "_symb" ++ (integer_to_list(Id)),
  SymbA = list_to_atom(X),
  abstract(As, [SymbA|Acc], Id+1).
  
  
  
mock_bif({M, F, A}, Args) ->
  {{M, F, A}, Args}.
