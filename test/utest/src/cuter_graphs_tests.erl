%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_graphs_tests).

-include_lib("eunit/include/eunit.hrl").
%-include("include/eunit_config.hrl").
%-include("include/cuter_macros.hrl").

-spec test() -> ok | {error, any()}.

-spec calculate_dag_callgraph_test_() -> [{string(), {'setup', fun(), fun(), fun()}}].
calculate_dag_callgraph_test_() ->
  EntryPoints = [
		 {callgraph_examples, f1, 1},
		 {callgraph_examples, f2, 1},
		 {callgraph_examples, f3, 1},
		 {callgraph_examples, f4, 1}
		],
  Setup = fun(E) -> fun() -> setup(E) end end,
  Inst = fun validate_callgraph/1,
  Cleanup = fun cleanup/1,
  [{"Calculate callgraphs", {setup, Setup(E), Cleanup, Inst}} || E <- EntryPoints].

setup(EntryPoint) ->
  {G, _, NewEntryPoint} = cuter_graphs:calculate_dag_callgraph(EntryPoint),
  {EntryPoint, NewEntryPoint, G}.

validate_callgraph({EntryPoint, NewEntryPoint, G}) ->
  E = fun(F, A) -> {callgraph_examples, F, A} end,
  N = fun(X) -> {node, X} end,
  C = fun(X) -> {cycle, X} end,
  case EntryPoint of
    {callgraph_examples, f1, 1} ->
      Nodes = [N(E(f1, 1)), N(E(f11, 1))],
      Children = [[N(E(f11, 1))], []],
      NewEntryPoint1 = N(E(f1, 1));
    {callgraph_examples, f2, 1} ->
      Nodes = [C([E(f2, 1), E(f21, 1)])],
      Children = [[]],
      NewEntryPoint1 = C([E(f2, 1), E(f21, 1)]);
    {callgraph_examples, f3, 1} ->
      Nodes = [
	       N(E(f3, 1)),
	       C([E(f31, 1), E(f32, 1)]),
	       N(E(f33, 1))
	      ],
      Children = [
		  [C([E(f31, 1), E(f32, 1)])],
		  [N(E(f33, 1))],
		  []
		 ],
      NewEntryPoint1 = N(E(f3, 1));
    {callgraph_examples, f4, 1} ->
      Nodes = [
	       C([E(f4, 1), E(f41, 1), E(f42, 1), E(f43, 1)]),
	       N(E(f44, 1))
	      ],
      Children = [
		  [N(E(f44, 1))],
		  []
		 ],
      NewEntryPoint1 = C([E(f4, 1), E(f41, 1), E(f42, 1), E(f43, 1)])
  end,
  G1 = cuter_graphs:make_graph_from_children(Nodes, Children),
  [?_assertEqual(equal_graphs(G, G1), true), ?_assertEqual(equal_entry_points(NewEntryPoint, NewEntryPoint1), true)].

cleanup(_) -> ok.

equal_graphs(G1, G2) ->
  G1Keys = dict:fetch_keys(G1),
  G2Keys = dict:fetch_keys(G2),
  case equal_keys(G1Keys, G2Keys) of
    false -> false;
    C -> 
      G2New = change_keys(G2, C),
      equal_graphs_helper(G1, G2New)
  end.

equal_keys(Keys1, Keys2) ->
  case length(Keys1) =:= length(Keys2) of
    true -> equal_keys(Keys1, Keys2, dict:new());
    false -> false
  end.

equal_keys([], _, Acc) -> Acc;
equal_keys([{KeyType, K1}=Key1|Keys1], Keys2, Acc) ->
  case KeyType of
    node ->
      case [K || K <- Keys2, K =:= Key1] of
	[] -> false;
	[_] -> equal_keys(Keys1, Keys2, Acc)
      end;
    cycle ->
      case [K2 || {cycle, K2} <- Keys2, cuter_types:are_sets_equal(sets:from_list(K1), sets:from_list(K2))] of
	[] -> false;
	[K2] ->
	  equal_keys(Keys1, Keys2, dict:store(K2, K1, Acc))
      end
  end.

change_keys(G, C) ->
  H = change_keys_helper(dict:to_list(G), C, []),
  dict:from_list(H).
  
change_keys_helper([], _, Acc) -> lists:reverse(Acc);
change_keys_helper([{{node, _}, _N}=K|Ks], C, Acc)->
  change_keys_helper(Ks, C, [K|Acc]);
change_keys_helper([{{cycle, Cycle}, N}|Ks], C, Acc) ->
  change_keys_helper(Ks, C, [{{cycle, dict:fetch(Cycle, C)}, N}|Acc]).

equal_graphs_helper(G1, G2) ->
  F = fun(Node, N1, Acc) ->
	  N2 = dict:fetch(Node, G2),
	  case equal_keys(N1, N2) of
	    false -> Acc;
	    _ -> true
	  end
      end,
  dict:fold(F, false, G1).

equal_entry_points({T, E1}, {T, E2}) ->
  case T of
    node -> E1 =:= E2;
    cycle -> cuter_types:are_sets_equal(sets:from_list(E1), sets:from_list(E2))
  end;
equal_entry_points(_, _) -> false.

  
