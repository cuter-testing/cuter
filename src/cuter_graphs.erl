-module(cuter_graphs).
-export([children/2, list_contains/2, calculate_dag_callgraph/1]).
-export_type([graph/0, graph_node/0]).
%debugging
-export([print_graph/1, report_callgraphs/1]).


% ================
% types for graphs
% ================

-type graph() :: dict:dict().
-type graph_node() :: mfa() 
		    | {node, mfa()} 
		    | {cycle, [mfa()]}.


% =========================
% graph implementation
% =========================

new_graph() ->
  dict:new().

add_node(Node, Graph) ->
  dict:store(Node, [], Graph).

add_edge({Node1, Node2}, Graph) ->
  dict:store(Node1, [Node2|element(2, dict:find(Node1, Graph))], Graph).

add_node_with_children(Node, Children, Graph) ->
  NewGraph = add_node(Node, Graph),
  lists:foldl(fun(A, B) -> add_edge({Node, A}, B) end, NewGraph, Children).

make_graph_from_children(Nodes, Children) ->
  G = lists:foldl(fun add_node/2, new_graph(), Nodes),
  lists:foldl(fun({Node, Ch}, B) -> dict:store(Node, Ch, B) end, G, lists:zip(Nodes, Children)).

-spec children(graph_node(), graph()) -> [graph_node()].
children(Node, Graph) ->
  {ok, C} = dict:find(Node, Graph),
  C.

get_nodes(Graph) ->
  dict:fetch_keys(Graph).

has_node(Node, Graph) ->
  dict:is_key(Node, Graph).


% ===========
% find cycles
% ===========

-spec cycle_nodes(graph_node(), graph()) -> [[graph_node()]].
cycle_nodes(EntryPoint, Graph) ->
  {Cycled, _, _} = cycle_nodes(EntryPoint, Graph, sets:new(), sets:new()),
  Cycled.

cycle_nodes(Node, Graph, Visited, Ignored) ->
  C = children(Node, Graph),
  TC = lists:filter(
	 fun(Y) -> not (sets:is_element(Y, Visited) or sets:is_element(Node, Ignored)) end,
	 C
	),
  {ChildrenCycled, ChildrenActiveCycled, VisitedBelow} = cycle_nodes_children(TC, Graph, sets:add_element(Node, Visited), Ignored),
  ActiveCycled = lists:filter(fun(X) -> sets:is_element(X, Visited) end, C),
  {Cycles, ActiveCycles} = update_active_cycles(Node, ActiveCycled, ChildrenCycled, ChildrenActiveCycled),
  {Cycles, ActiveCycles, sets:add_element(Node, VisitedBelow)}.

cycle_nodes_children(C, G, V, I) ->
  cycle_nodes_children(C, G, V, I, [], [], sets:new()).

cycle_nodes_children([], _, _, _, CycleAcc, ActiveCycleAcc, VisitedAcc) ->
  {CycleAcc, ActiveCycleAcc, VisitedAcc};
cycle_nodes_children([Ch|C], G, V, I, CycleAcc, ActiveCycleAcc, VisitedAcc) ->
  {Cycle, ActiveCycle, VisitedBelow} = cycle_nodes(Ch, G, V, I),
  cycle_nodes_children(C, G, V, sets:union([I, VisitedBelow]), lists:append([CycleAcc, Cycle]), lists:append([ActiveCycleAcc,  ActiveCycle]), sets:union([VisitedAcc, VisitedBelow])).

-spec update_active_cycles(graph_node(), [{graph_node(), [graph_node()]}], [[graph_node()]], [{graph_node(), [graph_node()]}]) -> {[[graph_node()]], [{graph_node(), [graph_node()]}]}.
update_active_cycles(Node, ActiveCycled, ChildrenCycled, ChildrenActiveCycled) ->
  ActiveCycled1 = create_new_cycles(ActiveCycled, ChildrenActiveCycled),
  {Cycles1, ActiveCycled2} = update_all_cycles(Node, ActiveCycled1),
  {lists:append([Cycles1, ChildrenCycled]), ActiveCycled2}.

-spec create_new_cycles([graph_node()], [{graph_node(), [graph_node()]}]) -> [{graph_node(), [graph_node()]}]. 
create_new_cycles([], Acc) ->
  Acc;
create_new_cycles([H|T], Acc) ->
  [{H,[]}|create_new_cycles(T, Acc)].

-spec update_all_cycles(graph_node(), [{graph_node(), [graph_node()]}]) -> {[[graph_node()]], [{graph_node(), [graph_node()]}]}.
update_all_cycles(Node, ActiveCycled) ->
  update_all_cycles(Node, ActiveCycled, [], []).

-spec update_all_cycles(graph_node(), [{graph_node(), [graph_node()]}], [{graph_node(), [graph_node()]}], [[graph_node()]]) -> {[[graph_node()]], [{graph_node(), [graph_node()]}]}.
update_all_cycles(_, [], ActiveAcc, CyclesAcc) ->
  {CyclesAcc, ActiveAcc};
update_all_cycles(Node, [{First, List}|T], ActiveAcc, CyclesAcc) ->
  case First of
    Node ->
      CyclesAcc1 = [[Node|List]|CyclesAcc],
      ActiveAcc1 = ActiveAcc;
    _ ->
      CyclesAcc1 = CyclesAcc,
      ActiveAcc1 = [{First, [Node|List]}|ActiveAcc]
  end,
  update_all_cycles(Node, T, ActiveAcc1, CyclesAcc1).


% =========================
% merge overlapping cycles
% =========================

merge_cycles(Cycles) ->
  G = make_help_graph(Cycles),
  connected_components(G).

-spec make_help_graph([[graph_node()]]) -> dict:dict().
make_help_graph(Cycles) ->
  G = dict:new(),
  lists:foldl(fun put_cycle/2, G, Cycles).

-spec put_cycle([graph_node()], dict:dict()) -> dict:dict().
put_cycle(Cycle, Graph) ->
  put_cycle(nonode, Cycle, Graph).

put_cycle(_, [], Graph) ->
  Graph;
put_cycle(Prev, [N|Ns], Graph) ->
  Graph1 = case dict:is_key(N, Graph) of
	     true ->
	       Graph;
	     false ->
	       dict:store(N, [], Graph)
	   end,
  Graph2 = case Prev of
	     nonode ->
	       Graph1;
	     _ ->
	       G = dict:append_list(Prev, [N], Graph1),
	       dict:append_list(N, [Prev], G)
	   end,
  put_cycle(N, Ns, Graph2).

-spec connected_components(dict:dict()) -> [sets:set()].
connected_components(G) ->
  connected_components(G, []).

-spec connected_components(dict:dict(), [sets:set()]) -> [sets:set()].
connected_components(G, Acc) ->
  case dict:is_empty(G) of
    true ->
      Acc;
    false ->
      C = connected_component(G),
      G1 = remove_keys(C, G),
      connected_components(G1, [C|Acc])
  end.

-spec connected_component(dict:dict()) -> sets:set().
connected_component(G) ->
  connected_component(hd(dict:fetch_keys(G)), sets:new(), G).

-spec connected_component(graph_node(), sets:set(), dict:dict()) -> sets:set().
connected_component(Node, Visited, Graph) ->
  {ok, Children} = dict:find(Node, Graph),
  Visited1 = sets:add_element(Node, Visited),
  connected_component_children(Children, Visited1, Graph).

-spec connected_component_children([graph_node()], sets:set(), dict:dict()) -> sets:set().
connected_component_children([], Visited, _) -> Visited;
connected_component_children([C|Cs], Visited, Graph) ->
  case sets:is_element(C, Visited) of
    false ->
      Visited1 = connected_component(C, Visited, Graph);
    true ->
      Visited1 = Visited
  end,
  connected_component_children(Cs, Visited1, Graph).

-spec remove_keys(sets:set(), dict:dict()) -> dict:dict().
remove_keys(C, G) ->
  lists:foldl(fun dict:erase/2, G, sets:to_list(C)).


% ===================================
% make new graph merging cycled nodes
% ===================================

remake_graph(EntryPoint, Graph) ->
  Cycles = merge_cycles(cycle_nodes(EntryPoint, Graph)),
  CycleNodes = [{cycle, sets:to_list(X)} || X <- Cycles],
  AllCycledNodes = sets:union(Cycles),
  Children = find_children([A || {cycle, A} <- CycleNodes], Graph),
  NewNodes = [{node, X} || X <- get_nodes(Graph), not sets:is_element(X, AllCycledNodes)],
  NewChildren = [update_children(children(Y, Graph), AllCycledNodes, Cycles, CycleNodes) || {node, Y} <- NewNodes],
  CycleChildren = [update_children(Z, AllCycledNodes, Cycles, CycleNodes) || Z <- Children],
  Nodes = lists:append(NewNodes, CycleNodes),
  ChildrenPerNodeTemp = [sets:to_list(sets:from_list(W)) || W <- lists:append(NewChildren, CycleChildren)],
  ChildrenPerNode = [try_remove(B, C) || {B, C} <- lists:zip(Nodes, ChildrenPerNodeTemp)],
  make_graph_from_children(Nodes, ChildrenPerNode).  

-spec find_children([sets:set()], graph()) -> [[graph_node()]].
find_children(Cycles, Graph) ->
  find_children(Cycles, Graph, []).

-spec find_children([sets:set()], graph(), [[graph_node()]]) -> [[graph_node()]].
find_children([], _, Acc) -> lists:reverse(Acc);
find_children([C|Cs], Graph, Acc) ->
  find_children(Cs, Graph, [lists:append([children(X, Graph) || X <- C])|Acc]).

-spec update_children([[graph_node()]], sets:set(), [sets:set()], [{atom(), [graph_node()]}]) -> [{atom(), [graph_node()]}].
update_children(Children, AllCycledNodes, Cycles, CyclesAsLists) ->
  update_children(Children, AllCycledNodes, Cycles, CyclesAsLists, []).

-spec update_children([[graph_node()]], sets:set(), [sets:set()], [{atom(), [graph_node()]}], [{atom(), [graph_node()]}]) -> [{atom(), [graph_node()] | graph_node()}].
update_children([], _, _, _, Acc) -> Acc;
update_children([C|Cs], AllCycles, Cycles, CyclesAsLists, Acc) -> 
  case sets:is_element(C, AllCycles) of
    true ->
      update_children(Cs, AllCycles, Cycles, CyclesAsLists, [which_cycle(C, Cycles, CyclesAsLists)|Acc]);
    false ->
      update_children(Cs, AllCycles, Cycles, CyclesAsLists, [{node, C}|Acc])
  end.

which_cycle(_, [], _) -> error('cycle not found');
which_cycle(Node, [C|Cs], [CL|CLs]) ->
  case sets:is_element(Node, C) of
    true ->
      CL;
    false ->
      which_cycle(Node, Cs, CLs)
  end.

try_remove(Node, Children) ->
  try_remove(Node, Children, []).

try_remove(_, [], Acc) -> Acc;
try_remove(Node, [C|Cs], Acc) -> 
  case Node == C of
    true ->
      try_remove(Node, Cs, Acc);
    false ->
      try_remove(Node, Cs, [C|Acc])
  end.


% ===================
% Calculate callgraph
% ===================

-spec calculate_dag_callgraph(mfa()) -> {graph(), sets:set(), graph_node()}.
calculate_dag_callgraph(EntryPoint) ->
  Original = calculate_callgraph(EntryPoint),
  CallGraph = remake_graph(EntryPoint, Original),
  NewEntryPoint = find_node(EntryPoint, CallGraph),
  {CallGraph, sets:from_list(dict:fetch_keys(Original)), NewEntryPoint}.

find_node(EntryPoint, Graph) ->
  case has_node({node, EntryPoint}, Graph) of
    true ->
      {node, EntryPoint};
    false ->
      {cycle, hd([C || {cycle, C} <- get_nodes(Graph), list_contains(EntryPoint, C)])}
  end.

-spec calculate_callgraph(mfa()) -> graph().
calculate_callgraph(EntryPoint) ->
  xref:start(s),
  _FoundModules = add_modules_rec(EntryPoint),
  CallGraph = make_callgraph(EntryPoint, new_graph()),
  xref:stop(s),
  CallGraph.

add_modules_rec(MFA) ->
  add_modules_rec(MFA, sets:new(), sets:new()).

add_modules_rec({M, F, A}, Found, FoundNodes) ->
  Valid = fun({_, {M1, F1, _A}}) -> hd(atom_to_list(M1)) =/= 36 andalso hd(atom_to_list(F1)) =/= 36 end,
  NewFound = case sets:is_element(M, Found) of
	       false ->
		 xref:add_module(s, code:which(M)),
		 sets:add_element(M, Found);
	       true ->
		 Found
	     end,
  {ok, Edges1} = xref:q(s, lists:concat(["E | ", mfa_to_str({M, F, A})])),
  Edges = lists:filter(Valid, Edges1),
  NewFoundNodes = sets:add_element({M, F, A}, FoundNodes),
  lists:foldl(fun(X, Y) -> add_modules_rec(X, Y, NewFoundNodes) end, NewFound, [B || {_A, B} <- Edges, not sets:is_element(B, FoundNodes)]).

-spec make_callgraph(mfa(), graph()) -> graph().
make_callgraph(MFA, Graph) ->
  case has_node(MFA, Graph) of
    true -> Graph;
    false ->
      Valid = fun({_, {M1, F1, _A}}) -> hd(atom_to_list(M1)) =/= 36 andalso hd(atom_to_list(F1)) =/= 36 end,
      {ok, ChildEdges1} = xref:q(s, lists:concat(["E | ", mfa_to_str(MFA)])),
      ChildEdges = lists:filter(Valid, ChildEdges1),
      Children = [B || {_A,B} <- ChildEdges],
      NewGraph = add_node_with_children(MFA, Children, Graph),
      lists:foldl(fun make_callgraph/2, NewGraph, Children)
  end.

-spec list_contains(any(), [any()]) -> boolean().
list_contains(_, []) -> false;
list_contains(X, [H|_T]) when X == H -> true;
list_contains(X, [_H|T]) -> list_contains(X, T). 

-spec mfa_to_str(mfa()) -> string().
mfa_to_str({M, F, A}) ->
  lists:concat([atom_to_list(M), ":", atom_to_list(F), "/", lists:flatten(io_lib:format("~p", [A]))]).

		       
% =============
% for debugging
% =============

-spec print_graph(graph()) -> ok.
print_graph(Graph) ->
  lists:foreach(fun(A) -> io:format("~p: ~p;~n", [A, children(A, Graph)]) end, get_nodes(Graph)).

-spec report_callgraphs(mfa()) -> ok.
report_callgraphs(EntryPoint) ->
  Original = calculate_callgraph(EntryPoint),
  io:format("Original callgraph:~n"),
  graphs:print_graph(Original),
  CallGraph = remake_graph(EntryPoint, Original),
  NewEntryPoint = find_node(EntryPoint, CallGraph),
  io:format("Final callgraph:~n"),
  graphs:print_graph(CallGraph),
  io:format("New Entry Point: ~p ~n", [NewEntryPoint]).
