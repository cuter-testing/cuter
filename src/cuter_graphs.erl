-module(cuter_graphs).
-export([make_graph_from_children/2, children/2, list_contains/2, calculate_dag_callgraph/1]).
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


%% =====================================================
%% Graph implementation. Each graph is a dictionary with
%%  nodes as keys and their list of neighbours as values
%% =====================================================

%% Creates an empty dict representing a graph.
new_graph() ->
  dict:new().

%% Adds a new node with no neighbours to a graph.
add_node(Node, Graph) ->
  dict:store(Node, [], Graph).

%% Adds an edge to a graph.
add_edge({Node1, Node2}, Graph) ->
  %% Add Node2 into the list of neighbours of Node1
  dict:store(Node1, [Node2|element(2, dict:find(Node1, Graph))], Graph).

%% Adds a node along with its neighbours in a graph.
add_node_with_children(Node, Children, Graph) ->
  NewGraph = add_node(Node, Graph),
  lists:foldl(fun(A, B) -> add_edge({Node, A}, B) end, NewGraph, Children).

%% Given a list of nodes and a list of neighbours per node, it returns a graph.
-spec make_graph_from_children([node()], [[node()]]) -> graph().
make_graph_from_children(Nodes, Children) ->
  G = lists:foldl(fun add_node/2, new_graph(), Nodes),
  lists:foldl(fun({Node, Ch}, B) -> dict:store(Node, Ch, B) end, G, lists:zip(Nodes, Children)).

%% Returns the neighbour list of a node in a graph.
-spec children(graph_node(), graph()) -> [graph_node()].
children(Node, Graph) ->
  {ok, C} = dict:find(Node, Graph),
  C.

%% Returns all the nodes of a graph.
get_nodes(Graph) ->
  dict:fetch_keys(Graph).

%% Checks if a node is contained in a graph.
has_node(Node, Graph) ->
  dict:is_key(Node, Graph).


%% =====================================================
%% Logic for finding cycles. Implemented as a DFS search
%% with a visited set.
%% =====================================================

%% Returns a list of cycles in a graph.
cycle_nodes(EntryPoint, Graph) ->
  {Cycled, _, _} = cycle_nodes(EntryPoint, Graph, sets:new(), sets:new()),
  Cycled.

cycle_nodes(Node, Graph, Visited, Ignored) ->
  %% Get the children of the node.
  C = children(Node, Graph),
  %% Filter out the ones that have been visited or are ignored.
  TC = [Y || Y <- C, not (sets:is_element(Y, Visited) or sets:is_element(Node, Ignored))],
  %% Call self for every child.
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

update_active_cycles(Node, ActiveCycled, ChildrenCycled, ChildrenActiveCycled) ->
  ActiveCycled1 = create_new_cycles(ActiveCycled, ChildrenActiveCycled),
  {Cycles1, ActiveCycled2} = update_all_cycles(Node, ActiveCycled1),
  {lists:append([Cycles1, ChildrenCycled]), ActiveCycled2}.

create_new_cycles([], Acc) ->
  Acc;
create_new_cycles([H|T], Acc) ->
  [{H,[]}|create_new_cycles(T, Acc)].

update_all_cycles(Node, ActiveCycled) ->
  update_all_cycles(Node, ActiveCycled, [], []).

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


%% =================================================================
%% Logic for merging overlapping cycles.
%% Overlapping cycles are cycles that have at least one common node.
%% Each cycle is a list of nodes, so we have to find all lists with
%% common elements and merge them.
%% =================================================================

%% Cycles are lists of nodes.
%% If two cycles contain at least one commone element are merged into one list.
merge_cycles(Cycles) ->
  %% Make a helper graph.
  G = make_help_graph(Cycles),
  %% Merged cycles are the connected components of the helper graph.
  connected_components(G).

%% Creates the helper graph for merging the cycles.
-spec make_help_graph([[graph_node()]]) -> dict:dict().
make_help_graph(Cycles) ->
  %% Initialize a graph.
  G = dict:new(),
  %% Add each cycle to the graph.
  lists:foldl(fun put_cycle/2, G, Cycles).

%% Adds a cycle to a helper graph.
-spec put_cycle([graph_node()], dict:dict()) -> dict:dict().
put_cycle(Cycle, Graph) -> put_cycle(nonode, Cycle, Graph).

%% If we don't have other nodes to add, return the graph.
put_cycle(_, [], Graph) -> Graph;
put_cycle(Prev, [N|Ns], Graph) ->
  %% If the node is not already in the graph, add it.
  Graph1 = case dict:is_key(N, Graph) of
	     true ->
	       Graph;
	     false ->
	       dict:store(N, [], Graph)
	   end,
  %% Connect the previous node with the current one bidirectionally.
  Graph2 = case Prev of
	     nonode ->
	       Graph1;
	     _ ->
	       G = dict:append_list(Prev, [N], Graph1),
	       dict:append_list(N, [Prev], G)
	   end,
  put_cycle(N, Ns, Graph2).

%% Returns the connected components of a graph.
connected_components(G) ->
  connected_components(G, []).

connected_components(G, Acc) ->
  %% If the graph is empty, we have found all connected components.
  case dict:is_empty(G) of
    true ->
      Acc;
    false ->
      %% Else, get one connected component.
      C = connected_component(G),
      %% Remove the whole component from G.
      G1 = remove_nodes(C, G),
      %% Find the rest connected components.
      connected_components(G1, [C|Acc])
  end.

connected_component(G) ->
  %% To find one connected component, fetch a random node from the graph
  %% and find everything that can be reached from that node.
  connected_component(hd(dict:fetch_keys(G)), sets:new(), G).

%% Finds the connected component of Graph
%% which contains the node Node given that we have
%% visited Visited nodes of this component already
connected_component(Node, Visited, Graph) ->
  %% Get the neighbours of this Node.
  {ok, Children} = dict:find(Node, Graph),
  %% Add them to the visited set.
  Visited1 = sets:add_element(Node, Visited),
  %% Call self for each child through connected_component_children/3.
  connected_component_children(Children, Visited1, Graph).

%% Calls connected_component/3 for each of the nodes
%% in a list if they haven't alreadybeen visited.
%% Returns all the nodes that have been visited by
%% this procedure.
connected_component_children([], Visited, _) -> Visited;
connected_component_children([C|Cs], Visited, Graph) ->
  %% If node C has not been visited.
  case sets:is_element(C, Visited) of
    false ->
      %% Find the connected component containing this node.
      Visited1 = connected_component(C, Visited, Graph);
    true ->
      Visited1 = Visited
  end,
  connected_component_children(Cs, Visited1, Graph).

%% Removes all nodes in list C from graph G.
remove_nodes(C, G) ->
  lists:foldl(fun dict:erase/2, G, sets:to_list(C)).


%% ==================================================
%% Logic for making a new graph merging cycled nodes.
%% ==================================================

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

find_children(Cycles, Graph) ->
  find_children(Cycles, Graph, []).

find_children([], _, Acc) -> lists:reverse(Acc);
find_children([C|Cs], Graph, Acc) ->
  find_children(Cs, Graph, [lists:append([children(X, Graph) || X <- C])|Acc]).

update_children(Children, AllCycledNodes, Cycles, CyclesAsLists) ->
  update_children(Children, AllCycledNodes, Cycles, CyclesAsLists, []).

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


%% ===========================================================
%% Logic for calculating a callgraph and merging all functions 
%% having a cyclic dependency onto a new node.
%% ===========================================================

%% First calculates the callgraph.
%% Then it replaces all cycles with single nodes,
%% merging cycles with common functions.
%% Last it finds the new entry point which may be part of a cycle.
%% Returns the processed callgraph, all functions belonging to the callgraph in a set
%% and the new entry point.
-spec calculate_dag_callgraph(mfa()) -> {graph(), sets:set(), graph_node()}.
calculate_dag_callgraph(EntryPoint) ->
  Original = calculate_callgraph(EntryPoint),
  CallGraph = remake_graph(EntryPoint, Original),
  NewEntryPoint = find_entry_point(EntryPoint, CallGraph),
  {CallGraph, sets:from_list(dict:fetch_keys(Original)), NewEntryPoint}.

%% Finds the new entry point of the graph
find_entry_point(EntryPoint, Graph) ->
  case has_node({node, EntryPoint}, Graph) of
    true ->
      %% If entry point is just a node, return it.
      {node, EntryPoint};
    false ->
      %% Else return the first cycle that contains the entry point.
      %% Only one cycle will contain it, since if it belonge to many,
      %% they would have been merged.
      {cycle, hd([C || {cycle, C} <- get_nodes(Graph), list_contains(EntryPoint, C)])}
  end.

%% Calculates the callgraph produced by the
%% visibly reachable mfas from an entry point.
calculate_callgraph(EntryPoint) ->
  %% Start the xref server.
  xref:start(s),
  %% Add all modules reachable by the entry point
  %% in the xref server.
  _FoundModules = add_modules_rec(EntryPoint),
  %% Make the callgraph from the gathered modules.
  CallGraph = make_callgraph(EntryPoint, new_graph()),
  %% Stop the xref server.
  xref:stop(s),
  CallGraph.

%% Adds to the xref server the module of the argument mfa.
%% Then it recursively adds the modules of the functions called by the mfa.
add_modules_rec(MFA) ->
  add_modules_rec(MFA, sets:new(), sets:new()).

add_modules_rec({M, _F, _A}=MFA, Found, FoundNodes) ->
  %% Function used to filter edges pointing to compiler generated functions (eg. from list comprehensions).
  Valid = fun({_, {M1, F1, _A1}}) ->
	      %% The compiler generated function name starts with char '$'.
	      hd(atom_to_list(M1)) =/= 36 andalso hd(atom_to_list(F1)) =/= 36 
	  end,
  %% If we haven't seen the mfa's module yet, add it to the xref server and the Found set.
  NewFound = case sets:is_element(M, Found) of
	       false ->
		 xref:add_module(s, code:which(M)),
		 sets:add_element(M, Found);
	       true ->
		 Found
	     end,
  %% Get the outward edges from our mfa to other functions.
  %% To do this, we make a query to the xref server starting with
  %% 'E' to fetch edges and use | <mfa> to specify those that start with our mfa.
  {ok, Edges1} = xref:q(s, lists:concat(["E | ", mfa_to_str(MFA)])),
  %% Filter the edges to compiler generated functions.
  Edges = lists:filter(Valid, Edges1),
  %% Add the current mfa to FoundNodes set.
  NewFoundNodes = sets:add_element(MFA, FoundNodes),
  lists:foldl(fun(X, Y) -> add_modules_rec(X, Y, NewFoundNodes) end, NewFound, [N2 || {_N1, N2} <- Edges, not sets:is_element(N2, FoundNodes)]).

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
  NewEntryPoint = find_entry_point(EntryPoint, CallGraph),
  io:format("Final callgraph:~n"),
  graphs:print_graph(CallGraph),
  io:format("New Entry Point: ~p ~n", [NewEntryPoint]).
