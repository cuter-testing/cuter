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
		    | {scc, [mfa()]}.


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
%% Logic for finding sccs. Implemented as a DFS search
%% with a visited set.
%% =====================================================

%% Returns a list of sccs in a graph.
scc_nodes(EntryPoint, Graph) ->
  {Sccd, _, _} = scc_nodes(EntryPoint, Graph, sets:new(), sets:new()),
  Sccd.

scc_nodes(Node, Graph, Visited, Ignored) ->
  %% Get the children of the node.
  C = children(Node, Graph),
  %% Filter out the ones that have been visited or are ignored.
  TC = [Y || Y <- C, not (sets:is_element(Y, Visited) or sets:is_element(Node, Ignored))],
  %% Call self for every child.
  {ChildrenSccd, ChildrenActiveSccd, VisitedBelow} = scc_nodes_children(TC, Graph, sets:add_element(Node, Visited), Ignored),
  %% An active scc is a detected scc that hasn't been
  %% completed yet when backtracking
  ActiveSccd = lists:filter(fun(X) -> sets:is_element(X, Visited) end, C),
  {Sccs, ActiveSccs} = update_active_sccs(Node, ActiveSccd, ChildrenSccd, ChildrenActiveSccd),
  {Sccs, ActiveSccs, sets:add_element(Node, VisitedBelow)}.

scc_nodes_children(C, G, V, I) ->
  scc_nodes_children(C, G, V, I, [], [], sets:new()).

scc_nodes_children([], _, _, _, SccAcc, ActiveSccAcc, VisitedAcc) ->
  {SccAcc, ActiveSccAcc, VisitedAcc};
scc_nodes_children([Ch|C], G, V, I, SccAcc, ActiveSccAcc, VisitedAcc) ->
  {Scc, ActiveScc, VisitedBelow} = scc_nodes(Ch, G, V, I),
  scc_nodes_children(C, G, V, sets:union([I, VisitedBelow]), lists:append([SccAcc, Scc]), lists:append([ActiveSccAcc,  ActiveScc]), sets:union([VisitedAcc, VisitedBelow])).

update_active_sccs(Node, ActiveSccd, ChildrenSccd, ChildrenActiveSccd) ->
  ActiveSccd1 = create_new_sccs(ActiveSccd, ChildrenActiveSccd),
  {Sccs1, ActiveSccd2} = update_all_sccs(Node, ActiveSccd1),
  {lists:append([Sccs1, ChildrenSccd]), ActiveSccd2}.

create_new_sccs([], Acc) ->
  Acc;
create_new_sccs([H|T], Acc) ->
  [{H,[]}|create_new_sccs(T, Acc)].

update_all_sccs(Node, ActiveSccd) ->
  update_all_sccs(Node, ActiveSccd, [], []).

update_all_sccs(_, [], ActiveAcc, SccsAcc) ->
  {SccsAcc, ActiveAcc};
update_all_sccs(Node, [{First, List}|T], ActiveAcc, SccsAcc) ->
  case First of
    Node ->
      SccsAcc1 = [[Node|List]|SccsAcc],
      ActiveAcc1 = ActiveAcc;
    _ ->
      SccsAcc1 = SccsAcc,
      ActiveAcc1 = [{First, [Node|List]}|ActiveAcc]
  end,
  update_all_sccs(Node, T, ActiveAcc1, SccsAcc1).


%% =================================================================
%% Logic for merging overlapping sccs.
%% Overlapping sccs are sccs that have at least one common node.
%% Each scc is a list of nodes, so we have to find all lists with
%% common elements and merge them.
%% =================================================================

%% Sccs are lists of nodes.
%% If two sccs contain at least one commone element are merged into one list.
merge_sccs(Sccs) ->
  %% Make a helper graph.
  G = make_help_graph(Sccs),
  %% Merged sccs are the connected components of the helper graph.
  connected_components(G).

%% Creates the helper graph for merging the sccs.
-spec make_help_graph([[graph_node()]]) -> dict:dict().
make_help_graph(Sccs) ->
  %% Initialize a graph.
  G = dict:new(),
  %% Add each scc to the graph.
  lists:foldl(fun put_scc/2, G, Sccs).

%% Adds a scc to a helper graph.
-spec put_scc([graph_node()], dict:dict()) -> dict:dict().
put_scc(Scc, Graph) -> put_scc(nonode, Scc, Graph).

%% If we don't have other nodes to add, return the graph.
put_scc(_, [], Graph) -> Graph;
put_scc(Prev, [N|Ns], Graph) ->
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
  put_scc(N, Ns, Graph2).

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
%% Logic for making a new graph merging sccd nodes.
%% ==================================================

remake_graph(EntryPoint, Graph) ->
  Sccs = merge_sccs(scc_nodes(EntryPoint, Graph)),
  SccNodes = [{scc, sets:to_list(X)} || X <- Sccs],
  AllSccdNodes = sets:union(Sccs),
  Children = find_children([A || {scc, A} <- SccNodes], Graph),
  NewNodes = [{node, X} || X <- get_nodes(Graph), not sets:is_element(X, AllSccdNodes)],
  NewChildren = [update_children(children(Y, Graph), AllSccdNodes, Sccs, SccNodes) || {node, Y} <- NewNodes],
  SccChildren = [update_children(Z, AllSccdNodes, Sccs, SccNodes) || Z <- Children],
  Nodes = lists:append(NewNodes, SccNodes),
  ChildrenPerNodeTemp = [sets:to_list(sets:from_list(W)) || W <- lists:append(NewChildren, SccChildren)],
  ChildrenPerNode = [try_remove(B, C) || {B, C} <- lists:zip(Nodes, ChildrenPerNodeTemp)],
  make_graph_from_children(Nodes, ChildrenPerNode).  

find_children(Sccs, Graph) ->
  find_children(Sccs, Graph, []).

find_children([], _, Acc) -> lists:reverse(Acc);
find_children([C|Cs], Graph, Acc) ->
  find_children(Cs, Graph, [lists:append([children(X, Graph) || X <- C])|Acc]).

update_children(Children, AllSccdNodes, Sccs, SccsAsLists) ->
  update_children(Children, AllSccdNodes, Sccs, SccsAsLists, []).

update_children([], _, _, _, Acc) -> Acc;
update_children([C|Cs], AllSccs, Sccs, SccsAsLists, Acc) -> 
  case sets:is_element(C, AllSccs) of
    true ->
      update_children(Cs, AllSccs, Sccs, SccsAsLists, [which_scc(C, Sccs, SccsAsLists)|Acc]);
    false ->
      update_children(Cs, AllSccs, Sccs, SccsAsLists, [{node, C}|Acc])
  end.

which_scc(_, [], _) -> error('scc not found');
which_scc(Node, [C|Cs], [CL|CLs]) ->
  case sets:is_element(Node, C) of
    true ->
      CL;
    false ->
      which_scc(Node, Cs, CLs)
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
%% Then it replaces all sccs with single nodes,
%% merging sccs with common functions.
%% Last it finds the new entry point which may be part of a scc.
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
      %% Else return the first scc that contains the entry point.
      %% Only one scc will contain it, since if it belonge to many,
      %% they would have been merged.
      {scc, hd([C || {scc, C} <- get_nodes(Graph), list_contains(EntryPoint, C)])}
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
