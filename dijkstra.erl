-module(dijkstra).
-compile(export_all).

%% Heap (Open Set) Abstractions
-define(EMPTY_HEAP, gb_trees:empty()).
-define(INSERT_NODE_TO_HEAP(Node, Prev, Dist, OldHeap), gb_trees:insert({Dist, Node, Prev}, 0, OldHeap)).
-define(FILTER_MIN_HEAP(R), {element(2, element(1, R)), element(1, element(1, R)), element(3, R)}).
-define(GET_MIN_HEAP(Heap), ?FILTER_MIN_HEAP(gb_trees:take_smallest(Heap))).
%% Graph Abstractions
-define(EMPTY_GRAPH, digraph:new([cyclic])).
-define(ADD_VERTEX(Graph, Vertex), digraph:add_vertex(Graph, Vertex)).
-define(ADD_EDGE(From, To, Weight, Graph), digraph:add_edge(Graph, {From, To}, From, To, Weight)).
-define(EDGE_WEIGHT(Graph, Edge), element(4, digraph:edge(Graph, Edge))).
-define(GET_NEIGHBOURS(Vertex, Graph), digraph:out_neighbours(Graph, Vertex)).
%% Visited Set Abstractions
-define(EMPTY_VISITED, dict:new()).
-define(IS_VISITED(V, Visited), dict:is_key(V, Visited)).
-define(ADD_TO_VISITED(Node, OldVisited), dict:store(Node, true, OldVisited)).
%% Minimum Distances List Abstractions
-define(EMPTY_DISTANCES, []).
-define(ADD_TO_DISTANCES(Node, Dist, Dists), [{Node, Dist} | Dists]).
-define(SORT_DISTANCES(Dists), lists:reverse(Dists)).

%% ---------------------------------------------------------
%% Initialize Graph
init_graph(Vertices, Edges, File) ->
  Graph = ?EMPTY_GRAPH,
  init_vertices(Vertices, Graph),
  init_edges(Edges, File, Graph),
  Graph.
  
init_vertices(Vertices, Graph) ->
  init_vertices(Vertices, Graph, 0).
  
init_vertices(_Vertices, Graph, _Vertices) ->
  Graph;
init_vertices(Vertices, Graph, CurrVertex) ->
  ?ADD_VERTEX(Graph, CurrVertex),
  init_vertices(Vertices, Graph, CurrVertex+1).

init_edges(0, _File, Graph) ->
  Graph;
init_edges(Edges, File, Graph) ->
  {ok, [V, W, D]} = io:fread(File, ">", "~d ~d ~d"),
  ?ADD_EDGE(V, W, D, Graph),
  ?ADD_EDGE(W, V, D, Graph),
  init_edges(Edges-1, File, Graph).
%% ---------------------------------------------------------

%% ---------------------------------------------------------
%% DIJKSTRA FUNCTIONS
%% ---------------------------------------------------------

%% Initalize Heap (Open Set), Visited (Closed Set), Min Distances List
init_heap(Root) ->
  ?INSERT_NODE_TO_HEAP(Root, nil, 0, ?EMPTY_HEAP).

dijkstra_init(Root) ->
  Heap = init_heap(Root),
  Visited = ?EMPTY_VISITED,
  Dists = ?EMPTY_DISTANCES,
  {Heap, Visited, Dists}.
  
%% Dijkstra loop
%% Modified to stop at DistLimit and only record Appletrees distances
dijkstra_step(Root, Graph, Heap, Visited, Dists, DistLimit) ->
  try
    {Node, Dist, NewHeap} = ?GET_MIN_HEAP(Heap),
    case Dist > DistLimit of
      true ->
        erlang:error(max_dist);
      false ->
        case ?IS_VISITED(Node, Visited) of
          true ->
            dijkstra_step(Root, Graph, NewHeap, Visited, Dists, DistLimit);
          false ->
            NewDists = 
              ?ADD_TO_DISTANCES(Node, Dist, Dists),
            NewVisited = ?ADD_TO_VISITED(Node, Visited),
            AdjList = ?GET_NEIGHBOURS(Node, Graph),
            NextHeap = lists:foldl(
              fun(V, H) ->
                case ?IS_VISITED(V, NewVisited) of
                  true ->
                    H;
                  false ->
                    Edge = {Node, V},
                    Weight = ?EDGE_WEIGHT(Graph, Edge),
                    ?INSERT_NODE_TO_HEAP(V, Node, Dist + Weight, H)
                end
              end,
              NewHeap,
              AdjList
            ),
          dijkstra_step(Root, Graph, NextHeap, NewVisited, NewDists, DistLimit)
        end
    end
  catch
    error:_E ->
      ?SORT_DISTANCES(Dists)
  end.

%% Wrapper function
%% Result is [{AppleTree, Distance}]
dijkstra(Graph, Root, DistLimit) ->
  {Heap, Visited, Dists} = dijkstra_init(Root),
  dijkstra_step(Root, Graph, Heap, Visited, Dists, DistLimit).
%% ---------------------------------------------------------


%% ---------------------------------------------------------
%% Perform N times the Dijkstra algorith (sequentially)
%% Result is [{Root, MinDistList}]
%%    Root :: 0...N-1
%%    MinDistList :: [{AppleTree, Distance}]
dijkstra_all_seq(Graph, DistLimit, Appletrees) ->
  dijkstra_all_seq(Graph, DistLimit, Appletrees, []).
  
dijkstra_all_seq(_Graph, _DistLimit, [], Acc) ->
  Acc;
dijkstra_all_seq(Graph, DistLimit, [CurrTree | Appletrees], Acc) ->
  MinDs = dijkstra(Graph, CurrTree, DistLimit),
  dijkstra_all_seq(Graph, DistLimit, Appletrees, [{CurrTree, MinDs} | Acc]).
%% ---------------------------------------------------------

%% ---------------------------------------------------------
%% Perform N times the Dijkstra algorith (concurrently)
%% Result is [{Root, MinDistList}]
%%    Root :: 0...N-1
%%    MinDistList :: [{Root, AppleTree, Distance}]
dijkstra_all_conc(Graph, DistLimit, Appletrees) ->
  process_flag(trap_exit, true),
  WorkerArgs = fun(Root) ->
    [Graph, Root, DistLimit]
  end,
  spawn_dijkstra_workers(Appletrees, WorkerArgs),
  Dists = dijkstra_super({length(Appletrees), []}),
  process_flag(trap_exit, false),
  Dists.

%% Supervisor code
dijkstra_super({0, Acc}) ->
  Acc;
dijkstra_super({Nodes, Acc}) ->
  receive
    {'EXIT', _Worker, {Root, MinDs}} ->
      %% Same optimization as in sequential
      case MinDs of
        [] -> dijkstra_super({Nodes-1, Acc});
        _  -> dijkstra_super({Nodes-1, [{Root, MinDs} | Acc]})
      end
  end.
  
%% Worker Code
dijkstra_worker(Graph, Root, DistLimit) ->
  MinDs = dijkstra(Graph, Root, DistLimit),
  exit({Root, MinDs}).

%% Spawn all workers  
spawn_dijkstra_workers([], _Args) ->
  ok;
spawn_dijkstra_workers([N | Ns], Args) ->
  spawn_link(?MODULE, dijkstra_worker, Args(N)),
  spawn_dijkstra_workers(Ns, Args).
%% ---------------------------------------------------------

%% Dijkstra Mode Selector (sequential or concurrent)
%%   Mode :: seq | conc
dijkstra_all('seq', Graph, DistLimit, Appletrees) ->
  dijkstra_all_seq(Graph, DistLimit, Appletrees);
dijkstra_all('conc', Graph, DistLimit, Appletrees) ->
  dijkstra_all_conc(Graph, DistLimit, Appletrees).
  

%% ---------------------------------------------------------
%% Initialize appletrees and drills lists
init_appletrees(Trees, File) ->
  init_appletrees(Trees, File, 0, []).

init_appletrees(_Trees, _File, _Trees, Acc) ->
  Acc;
init_appletrees(Trees, File, CurrTree, Acc) ->
  {ok, [TreePos]} = io:fread(File, ">", "~d"),
  init_appletrees(Trees, File, CurrTree+1, [TreePos | Acc]).
  
init_drills(Drills, File) ->
  init_drills(Drills, File, 0, []).
  
init_drills(_Drills, _File, _Drills, Acc) ->
  Acc;
init_drills(Drills, File, CurrDrill, Acc) ->
  {ok, [DrillPos]} = io:fread(File, ">", "~d"),
  init_drills(Drills, File, CurrDrill+1, [DrillPos | Acc]).
%% ---------------------------------------------------------


run(F) ->
  %% Perform Initializations
  File = filename:absname(F),
  {ok, IOFile} = file:open(File, [read]),
  {ok, [N, M, P, B]} = io:fread(IOFile, ">", "~d ~d ~d ~d"),
  %% Trivial case when P <= B
  case P =< B of
    true ->
      P;
    false ->
      Graph = init_graph(N, M, IOFile),
      Appletrees = init_appletrees(P, IOFile),
      Drills = init_drills(B, IOFile),
      MaxDrill = lists:max(Drills),
      %% Execute Dijkstra algorithm (N times)
      Start = now(),
      DistsS = dijkstra_all('seq', Graph, MaxDrill, Appletrees),
      Point1 = now(),
      DistsC = dijkstra_all('conc', Graph, MaxDrill, Appletrees),
      End = now(),
      %% Wrapping up
      file:close(IOFile),
      digraph:delete(Graph),
      %% Measure Execution Time
      io:format("Sequential Dijkstra: ~w secs~n", [timer:now_diff(Point1, Start)/1000000]),
      io:format("Concurrent Dijkstra: ~w secs~n", [timer:now_diff(End, Point1)/1000000]),
      {DistsS, DistsC}
  end.
