%% -*- erlang-indent-level: 2 -*-
-module(cuter_bfs_strategy).
-behaviour(cuter_strategy_behaviour).

-export([locate_next_reversible/2, init_data/0, destroy_data/1, no_paths/1, handle_execution/6]).

-type visited() :: boolean().
-type item() :: {visited(), operationId(), cuter_cerl:tagID(), handle()}.
-type handle() :: nonempty_string().
-type operationId() :: integer().

-spec init_data() -> cuter_minheap:minheap().
init_data() ->
  cuter_minheap:new(fun erlang:'<'/2).

-spec destroy_data(cuter_minheap:minheap()) -> ok.
destroy_data(TQ) ->
  cuter_minheap:delete(TQ),
  ok.

-spec handle_execution(cuter_minheap:minheap(), cuter_analyzer:reversible_with_tags(), handle(), cuter_cerl:visited_tags(), operationId(), cuter:depth()) -> cuter_minheap:minheap().
handle_execution(TQ, Rvs, Handle, Visited, N, Depth) ->
  Items = generate_queue_items(Rvs, Handle, Visited, N, Depth),
  lists:foreach(fun(Item) -> cuter_minheap:insert(Item, TQ) end, Items),
  TQ.

-spec no_paths(cuter_minheap:minheap()) -> boolean().
no_paths(TagsQueue) ->
  cuter_minheap:is_empty(TagsQueue).

-spec locate_next_reversible(cuter_minheap:minheap(), cuter_cerl:visited_tags()) -> {ok, integer(), cuter_scheduler_maxcover:handle()} | empty.
locate_next_reversible(Queue, Visited) ->
  locate_next_reversible(Queue, Visited, cuter_minheap:heap_size(Queue)).

-spec locate_next_reversible({cuter_minheap:minheap()}, cuter_cerl:visited_tags(), integer()) -> {ok, integer(), cuter_scheduler_maxcover:handle()} | empty.
locate_next_reversible(Queue, Visited, M) ->
  case cuter_minheap:take_min(Queue) of
    {error, empty_heap} -> empty;
    {true, N, _TagID, Handle} -> {ok, N, Handle};
    {false, N, TagID, Handle} ->
      %% Check if the tag is actually visited
      case gb_sets:is_element(TagID, Visited) of
	%% If it's not visited, then return it.
	false -> {ok, N, Handle};
	%% else, put it back in the heap.
	true  ->
	  case M of
	    0 -> {ok, N, Handle};  % Have seen all the entries at least once (possible redundant)
	    _ ->
	      cuter_minheap:insert({true, N, TagID, Handle}, Queue),
	      locate_next_reversible(Queue, Visited, M-1)
	  end
      end
  end.


%% ----------------------------------------------------------------------------
%% Functions for tags
%% ----------------------------------------------------------------------------

%% Each item in the heap is in the form {Visited, OperationId, TagId, Handle}
%% where Visited     : if the tag has already been visited during an execution
%%       OperationId : the cardinality of the constraint in the path vertex
%%       TagId       : the Id of the tag that will be visited
%%       Handle      : the unique identifier of the concolic execution


-spec generate_queue_items(cuter_analyzer:reversible_with_tags(), handle(), cuter_cerl:visited_tags(), operationId(), cuter:depth()) -> [item()].
generate_queue_items(Rvs, Handle, Visited, N, Depth) ->
  generate_queue_items(Rvs, Handle, Visited, N, Depth, []).

-spec generate_queue_items(cuter_analyzer:reversible_with_tags(), handle(), cuter_cerl:visited_tags(), operationId(), cuter:depth(), [item()]) -> [item()].
generate_queue_items([], _Handle, _Visited, _N, _Depth, Acc) ->
  lists:reverse(Acc);
generate_queue_items([R|Rs], Handle, Visited, N, Depth, Acc) ->
  case maybe_item(R, Handle, Visited, N) of
    false -> generate_queue_items(Rs, Handle, Visited, N, Depth, Acc);
    {ok, Item} -> generate_queue_items(Rs, Handle, Visited, N, Depth, [Item|Acc])
  end.

-spec maybe_item(cuter_analyzer:reversible_with_tag(), handle(), cuter_cerl:visited_tags(), operationId()) -> {ok, item()} | false.
maybe_item({Id, TagID}, Handle, Visited, N) ->
  case Id < N of
    true  -> false;
    false -> {ok, {gb_sets:is_element(TagID, Visited), Id, TagID, Handle}}
  end.
