%% -*- erlang-indent-level: 2 -*-
-module(cuter_bfs_strategy).

-export([locate_next_reversible/2, init_data/0, no_paths/1, handle_execution/2]).

-spec init_data() -> tuple().
init_data() ->
  {cuter_minheap:new(fun erlang:'<'/2)}.

-spec handle_execution(tuple(), any()) -> {cuter_minheap:minheap()}.
handle_execution({TQ}, Items) ->
  lists:foreach(fun(Item) -> cuter_minheap:insert(Item, TQ) end, Items),
  {TQ}.

-spec no_paths(tuple()) -> boolean().
no_paths({TagsQueue}) ->
  cuter_minheap:is_empty(TagsQueue).

-spec locate_next_reversible({cuter_minheap:minheap()}, cuter_cerl:visited_tags()) -> {ok, integer(), cuter_scheduler_maxcover:handle()} | empty.
locate_next_reversible({Queue}, Visited) ->
  locate_next_reversible({Queue}, Visited, cuter_minheap:heap_size(Queue)).


-spec locate_next_reversible({cuter_minheap:minheap()}, cuter_cerl:visited_tags(), integer()) -> {ok, integer(), cuter_scheduler_maxcover:handle()} | empty.
locate_next_reversible({Queue}, Visited, M) ->
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
