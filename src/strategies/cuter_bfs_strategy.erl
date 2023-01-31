%% -*- erlang-indent-level: 2 -*-
-module(cuter_bfs_strategy).
-behaviour(cuter_strategy).

-export([locate_next_reversible/2, init/0, clean_up/1, no_paths/1, handle_execution/6]).

-export_type([state/0]).

%% The persistent state for the strategy; a min-heap.
-type state() :: cuter_minheap:minheap().
%% Each item in the heap is in the form {Visited, OperationId, TagId, Handle}
%% where Visited     : if the tag has already been visited during an execution
%%       OperationId : the cardinality of the constraint in the path vertex
%%       TagId       : the Id of the tag that will be visited
%%       Handle      : the unique identifier of the concolic execution
-type item() :: {boolean(), cuter_scheduler:operation_id(), cuter_cerl:tagID(), cuter_scheduler:handle()}.

%% Initializes the persistent state.
-spec init() -> state().
init() ->
  cuter_minheap:new(fun erlang:'<'/2).

%% Cleans up the persistent state.
-spec clean_up(state()) -> ok.
clean_up(S) ->
  cuter_minheap:delete(S),
  ok.

%% Returns true if there are no more nodes to be reversed.
-spec no_paths(state()) -> boolean().
no_paths(S) ->
  cuter_minheap:is_empty(S).

%% Fetches the next node that should be reversed.
-spec locate_next_reversible(state(), cuter_cerl:visited_tags()) -> cuter_strategy:next_tag_response().
locate_next_reversible(State, Visited) ->
  locate_next_reversible(State, Visited, cuter_minheap:heap_size(State)).

-spec locate_next_reversible(state(), cuter_cerl:visited_tags(), integer()) -> cuter_strategy:next_tag_response().
locate_next_reversible(State, Visited, M) ->
  case cuter_minheap:take_min(State) of
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
	            cuter_minheap:insert({true, N, TagID, Handle}, State),
	            locate_next_reversible(State, Visited, M-1)
	        end
      end
  end.

%% Consumes the result of an execution and returns the updated state.
-spec handle_execution(state(), cuter_analyzer:reversible_with_tags(), cuter_scheduler:handle(), cuter_cerl:visited_tags(),
                       cuter_scheduler:operation_id(), cuter:depth()) -> state().
handle_execution(TQ, Rvs, Handle, Visited, N, Depth) ->
  Items = generate_queue_items(Rvs, Handle, Visited, N, Depth),
  lists:foreach(fun(Item) -> cuter_minheap:insert(Item, TQ) end, Items),
  TQ.

-spec generate_queue_items(cuter_analyzer:reversible_with_tags(), cuter_scheduler:handle(), cuter_cerl:visited_tags(),
                           cuter_scheduler:operation_id(), cuter:depth()) -> [item()].
generate_queue_items(Rvs, Handle, Visited, N, Depth) ->
  generate_queue_items(Rvs, Handle, Visited, N, Depth, []).

-spec generate_queue_items(cuter_analyzer:reversible_with_tags(), cuter_scheduler:handle(), cuter_cerl:visited_tags(),
                           cuter_scheduler:operation_id(), cuter:depth(), [item()]) -> [item()].
generate_queue_items([], _Handle, _Visited, _N, _Depth, Acc) ->
  lists:reverse(Acc);
generate_queue_items([R|Rs], Handle, Visited, N, Depth, Acc) ->
  case maybe_item(R, Handle, Visited, N) of
    false -> generate_queue_items(Rs, Handle, Visited, N, Depth, Acc);
    {ok, Item} -> generate_queue_items(Rs, Handle, Visited, N, Depth, [Item|Acc])
  end.

-spec maybe_item(cuter_analyzer:reversible_with_tag(), cuter_scheduler:handle(), cuter_cerl:visited_tags(),
                 cuter_scheduler:operation_id()) -> {ok, item()} | false.
maybe_item({Id, TagID}, Handle, Visited, N) ->
  case Id < N of
    true  -> false;
    false -> {ok, {gb_sets:is_element(TagID, Visited), Id, TagID, Handle}}
  end.
