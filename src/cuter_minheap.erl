%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_minheap).

-export([new/1, delete/1, heap_size/1, is_empty/1, min/1, take_min/1, insert/2, from_list/2]).

-export_type([minheap/0]).

-type compare_function() :: fun((any(), any()) -> any()).

%%
%% @type minheap(). Min Heap with user-defined compare function.
%%
-record(heap, {
  lt   :: compare_function(),
  htab :: ets:tab()
}).
-type minheap() :: #heap{}.


%% Creates an empty heap.
-spec new(compare_function()) -> minheap().
new(F) when is_function(F) ->
  H = ets:new(?MODULE, [ordered_set, public]),
  ets:insert(H, {size, 0}),
  #heap{lt=F, htab=H};
new(_F) ->
  erlang:error(badarg).

%% Deletes a heap.
-spec delete(minheap()) -> true.
delete(H) -> ets:delete(H#heap.htab).

%% Returns the number of elements contained in a heap.
-spec heap_size(minheap()) -> non_neg_integer().
heap_size(H) ->
  [{size, Len}] = ets:lookup(H#heap.htab, size),
  Len.

%% Checks whether a heap is empty or not.
-spec is_empty(minheap()) -> boolean().
is_empty(H) -> heap_size(H) =:= 0.

%% Returns the element with the minimum priority without removing it.
-spec min(minheap()) -> any() | {error, empty_heap}.
min(H) ->
  case ets:lookup(H#heap.htab, 1) of
    [] -> {error, empty_heap};
    [{1, Min}] -> Min
  end.

%% Adds a new element to a heap.
-spec insert(any(), minheap()) -> ok.
insert(X, H) ->
  Pos = heap_size(H) + 1,
  ets:insert(H#heap.htab, {Pos, X}),
  ets:insert(H#heap.htab, {size, Pos}),
  I = Pos,
  P = I div 2,
  insert_loop(H, I, P).

%% Removes and returns the minimum priority element of a min heap.
-spec take_min(minheap()) -> any() | {error, empty_heap}.
take_min(H) -> pop(H).

%% Creates a heap from a list of terms.
-spec from_list(compare_function(), [any()]) -> minheap().
from_list(F, L) when is_function(F), is_list(L) ->
  HS = erlang:length(L),
  H = ets_from_elements(F, L, HS),
  I = HS div 2,
  construct_heap(H, I, HS),
  H;
from_list(_F, _L) ->
  erlang:error(badarg).

%% Deletes and returns the element at the top of the heap
%% and re-arranges the rest of the heap.
-spec pop(minheap()) -> any() | {error, empty_heap}.
pop(H) ->
  case ets:lookup(H#heap.htab, 1) of
    [] -> {error, empty_heap};
    [{1, Head}] ->
      HS = heap_size(H),
      [{HS, X}] = ets:lookup(H#heap.htab, HS),
      ets:delete(H#heap.htab, HS),
      HS_n = HS - 1,
      ets:insert(H#heap.htab, {size, HS_n}),
      case HS_n of
        0 -> ok;
        _ -> ets:insert(H#heap.htab, {1, X})
      end,
      combine(H, 1, HS_n),
      Head
  end.

%% Re-arranges the heap in a top-down manner.
-spec combine(minheap(), pos_integer(), pos_integer()) -> ok.
combine(H, I, HS) ->
  L = 2*I,
  R = 2*I + 1,
  MP = I,
  MP_L = combine_h1(H, L, MP, HS),
  MP_R = combine_h1(H, R, MP_L, HS),
  combine_h2(H, MP_R, I, HS).

-spec combine_h1(minheap(), pos_integer(), pos_integer(), pos_integer()) -> pos_integer().
combine_h1(H, W, MP, HS) when W =< HS ->
  Lt = H#heap.lt,
  [{W, X}] = ets:lookup(H#heap.htab, W),
  [{MP, Y}] = ets:lookup(H#heap.htab, MP),
  case Lt(X, Y) of
    true  -> W;
    false -> MP
  end;
combine_h1(_H, _W, MP, _HS) -> MP.

-spec combine_h2(minheap(), pos_integer(), pos_integer(), pos_integer()) -> ok.
combine_h2(_H, MP, I, _HS) when MP =:= I -> ok;
combine_h2(H, MP, I, HS) ->
  swap(H, I, MP),
  combine(H, MP, HS).

%% Re-arranges the heap in a bottom-up manner.
-spec insert_loop(minheap(), pos_integer(), pos_integer()) -> ok.
insert_loop(H, I, P) when I > 1 ->
  Lt = H#heap.lt,
  [{I, X}] = ets:lookup(H#heap.htab, I),
  [{P, Y}] = ets:lookup(H#heap.htab, P),
  case Lt(Y, X) of
    true -> ok;
    false ->
      swap(H, P, I),
      NI = P,
      NP = NI div 2,
      insert_loop(H, NI, NP)
  end;
insert_loop(_H, _I, _P) -> ok.

%% Swaps two elements of the heap.
-spec swap(minheap(), pos_integer(), pos_integer()) -> true.
swap(H, I, J) ->
  [{I, X}] = ets:lookup(H#heap.htab, I),
  [{J, Y}] = ets:lookup(H#heap.htab, J),
  ets:insert(H#heap.htab, {I, Y}),
  ets:insert(H#heap.htab, {J, X}).

%% Used for constructing a heap from a list.
-spec construct_heap(minheap(), pos_integer(), pos_integer()) -> ok.
construct_heap(H, I, HS) when I > 0 ->
  combine(H, I, HS),
  construct_heap(H, I-1, HS);
construct_heap(_H, _I, _HS) -> ok.

%% Adds a list of terms to a heap's ets table.
-spec ets_from_elements(compare_function(), [any()], non_neg_integer()) -> minheap().
ets_from_elements(F, L, HS) ->
  H = new(F),
  ets:insert(H#heap.htab, {size, HS}),
  add_elements(H, L, 1).

-spec add_elements(minheap(), [any()], pos_integer()) -> minheap().
add_elements(H, [], _N) ->
  H;
add_elements(H, [T|Ts], N) ->
  ets:insert(H#heap.htab, {N, T}),
  add_elements(H, Ts, N+1).
