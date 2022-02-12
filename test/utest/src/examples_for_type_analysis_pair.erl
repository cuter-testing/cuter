-module(examples_for_type_analysis_pair).

-export([to_int/1, can_bark/1, count_trees/1, tree_height/1]).

-export_type([t_dog_or_cat/0]).

-type t_dog_or_cat() :: dog | cat.

-spec to_int(examples_for_type_analysis:t_int_or_atom()) -> integer().
to_int(X) when is_integer(X) -> X;
to_int(X) when is_atom(X) -> lists:max(atom_to_list(X)).

-spec can_bark(Animals) -> boolean() when
        Animals :: [Animal],
        Animal :: t_dog_or_cat().
can_bark(Animals) -> lists:any(fun (A) -> A =:= dog end, Animals).

-spec count_trees(Forest) -> integer() when
        Forest :: {Tree, Forest} | nil,
        Tree :: {atom(), Forest} | empty.
count_trees(F) ->
  count_trees([F], 0).

count_trees([], N) -> N;
count_trees([nil|Forests], N) ->
  count_trees(Forests, N);
count_trees([{empty, Forest}|Forests], N) ->
  count_trees([Forest|Forests], N + 1);
count_trees([{{_Name, InnerForest}, Forest}|Forests], N) ->
  count_trees([InnerForest, Forest|Forests], N + 1).

-spec tree_height(Tree) -> integer() when
        Tree :: Node | Leaf,
        Node :: {Tree, Tree},
        Leaf :: nil.
tree_height(nil) -> 0;
tree_height({Left, Right}) ->
  H1 = tree_height(Left),
  H2 = tree_height(Right),
  1 + max(H1, H2).
