-module(examples_for_type_analysis_pair).

-export([to_int/1, can_bark/1]).

-export_type([t_dog_or_cat/0]).

-type t_dog_or_cat() :: dog | cat.

-spec to_int(examples_for_type_analysis:t_int_or_atom()) -> integer().
to_int(X) when is_integer(X) -> X;
to_int(X) when is_atom(X) -> lists:max(atom_to_list(X)).

-spec can_bark(Animal) -> boolean() when Animal :: t_dog_or_cat().
can_bark(Animal) -> Animal =:= dog.
