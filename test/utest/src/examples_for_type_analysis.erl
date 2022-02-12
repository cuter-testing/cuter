-module(examples_for_type_analysis).

-export([id/1, inc/1, to_atom/1, translate/3, root/1, max_x/1, is_dog/1]).

-export_type([t_int_or_atom/0]).

-type t_int_or_atom() :: t_int() | atom().
-type t_int() :: integer().

-record(point, {x :: number(), y :: number()}).

-type tree() :: {integer(), tree(), tree()} | nil.

-type list_of(X) :: [X].

-type point() :: #point{}.

-spec id(any()) -> any().
id(X) -> X.

-spec inc(t_int()) -> t_int().
inc(X) -> X + 1.

-spec to_atom(t_int_or_atom()) -> atom().
to_atom(X) when is_atom(X) -> X;
to_atom(X) when is_integer(X) -> list_to_atom([$0 + X]).

-spec translate(#point{}, number(), number()) -> point().
translate(#point{x=X, y=Y}, DX, DY) -> #point{x = X + DX, y = Y + DY}.

-spec root(tree()) -> integer() | nil.
root({X, _L, _R}) -> X;
root(nil) -> nil.

-spec max_x(list_of(#point{})) -> number().
max_x(Ps) -> lists:max([P#point.x || P <- Ps]).

-spec is_dog(examples_for_type_analysis_pair:t_dog_or_cat()) -> boolean().
is_dog(X) -> X =:= dog.
