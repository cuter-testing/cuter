-module(complex_spec).
-export([f/1, g/1]).
-export_type([int/0]).

-type x() :: [{[[integer()]], [float()]} | integer() | {any(), [float()], atom()}].
-type e() :: {x(), x()}.
-type int() :: pos_integer().

-spec f([e()]) -> ok.
f([H|T]=L) when length(L) < 4 ->
  case H of
    {[1,2], [{[[1],[2]], [3.14]},2]} -> error(boom);
    _ -> f(T)
  end;
f(_) -> ok.

-type t() :: nil | {integer(), t(), t()}.

-spec g([t()]) -> integer().
g([]) -> 0;
g([nil|L]) -> g(L);
g([{X,T1,T2}|L]) -> X + g([T1,T2|L]).
