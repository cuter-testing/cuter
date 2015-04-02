% Run, e.g., with:
% erl -noshell -pa $PWD/ebin -eval "cuter:run(example, foo11, [[17]], 10)" -s init stop

-module(example).
-export([foo11/1, foo21/1, foo12/1, foo22/1,
         foo11s/1, foo21s/1, foo12s/1, foo22s/1,
         foo11i/1, foo21i/1, foo12i/1, foo22i/1]).

foo11(L) ->
  lists:foreach(fun fcmp1/1, L).

foo21(L) when length(L) < 3 -> small;
foo21(L) -> lists:foreach(fun fcmp1/1, L).

foo12(L) ->
  lists:foreach(fun fcmp2/1, L).

foo22(L) when length(L) < 3 -> small;
foo22(L) -> lists:foreach(fun fcmp2/1, L).

-spec foo11s([any()]) -> any().
foo11s(L) ->
  lists:foreach(fun fcmp1/1, L).

-spec foo21s([any()]) -> any().
foo21s(L) when length(L) < 3 -> small;
foo21s(L) -> lists:foreach(fun fcmp1/1, L).

-spec foo12s([any()]) -> any().
foo12s(L) ->
  lists:foreach(fun fcmp2/1, L).

-spec foo22s([any()]) -> any().
foo22s(L) when length(L) < 3 -> small;
foo22s(L) -> lists:foreach(fun fcmp2/1, L).

-spec foo11i([integer()]) -> ok.
foo11i(L) ->
  lists:foreach(fun fcmp1/1, L).

-spec foo21i([integer()]) -> small | ok.
foo21i(L) when length(L) < 3 -> small;
foo21i(L) -> lists:foreach(fun fcmp1/1, L).

-spec foo12i([integer()]) -> ok.
foo12i(L) ->
  lists:foreach(fun fcmp2/1, L).

-spec foo22i([integer()]) -> small | ok.
foo22i(L) when length(L) < 3 -> small;
foo22i(L) -> lists:foreach(fun fcmp2/1, L).

fcmp1(X) ->
  case cmp(X) of
    gt -> ok;
    lt -> ok
  end.

fcmp2(X) ->
  case atom_to_list(cmp(X)) of
    [_, $t] -> ok
  end.

cmp(X) when X > 42 -> gt;
cmp(42) -> eq;
cmp(X) when X < 42 -> lt.
