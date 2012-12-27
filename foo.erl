-module(foo).
-export([mymin/1, test/3, test/2, test/0, goo/1]).

mymin([H|T]) -> mymin(T, H).

mymin([], CurrentMin) -> CurrentMin;
mymin([H|T], CurrentMin) ->
   case H < CurrentMin of
      true  -> mymin(T, H);
      false -> mymin(T, CurrentMin)
   end.
   
test(A, B) ->
  X = fun(Y) -> Y end,
  Y = fun(F, XX) -> F(XX) + 4 end,
  {X(A), test(), Y(X, B), lists:reverse([1,2,3])}.
  
test() ->
  [1,2].
  
test(A, B, {C, D}=E) ->
  case A of
    [a,b,c] -> E;
    _ -> {B, C, D}
  end.

goo(X) ->
  try throw({ok, X}) of
    Y -> Y
    catch C:E -> {C, E} end.
