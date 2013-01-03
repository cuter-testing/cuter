-module(foo).
-export([mymin/1, test/3, test/2, test/0, goo/1, boo/1, y/1]).

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
  lists:map(fun moo/1, [1,2,3]).
  
test(A, B, {C, D}=E) ->
  case A of
    [a,b,c] -> E;
    _ -> {B, C, D}
  end.
  
moo(X) -> 2 * X.

goo(X) ->
  Y = length(X),
  F = fun() -> lists:reverse([2,4]) end,
  spawn_opt(F, [monitor]),
  Y * Y.
  
boo(X) ->
  F = fun(Z) ->
    try error({ok, Z}) of
      Y -> Y
    catch
      error:E -> {got, E}
    end
  end,
  catch F(X).
  
y(M) ->
    G = fun (F) -> M(fun() -> (F(F))() end) end,
    G(G).
