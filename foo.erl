-module(foo).
-export([mymin/1, test/3, test/1]).

mymin([H|T]) -> mymin(T, H).

mymin([], CurrentMin) -> CurrentMin;
mymin([H|T], CurrentMin) ->
   case H < CurrentMin of
      true  -> mymin(T, H);
      false -> mymin(T, CurrentMin)
   end.
   
test(A) ->
  {1, a}.
  
test(A, B, C) ->
  [A, B, C].

