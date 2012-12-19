-module(foo).
-export([mymin/1, test/3]).

mymin([H|T]) -> mymin(T, H).

mymin([], CurrentMin) -> CurrentMin;
mymin([H|T], CurrentMin) ->
   case H < CurrentMin of
      true  -> mymin(T, H);
      false -> mymin(T, CurrentMin)
   end.
   
test(A) ->
  B = A,
  B = 5.
  
test(A, B, C) ->
  [A, B, C].

