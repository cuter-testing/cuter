-module(demo).
-compile(export_all).

-compile({no_auto_import,[min/2]}).

f(X, Y) ->
  Z = 2*Y,
  case X =:= 1000000 of
    true  ->
      case X < Z of 
        true  -> error(assertion);
        false -> ok
      end;
    false ->
      ok
  end.
  
  
  
min([H|T]) -> min(T, H).

min([], CurrentMin) -> CurrentMin;
min([H|T], CurrentMin) ->
  case H < CurrentMin of
    true  -> min(T, H);
    false -> min(T, CurrentMin)
  end.
