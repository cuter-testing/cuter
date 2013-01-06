-module(foo).
-export([mymin/1, test/3, test/2, test/0, goo/1, boo/1, y/1, moo/0, test_fac/1, fac/1, r/1, loop/0, send/1, spawn/3]).

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
  
moo() -> [X+Y || X <- [1,2], Y <- [2,3]].
  
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
    
fac(0) -> 1;
fac(N) when N > 0 -> N*fac(N-1).
test_fac(X) -> fac(X),ok.

r(X) ->
  receive
    ok -> X
  end.
  
send(Pid) ->
%  timer:sleep(100),
  Pid ! gello,
  Pid ! {self(), test_msg}.
  
loop() ->
  Pid = erlang:spawn(foo, send, [self()]),
  receive
    {Pid, Msg} -> Msg
  after 100 -> bored
  end.
  
spawn(M, F, A) ->
  Parent = self(),
  Fun = fun() ->
%    io:format("Parent : ~p, Me : ~p~n", [Parent, self()]),
    R = apply(M, F, A)
%    io:format("Result = ~p~n", [R])
  end,
  erlang:spawn(Fun),
%  dict:new(),
%  timer:sleep(100),
  ok.
