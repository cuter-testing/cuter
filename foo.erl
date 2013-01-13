-module(foo).
-export([mymin/1, test/3, test/2, test/0, goo/1, boo/1, y/1, moo/0, test_fac/1, fac/1, r/1, loop/0, send/1, spawn/3, moo/1,
  qoo/1, too/1, too/0, foo/0, test/1]).

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
  lists:map(fun too/1, [1,{2,3},"546"]).
  
test(X) ->
  T = erlang:make_ref(),
  P = self(),
  F = fun() -> P ! {T, X} end,
  spawn(F),
  receive
    {T, Msg} -> {ok, Msg};
    Msg -> {error, Msg}
  end.
  
moo() -> [X+Y || X <- [1,2], Y <- [2,3]].

too(X) -> 
  <<X:4/little-signed-integer-unit:8>>.
%  <<X:8/bitstring>>.
%  Color = 16#F09A29,
%  <<Color:24>>.

too() ->
  Pixels = <<213, 45, 132, 64, 76, 32, 76, 0, 0, 234, 32, 15>>,
  <<Pix1:24, Pix2:24, Pix3:24, Pix4:24>> = Pixels,
  {Pix1, Pix2, Pix3, Pix4}.
%  <<X/bitstring>> = <<1:3>>,
%  X.
  
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
    io:format("Parent : ~p, Me : ~p~n", [Parent, self()]),
    R = apply(M, F, A),
    io:format("Result = ~p~n", [R])
  end,
  erlang:spawn_opt(Fun, [monitor]),
%  dict:new(),
%  timer:sleep(100),
  ok.
  
qoo(Z) ->
  F = fun() -> exit(vgainw), io:format("F~n") end,
  G = fun() -> lists:reverse(lists:reverse(lists:reverse(Z))), io:format("G~n") end,
  K = fun() -> timer:sleep(1000), io:format("K~n") end,
  erlang:spawn(G),erlang:spawn(K),erlang:spawn(F),
  ok.
  
foo() -> <<1:3>>.
