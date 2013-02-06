-module(demo).
-export([fib/1, min/1, spawn_apply/3]).
-compile({no_auto_import,[min/2]}).

%% Naive Fibonnaci Number implementation
%% coordinator:run(demo,fib,[4]).
fib(0) -> 0;
fib(1) -> 1;
fib(N) when N > 1 -> fib(N-1) + fib(N-2).

%% Calculate the Minimum element of a list
%% coordinator:run(demo,min,[[5,1,3]]).
min([H|T]) -> min(T, H).

min([], CurrentMin) -> CurrentMin;
min([H|T], CurrentMin) ->
  case H < CurrentMin of
    true  -> min(T, H);
    false -> min(T, CurrentMin)
  end.

%% Spawn a process that returns the result of apply(M,F,As)
%% coordinator:run(demo,spawn_apply,[erlang,'++',[[1,2,3],[a,b,c]]]).
spawn_apply(M, F, As) ->
  Parent = self(),
  P = spawn(fun() -> Parent ! {self(), erlang:apply(M, F, As)} end),
  receive
    {P, Value} -> Value
  end.

