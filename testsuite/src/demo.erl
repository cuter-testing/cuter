-module(demo).

-export([fib/1, min/1, distributed_pp/1, selective_receive/1, foo/2]).

-compile({no_auto_import,[min/2]}).

-spec foo(integer(), integer()) -> ok.
foo(X, Y) ->
  Z = 2*Y,
  case X =:= 100000 andalso X < Z of
    false -> ok;
    true -> error(assertion)
  end.

%% Naive Fibonnaci Number implementation
%% cuter:run(demo,fib,[4]).
-spec fib(non_neg_integer()) -> non_neg_integer().
fib(0) -> 0;
fib(1) -> 1;
fib(N) when N > 1 -> fib(N-1) + fib(N-2).

%% Calculate the Minimum element of a list
%% cuter:run(demo,min,[[5,1,3]]).
-spec min(list()) -> any().
min([H|T]) -> min(T, H).

min([], CurrentMin) -> CurrentMin;
min([H|T], CurrentMin) ->
  case H < CurrentMin of
    true  -> min(T, H);
    false -> min(T, CurrentMin)
  end.
  
%% Test selective receive
%% cuter:run(demo,selective_receive,[100]).
selective_receive(N) ->
  Msg1 = some_important_message,
  Msg2 = less_important_message,
  Msg3 = spam,
  Fun = 
    fun() ->
      receive go -> ok end,
      receive
        {From, Msg1} -> From ! {self(), high};
        {From, Msg2} -> From ! {self(), low}
      end
    end,
  [PH, PL] = [spawn(Fun) || _ <- [1,2]],
  [Pid ! {self(), Msg3} || _ <- lists:seq(1, N), Pid <- [PH, PL]],
  PH ! {self(), Msg1},
  PL ! {self(), Msg2},
  [Pid ! go || Pid <- [PH, PL]],
  receive {PH, high} -> ok end,
  receive {PL, low} -> ok end.
  
%% Start a slave node to perform a list operation
%% and receive the result
%% cuter:run(demo,distributed_pp,[lists:seq(1,100)]).
distributed_pp(X) when is_list(X) ->
  net_kernel:start(['master', 'shortnames']),
  {'ok', Host} = inet:gethostname(),
  {'ok', Node} = slave:start(list_to_atom(Host), 'slave'),
  F = fun() ->
    Rv = length(X),
    receive {From, 'ping'} -> From ! {self(), Rv} end
  end,
  Fpid = spawn(Node, F),
  Fpid ! {self(), 'ping'},
  receive {Fpid, Msg} -> Msg end.

