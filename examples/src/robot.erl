-module(robot).
-export([solve/4]).

%%
%% Simple DFS imple
%%
%% Assume a NxN map and a robot that wants to move from Start to End.
%% The robot can move Up, Right, Down and Left.
%% It cannot move to a cell that is occupied by an Obstacle.
%%
%% solve/4 returns the length of a feasible path.
%% If no path is available, it returns false.
%%
%% BUG: An exeption is raised when the robot tries to move to a point 
%% that is outside the map on the left side.
%% e.g. robot:solve(7, {7,4}, {1,2}, [{1,4},{5,7},{7,6}]).
%%
%%
%% Sample Usage:
%% robot:solve(8, {6,7}, {7,3}, [{5,5},{6,5},{7,5}]). (Result is 3)
%% 
%% Sample Cuter Usage
%% erl -noinput -pa ebin -pa examples/ebin -eval "coordinator:run(robot, solve, [8, {6,7}, {7,3}, [{5,5},{6,5},{7,5}]], 50)" -s init stop

-define(MAP_SIZE, 8).

-type map_size() :: ?MAP_SIZE.
-type coordinate() :: 1..?MAP_SIZE.
-type point() :: {coordinate(), coordinate()}.
-type obstacles() :: [point()].

-spec solve(map_size(), point(), point(), obstacles()) -> term().
%solve(N, {SX, SY}, {EX, EY}, _) when SX > N; SY > N; EX > N; EY > N -> false;
solve(N, Start, End, Obstacles) -> dfs(N, Start, End, Obstacles, [], [], []).

dfs(_N, _End, _End, _Obstacles, _Queue, _Visited, Path) ->
  length(Path);
dfs(N, Curr, End, Obstacles, Queue, Visited, Path) ->
  case expand(N, Curr, Obstacles, Queue, Visited) of
    [] -> false;
    [M | Ms] ->
      _ = validate_move(M, N),
      dfs(N, M, End, Obstacles, Ms, [M|Visited], [M|Path])
  end.

validate_move({X, Y}, N) when X > 0, Y > 0, X =< N, Y =< N ->
  ok;
validate_move(Point, _N) -> exit({invalid_point, Point}).

expand(N, {X, Y}, Obstacles, Pending, Visited) -> 
  Up = {X, Y+1},
  Right = {X+1, Y},
  Down = {X, Y-1},
  Left = {X-1, Y},
  L = lists:foldl(
    fun(M, Acc) ->
      case can_move(N, M, Obstacles) andalso (not is_visited(M, Visited)) of
        true -> [M|Acc];
        false -> Acc
      end
    end,
    Pending,
    [Left, Down, Right, Up]
  ),
  L.

is_visited(Point, Visited) -> member(Point, Visited).

can_move(N, {X, Y}, _) when X > N; Y > N -> false;
can_move(_, {_, Y}, _) when Y < 1 -> false;
can_move(_, _Point, [_Point|_]) -> false;
can_move(_, _, []) -> true;
can_move(_, Point, Obstacles) -> not member(Point, Obstacles).


%% An implementation of lists:member/2 that is not a BIF
%member(X, L) -> lists:member(X, L).
member(_, []) -> false;
member(X, [X|_]) -> true;
member(X, [_|L]) -> member(X, L).

