%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 

%%
%% File:   timer_wheel.erl
%% Author: Björn-Egil Dahlberg
%% Date:   2010-09-10
%%

-module(timer_wheel).

-export([gen_args/0, run/2]).
-export([wheel/1,no_wheel/1]).

gen_args() -> [[wheel, 32], [no_wheel, 32]].

run(wheel, N) ->
  test(N, fun recv_loop_after/2);
run(no_wheel, N) ->
  test(N, fun recv_loop/2).

wheel(N) ->
  test(N, fun recv_loop_after/2).

no_wheel(N) ->
  test(N, fun recv_loop/2).

test(N, Fun) ->
  Me = self(),
  Pids = [spawn_link(fun() -> handler(N - 1, Fun, Me) end) || _ <- lists:seq(1, N)],
  [Pid ! {init, Pids -- [Pid]} || Pid <- Pids],
  [Pid ! start || Pid <- Pids],
  [receive {Pid, done} -> ok end || Pid <- Pids],
  ok.

handler(N, Fun, Me) ->
  Others = receive {init, Pids} -> Pids end,
  receive start -> ok end,
  loop(Others, N, Fun, Me).

loop([], 0, _, Me) ->
  Me ! {self(), done};
loop([], N, Fun, Me) ->
  loop([], Fun(undefined, N), Fun, Me);
loop([Pid|Pids], N, Fun, Me) ->
  Pid ! {self(), ping},
  loop(Pids, Fun(Pid, N), Fun, Me).

recv_loop_after(_, 0) ->
  0;
recv_loop_after(Pid, N) ->
  receive
    {Pid, pong} -> N;
    {Other, ping} ->  Other ! {self(), pong},
              recv_loop_after(Pid, N - 1)
    after 1073741824 -> exit(self(), kill)
  end.

recv_loop(_, 0) ->
  0;
recv_loop(Pid, N) ->
  receive
    {Pid, pong} -> N;
    {Other, ping} ->  Other ! {self(), pong},
              recv_loop(Pid, N - 1)
  end.
