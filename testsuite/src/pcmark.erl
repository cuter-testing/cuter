%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%

%% Author  : Björn-Egil Dahlberg
%% Created : 17 Dec 2009 by Björn-Egil Dahlberg

-module(pcmark).

-export([gen_args/0, run/3]).

-define(etstables, [ets1,ets2,ets3,ets4,ets5]).

%% abstract
%% 

gen_args() -> [[8, 16, 16]].

run(Size, Ongoing, Total) ->
  init_ets(?etstables, Size),
  master(init_procs(Ongoing), Total - Ongoing),
  [ets:delete(T) || T <- ?etstables],
  ok.

master(Pids, 0) -> 
  [receive {Pid, done} -> ok end || Pid <- Pids],
  ok;
master(Pids, N) ->
  receive
    {Pid, done} ->  Me  = self(),
            New = spawn_link(fun() -> worker(Me) end),
            master([New|lists:delete(Pid, Pids)], N - 1)
  end.

worker(Parent) ->
  S = lists:foldl(fun (T, EA) ->
    Ttotal = ets:foldl(fun ({K, V}, TA) ->
      ets:insert(T, {K, V + 1 }),
      TA + V
    end, 0, T),
    Ttotal + EA 
  end, 0, ?etstables),
  do(S),
  Parent ! {self(), done}.

do(S) -> do(S,0).

do(S, N) when S > 0 ->
  do(S - 1, N + S);
do(_,_) -> ok.

init_procs(Ongoing) ->
  Me = self(),
  lists:foldl(fun (_,Pids) -> [spawn_link(fun() -> worker(Me) end)|Pids] end, [], lists:seq(1, Ongoing)).

init_ets([], _) -> ok;
init_ets([T|Ts], Size) ->
  ets:new(T, [public, named_table, ordered_set]),
  lists:foreach(fun(I) ->
    ets:insert(T, {I, 1})
  end, lists:seq(1, Size)),
  init_ets(Ts, Size).
