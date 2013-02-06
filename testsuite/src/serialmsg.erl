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
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% %CopyrightEnd%

%% Author  : Björn-Egil Dahlberg
%% Created : 17 Dec 2009 by Björn-Egil Dahlberg

-module(serialmsg).

-export([bench_args/2, run/3]).

bench_args(Version, Conf) ->
    {_,Cores} = lists:keyfind(number_of_cores, 1, Conf),
	[F1, F2, F3] = case Version of
		short -> [2, 16, 32];  
		intermediate -> [2, 32, 40];  
		long -> [5, 16, 32]
	end,
	[[P,N,L] || P <- [F1 * Cores], N <- [F2 * Cores], L <- [F3 * Cores]].

run([P,N,L|_], _, _) ->
	Recvs = setup_receivers(P),
	Disp  = setup_dispatcher(),
	Gens  = setup_generators(Recvs, Disp, N, L),
	[Pid ! {self(), do} || Pid <- Gens],
	[receive {Pid, done} -> ok end || Pid <- Recvs],
	Disp ! {self(), done},
	ok.

%% setups

setup_receivers(P) -> setup_receivers(P, self(), []).

setup_receivers(0, _, Out) -> Out;
setup_receivers(P, Pid, Out) -> 
	setup_receivers(P - 1, Pid, [spawn_link(fun() -> receiver(Pid) end)|Out]).

setup_dispatcher() ->
	Me = self(),
	spawn_link(fun() -> dispatcher(Me) end).

setup_generators(Recvs, Disp, N, L) ->
	setup_generators(Recvs, Disp, self(), N, L, []).

setup_generators([],_,  _, _, _, Out) -> Out;
setup_generators([Recv|Recvs], Disp, Pid, N, L, Out) ->
	setup_generators(Recvs, Disp, Pid, N, L, [spawn_link(fun() -> generator(Recv, Disp, Pid, N, L) end) | Out]).

%% processes

receiver(Master) ->
	receive
		{_, done} -> Master ! {self(), done};
		{_, _} -> receiver(Master)
	end.

dispatcher(Master) ->
	receive
		{Master, done} -> ok;
		{Pid, To, Data} -> 	To ! {Pid, Data},
							dispatcher(Master)
	end.

generator(Recv, Disp, Master, N, L) ->
	Data = lists:seq(1, L),
	receive
		{Master, do} -> generator_push_loop(Recv, Disp, N, Data);
		{Master, do, NewN} -> generator_push_loop(Recv, Disp, NewN, Data)
	end.

generator_push_loop(Recv, Disp, 0, _) ->
	Disp ! {self(), Recv, done};
generator_push_loop(Recv, Disp, N, Data) ->
	Disp ! {self(), Recv, Data},
	generator_push_loop(Recv, Disp, N - 1, Data).
