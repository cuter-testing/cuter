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

-module(mbrot).

-export([bench_args/2, run/3]).

-compile(native).	%% This benchmark runs faster in native code

-define(MAXITER, 255).
-define(LIM_SQR, 4.0).
-define(RL, 2.0).
-define(IL, 2.0).

bench_args(Version, Conf) ->
	{_,Cores} = lists:keyfind(number_of_cores, 1, Conf),
	[F1, F2] = case Version of
		short -> [2, 4];
		intermediate -> [4, 4];
		long -> [7, 4]
	end,
	[[N,Np] || N <- [F1 * Cores], Np <- [F2 * Cores]].

run([N,Np|_], _, _) ->
	receive_workers(start_workers(N, Np)),
	ok.

start_workers(N, Np) ->
	start_workers(N, Np, self(), []).

start_workers(_, 0, _, Pids) -> Pids;
start_workers(N, Np, Me, Pids) ->
	Pid = spawn_link(fun() -> worker(N, Me) end),
	start_workers(N, Np - 1, Me, [Pid|Pids]).

receive_workers([]) -> ok;
receive_workers([Pid|Pids]) -> receive {Pid, done} -> receive_workers(Pids) end.

worker(N, Parent) ->
	rows(N, N),
	Parent ! {self(), done}.

rows(W,H) -> rows(W, H, H).

rows(W, H, Hi) when Hi > 0->
	cols(W, H, Hi),
	rows(W, H, Hi - 1);
rows(_, _, 0) -> ok.

cols(W, H, Hi) -> cols(W, H, W, Hi).

cols(W, H, Wi, Hi) when Wi > 0 ->
	%% transform X and Y pixel to mandelbrot coordinates
	X = (Wi - 1)/W*(2*?RL) - ?RL,
	Y = (Hi - 1)/H*(2*?IL) - ?IL,
	%% do mandelbrot
	mbrot(X, Y),
	cols(W, H, Wi - 1, Hi);
cols(_, _, 0, _) -> ok.

mbrot(X,Y) -> mbrot(X,Y,X,Y,0).

mbrot(X0, Y0, X1, Y1, I) when I < ?MAXITER, (X1*X1 + Y1*Y1) =< ?LIM_SQR ->
	X2 = X1*X1 - Y1*Y1 + X0,
	Y2 = 2*X1*Y1 + Y0,
	mbrot(X0, Y0, X2, Y2, I + 1);
mbrot(_, _, _, _, I) -> I.

