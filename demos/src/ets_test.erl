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

%%%-------------------------------------------------------------------
%%% File    : ets_test.erl
%%% Author  : Rickard Green <rickard.s.green@ericsson.com>
%%% Description : 
%%%
%%% Created : 27 Mar 2008 by Rickard Green <rickard.s.green@ericsson.com>
%%%-------------------------------------------------------------------

-module(ets_test).

-export([bench_args/2, run/3]).

bench_args(Version, Conf) ->
    {_,Cores} = lists:keyfind(number_of_cores, 1, Conf),
	[F1, F2, F3] = case Version of
		short -> [157, 1, 8];
		intermediate -> [157, 32, 32];
		long -> [157, 79, 40]
	end,
	[[N,W,R] || N <- [F1 * Cores], W <- [F2 * Cores], R <- [F3 * Cores]].

run([N,W,R|_], _, _) ->
	Parent = self(),
    io:format("xxxxxx"),
	T = ets:new(x, [public]),
	io:format("xxx"),
	w(T, N, init),
	Ws = lists:map(fun (_) ->
			spawn_link(fun () ->
				receive go -> ok end,
				w(T, N, self()),
				w(T, N, self()),
				Parent ! {done, self()},
				receive after infinity -> ok end
			end)
		end,lists:seq(1, W)),
	Rs = lists:map(fun (_) ->
			spawn_link(fun () ->
				receive go -> ok end,
				r(T, N),
				r(T, N),
				Parent ! {done, self()},
				receive after infinity -> ok end
			end)
		end, lists:seq(1, R)),
	lists:foreach(fun (P) -> P ! go end, Ws),
	lists:foreach(fun (P) -> P ! go end, Rs),
	lists:foreach(fun (P) -> receive {done, P} -> ok end end, Ws),
	lists:foreach(fun (P) -> receive {done, P} -> ok end end, Rs),
	lists:foreach(fun (P) -> unlink(P), exit(P, bye) end, Ws),
	lists:foreach(fun (P) -> unlink(P), exit(P, bye) end, Rs),
	ok.

r(_T, 0) ->
	ok;
r(T, N) ->
	[{N, _}] = ets:lookup(T, N),
	r(T, N-1).

w(_T, 0, _V) ->
	ok;
w(T, N, V) ->
	true = ets:insert(T, {N, V}),	
	w(T, N-1, V).
