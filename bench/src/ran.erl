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

%%%-------------------------------------------------------------------
%%% File    : ran.erl
%%% Author  : Rickard Green <rickard.s.green@ericsson.com>
%%% Description : 
%%%
%%% Created :  2 Nov 2005 by Rickard Green <rickard.s.green@ericsson.com>
%%%-------------------------------------------------------------------

-module(ran).

-export([bench_args/2, run/3]).

bench_args(Version, Conf) ->
    {_,Cores} = lists:keyfind(number_of_cores, 1, Conf),
	F = case Version of
		short -> 1;
		intermediate -> 2;
		long -> 4
	end,
	[[N] || N <- [F * Cores]].

mk_ranlist(0, _, Acc) -> Acc;
mk_ranlist(N, M, Acc) -> mk_ranlist(N-1, M, [random:uniform(M) | Acc]). 

mk_ranlist(Len, Max) ->
	random:seed(Len, Max, Max * 2),
	mk_ranlist(Len, Max, []).

random(N) ->
	Len = 100000,
	{_, [Mid| _]} = lists:split(Len div 2, lists:sort(mk_ranlist(Len, 2*N))),
	Mid.

run([N|_], _, _) when is_integer(N) ->
	Parent = self(),
	PList = lists:map(fun (_) ->
		spawn(fun () ->
			receive {Parent, go} -> ok end,
			Parent ! {self(), random(100)}
		end)
	end, lists:seq(1,N)),
	lists:foreach(fun (P) -> P ! {Parent, go} end, PList),
	lists:foreach(fun (P) -> receive {P, _RN} -> ok end end, PList),
	ok.
