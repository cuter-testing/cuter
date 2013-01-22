%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2009. All Rights Reserved.
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
%%% File    : big.erl
%%% Author  : Rickard Green <rickard.s.green@ericsson.com>
%%% Description : A simple message passing benchmark
%%%
%%% Created : 30 Dec 2005 by Rickard Green <rickard.s.green@ericsson.com>
%%%-------------------------------------------------------------------
-module(big).

-export([bench_args/2, run/3]).

bench_args(Version, Conf) ->
	{_,Cores} = lists:keyfind(number_of_cores, 1, Conf),
	F = case Version of 
		short -> 8;
		intermediate -> 16;
		long -> 24
	end,
	[[N] || N <- [F * Cores]].

run([N|_], _, _) ->
	Procs = spawn_procs(N),
	RMsgs = lists:map(fun (P) -> {done, P} end, Procs),
	send_procs(Procs, {procs, Procs, self()}),
	receive_msgs(RMsgs),
	lists:foreach(fun (P) -> P ! die end, Procs),
	ok.

pinger([], [], true) ->
	receive
		{procs, Procs, ReportTo} -> pinger(Procs, [], ReportTo)
    end;
pinger([], [], false) ->
	receive
		{ping, From} -> 
			From ! {pong, self()},
			pinger([],[],false);
		die ->
			ok			
	end;
pinger([], [], ReportTo) ->
	ReportTo ! {done, self()},
	pinger([],[],false);
pinger([],[Po|Pos] = Pongers, ReportTo) ->
	receive
	{ping, From} -> 
		From ! {pong, self()},
		pinger([], Pongers, ReportTo);
	{pong, Po} ->
		pinger([], Pos, ReportTo)
	end;
pinger([Pi|Pis], Pongers, ReportTo) ->
	receive 
		{ping, From} -> From ! {pong, self()}
		after 0 -> ok
	end,
	Pi ! {ping, self()},
	pinger(Pis, [Pi|Pongers], ReportTo).

spawn_procs(N) when N =< 0 ->
	[];
spawn_procs(N) ->
	[spawn_link(fun () -> pinger([], [], true) end) | spawn_procs(N-1)].

send_procs([], Msg) ->
	Msg;
send_procs([P|Ps], Msg) ->
	P ! Msg,
	send_procs(Ps, Msg).

receive_msgs([]) ->
	ok;
receive_msgs([M|Ms]) ->
	receive
		M -> receive_msgs(Ms)
	end.

