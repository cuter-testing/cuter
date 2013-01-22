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

-module(genstress).

-behaviour(gen_server).

-export([bench_args/2, run/3]).

% %gen cb:s

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/0, stop/0]).

-record(state, {mstate}).

bench_args(Version, Conf) ->
    {_,Cores} = lists:keyfind(number_of_cores, 1, Conf),
	[F1, F2, F3] = case Version of
		short -> [16, 4, 8]; 
		intermediate -> [16, 10, 11]; 
		long -> [16, 47, 79]
	end,
    [[Type,Np,N,Cqueue] || Type <- [proc_call,gen_call], Np <- [F1 * Cores], N <- [F2 * Cores], Cqueue <- [F3 * Cores]].

run([Type,Np,N,Cqueue|_], _, _) ->
	Server  = start_server(Type),
	Clients = start_clients(Np, Cqueue),
	[Pid ! {{Type, Server}, N, self()} || Pid <- Clients],
	[receive {Pid, ok} -> ok end || Pid <- Clients],
	stop_server({Type, Server}),
	stop_clients(Clients),
	ok.

start_server(gen_call) -> genstress:start();
start_server(proc_call) -> spawn_link(fun() -> server() end).

stop_server({gen_call,  _}) -> genstress:stop();
stop_server({proc_call, S}) -> S ! stop.

start_clients(Np, Queue) -> 
	[spawn_link(fun() -> client(Queue) end) || _ <- lists:seq(1, Np)].

stop_clients(Clients) ->
	[erlang:exit(Client, normal) || Client <- Clients],
	ok.

client(Queue) ->
	[self() ! dont_match_me || _ <- lists:seq(1, Queue)],
	client().
client() ->
	receive
		{{gen_call,  _}, N, Pid} -> client(gen_call, N, Pid);
		{{proc_call, S}, N, Pid} -> client({proc_call,S}, N, Pid)
	end.

client(_, 0, Pid) -> Pid ! {self(), ok};
client(CallType , N, Pid) ->
	stress = client_call(CallType, stress),
	client(CallType, N - 1, Pid).

client_call(gen_call, Msg) -> gen_server:call(?MODULE, Msg);
client_call({proc_call,S}, Msg) -> S ! {self(), Msg}, receive {S,Ans} -> Ans end.

server() ->
	receive 
		stop -> ok;
		{From, Msg} -> From ! {self(), Msg}, server()
	end.

%% -------------------------------------------------------------------- %%
%%
%% start/stop
%%
%% -------------------------------------------------------------------- %%

start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
stop() ->
	gen_server:call(?MODULE, stop).

init(_Args) ->
	{ok, #state{}}.

%% -------------------------------------------------------------------- %%
%%
%% handle_call
%%
%% -------------------------------------------------------------------- %%

handle_call(stop, From, S) ->
  {stop, normal, stopped, S#state{mstate=From}};
  
handle_call(Command, From, S)->
	{reply, Command, S#state{mstate=From}}.

%% -------------------------------------------------------------------- %%
%%
%% handle_cast
%%
%% -------------------------------------------------------------------- %%

handle_cast(_Other, State) ->
	{noreply, State}.

%% -------------------------------------------------------------------- %%
%%
%% handle_info
%%
%% -------------------------------------------------------------------- %%

handle_info(_Info, State) ->
	{noreply, State}.

%% -------------------------------------------------------------------- %%
%%
%% termination
%%
%% -------------------------------------------------------------------- %%

terminate(_Reason, _State) ->
	ok.

%% -------------------------------------------------------------------- %%
%%
%% code_change
%%
%% -------------------------------------------------------------------- %%

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

