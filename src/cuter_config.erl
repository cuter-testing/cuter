%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_config).
-behaviour(gen_server).

%% External API.
-export([fetch/1, store/2, start/0, stop/0]).
%% gen_server callbacks
-export([init/1, terminate/2, code_change/3, handle_info/2, handle_call/3, handle_cast/2]).

-define(CONFIG_SERVER, config_server).

-type from() :: {pid(), reference()}.
-type name() :: atom().
-type value() :: any().
-type fetch_ret() :: {ok, value()} | not_found.

%% The persistent state of the server.
-record(st, {
  %% The storage for the configuration parameters.
  parameters = orddict:new() :: orddict:orddict(name(), value())
}).
-type state() :: #st{}.

%% ============================================================================
%% External API
%% ============================================================================

%% Starts the configuration server.
-spec start() -> ok.
start() ->
  case gen_server:start_link({local, ?CONFIG_SERVER}, ?MODULE, [], []) of
    {ok, _Pid} -> ok;
    {error, R} -> exit({config_start, R})
  end.

%% Stops the configuration server.
-spec stop() -> ok.
stop() ->
  gen_server:call(?CONFIG_SERVER, stop).

%% Stores the value for the given parameter.
-spec store(name(), value()) -> ok.
store(Parameter, Value) ->
  gen_server:cast(?CONFIG_SERVER, {store, Parameter, Value}).

%% Retrieves the value for the given parameter.
-spec fetch(name()) -> fetch_ret().
fetch(Parameter) ->
  gen_server:call(?CONFIG_SERVER, {fetch, Parameter}).

%% ============================================================================
%% gen_server callbacks (Server Implementation)
%% ============================================================================

%% gen_server callback : init/1
-spec init([]) -> {ok, state()}.
init([]) ->
  {ok, #st{}}.

%% gen_server callback : terminate/2
-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
  ok.

%% gen_server callback : code_change/3
-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.  %% No change planned.

%% gen_server callback : handle_call/3
-spec handle_call({fetch, name()}, from(), state()) -> {reply, fetch_ret(), state()}
               ; (stop, from(), state()) -> {stop, normal, ok, state()}
               .
%% Retrieves the value for the given parameter.
handle_call({fetch, Name}, _From, #st{parameters = Ps}=S) ->
  case orddict:find(Name, Ps) of
    error ->
      {reply, not_found, S};
    {ok, V} ->
      {reply, {ok, V}, S}
  end;
%% Stops the configuration server.
handle_call(stop, _From, State) ->
  {stop, normal, ok, State}.

%% gen_server callback : handle_cast/2
-spec handle_cast({store, name(), value()}, state()) -> {noreply, state()}.
handle_cast({store, K, V}, #st{parameters = Ps}=S) ->
  {noreply, S#st{parameters = orddict:store(K, V, Ps)}}.

%% gen_server callback : handle_info/2
-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(_What, State) ->
  {noreply, State}.
