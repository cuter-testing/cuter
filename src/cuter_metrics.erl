%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_metrics).
-behaviour(gen_server).

%% External API.
-export([define_distribution_metric/1, measure_distribution/2, get_distribution/1,
         start/0, stop/0]).
%% gen_server callbacks
-export([init/1, terminate/2, code_change/3, handle_info/2, handle_call/3, handle_cast/2]).

-export_type([distribution_list/0]).

-define(METRICS_SERVER, metrics_server).

-type from() :: {pid(), reference()}.
-type name() :: atom().

-type distribution() :: dict:dict(any(), non_neg_integer()).
-type distribution_list() :: [{any(), non_neg_integer()}].

%% The persistent state of the server.
-record(st, {
  %% The storage for the distribution metrics.
  distribution_metrics = dict:new() :: dict:dict(name(), distribution())
}).
-type state() :: #st{}.

%% ============================================================================
%% Eternal API
%% ============================================================================

%% Starts the metrics server.
-spec start() -> ok.
start() ->
  case gen_server:start_link({local, ?METRICS_SERVER}, ?MODULE, [], []) of
    {ok, _Pid} -> ok;
    {error, R} -> exit({metrics_start, R})
  end.

%% Stops the metrics server.
-spec stop() -> ok.
stop() ->
  gen_server:call(?METRICS_SERVER, stop).

%% Defines a new distribution metric.
-spec define_distribution_metric(name()) -> ok | eexist.
define_distribution_metric(Name) ->
  gen_server:call(?METRICS_SERVER, {new_distribution_metric, Name}).

%% Stores a measurement for a distribution metric.
-spec measure_distribution(name(), any()) -> ok | enoent.
measure_distribution(Name, Value) ->
  gen_server:call(?METRICS_SERVER, {measure_distribution, Name, Value}).

%% Gets a distribution metric as a list.
-spec get_distribution(name()) -> {ok, distribution_list()} | enoent.
get_distribution(Name) ->
  gen_server:call(?METRICS_SERVER, {get_distribution, Name}).

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
-spec handle_call({new_distribution_metric, name()}, from(), state()) -> {reply, ok | eexist, state()}
               ; ({measure_distribution, name(), any()}, from(), state()) -> {reply, ok | enoent, state()}
               ; ({get_distribution, name()}, from(), state()) -> {reply, {ok, distribution_list()} | enoent, state()}
               ; (stop, from(), state()) -> {stop, normal, ok, state()}
               .
%% Defines a new distribution metric.
handle_call({new_distribution_metric, Name}, _From, #st{distribution_metrics = Metrics}=S) ->
  case dict:is_key(Name, Metrics) of
    true ->
      {reply, eexist, S};
    false ->
      UpdatedMetrics = dict:store(Name, dict:new(), Metrics),
      {reply, ok, S#st{distribution_metrics = UpdatedMetrics}}
  end;
%% Stores a measurement for a distribution metric.
handle_call({measure_distribution, Name, Value}, _From, #st{distribution_metrics = Metrics}=S) ->
  case dict:find(Name, Metrics) of
    error ->
      {reply, enoent, S};
    {ok, Distribution} ->
      CurrN =
        case dict:find(Value, Distribution) of
          error -> 0;
          {ok, N} -> N
        end,
      NewDistribution = dict:store(Value, CurrN + 1, Distribution),
      UpdatedMetrics = dict:store(Name, NewDistribution, Metrics),
      {reply, ok, S#st{distribution_metrics = UpdatedMetrics}}
  end;
%% Gets a distribution metric as a list.
handle_call({get_distribution, Name}, _From, #st{distribution_metrics = Metrics}=S) ->
  case dict:find(Name, Metrics) of
    error ->
      {reply, enoent, S};
    {ok, Distribution} ->
      Repr = lists:reverse(lists:keysort(2, dict:to_list(Distribution))),
      {reply, {ok, Repr}, S}
  end;
%% Stops the metrics server.
handle_call(stop, _From, State) ->
  {stop, normal, ok, State}.

%% gen_server callback : handle_cast/2
-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Cast, State) ->
  {noreply, State}.

%% gen_server callback : handle_info/2
-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(_What, State) ->
  {noreply, State}.
