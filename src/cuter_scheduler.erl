%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_scheduler).
-behaviour(gen_server).

%% External exports
-export([start/2, stop/1, seed_execution/2, request_input/1,
         store_execution/3]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3, handle_info/2,
         handle_call/3, handle_cast/2]).



%% gen_server state datatype
-record(state, {
  queue,
  info,
  python,
  depth,
  waiting = none
}).
-type state() :: #state{}.

%% ============================================================================
%% External exports
%% ============================================================================

%% Start the Scheduler
-spec start(string(), integer()) -> pid().
start(Python, Depth) ->
  case gen_server:start_link(?MODULE, [Python, Depth], []) of
    {ok, Scheduler} -> Scheduler;
    {error, R} -> exit({scheduler_start, R})
  end.


%% Stop the Scheduler
-spec stop(pid()) -> ok.
stop(Scheduler) ->
  gen_server:cast(Scheduler, stop).

%% Store the information of the 1st concolic execution
%% (that will be used as a guide)
-spec seed_execution(pid(), map()) -> ok.
seed_execution(Scheduler, Info) ->
  gen_server:call(Scheduler, {seed_execution, Info}).

%% Request a new Input vertex for concolic execution
-spec request_input(pid()) -> {reference(), [any()]} | empty.
request_input(Scheduler) ->
  gen_server:call(Scheduler, request_input, 200000).

%% Store the information of a concolic execution
-spec store_execution(pid(), reference(), map()) -> ok.
store_execution(Scheduler, Ref, Info) ->
  gen_server:call(Scheduler, {store_execution, Ref, Info}).

%% ============================================================================
%% gen_server callbacks
%% ============================================================================

%% ------------------------------------------------------------------
%% gen_server callback : init/1
%% ------------------------------------------------------------------
-spec init([string() | integer(), ...]) -> {ok, state()}.

init([Python, Depth]) ->
  Q = queue:new(),
  I = ets:new(?MODULE, [ordered_set, protected]),
  {ok, #state{queue = Q, info = I, python = Python, depth = Depth}}.

%% ------------------------------------------------------------------
%% gen_server callback : terminate/2
%% ------------------------------------------------------------------
-spec terminate(term(), state()) -> ok.

terminate(_Reason, #state{info = I}) ->
  ets:delete(I),
  ok.

%% ------------------------------------------------------------------
%% gen_server callback : code_change/3
%% ------------------------------------------------------------------
-spec code_change(term(), state(), term()) -> {ok, state()}.
  
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.  %% No change planned.

%% ------------------------------------------------------------------
%% gen_server callback : handle_info/2
%% ------------------------------------------------------------------
-spec handle_info(term(), state()) -> {noreply, state()}.

handle_info(Msg, State) ->
  %% Just outputting unexpected messages for now
  io:format("[~s]: Unexpected message ~p~n", [?MODULE, Msg]),
  {noreply, State}.

%% ------------------------------------------------------------------
%% gen_server callback : handle_call/3
%% ------------------------------------------------------------------
-spec handle_call({seed_execution, map()}, {pid(), reference()}, state()) -> {reply, ok, state()}
               ; (request_input, {pid(), reference()}, state()) -> {reply, (ok | {reference(), [any()]}), state()} | {noreply, state()}
               ; ({store_execution, reference(), map()}, {pid(), reference()}, state()) -> {reply, ok, state}.

handle_call({seed_execution, Info}, _From, S=#state{queue = Q, info = I}) ->
  R = make_ref(),
  Q1 = queue:in(R, Q),
  ExecInfo = Info#{next_constraint => 1},
  io:format("[SCHED]: ~p~n", [ExecInfo]),
  ets:insert(I, {R, ExecInfo}),
  {reply, ok, S#state{queue = Q1}};

handle_call(request_input, From, S=#state{queue = Q, info = I, python = P, depth = D}) ->
  case generate_testcase(Q, I, P, D) of
    empty -> {reply, empty, S};
    none_currently_available -> {noreply, S#state{waiting = From}};
    {ok, Ref, Inp, Q1} -> {reply, {Ref, Inp}, S#state{queue = Q1}}
  end;

handle_call({store_execution, Ref, Info}, _From, S=#state{queue = Q, info = I, waiting = W}) ->
  [{Ref, PartialInfo}] = ets:lookup(I, Ref),
  Ln = maps:get(pathLength, Info),
  N = maps:get(next_constraint, PartialInfo),
  case N > Ln of
    true ->
      cuter_lib:clear_and_delete_dir(maps:get(dir, Info)),
      ets:delete(I, Ref),
      {reply, ok, S};
    false ->
      ExecInfo = Info#{next_constraint => N},
      ets:insert(I, {Ref, ExecInfo}),
      Q1 = queue:in(Ref, Q),
      %% Do sth for the waiting
      {reply, ok, S#state{queue = Q1}}
  end.

%% ------------------------------------------------------------------
%% gen_server callback : handle_cast/2
%% ------------------------------------------------------------------
-spec handle_cast(stop, state()) -> {stop, normal, state()}.

handle_cast(stop, State) ->
  {stop, normal, State}.



is_info_empty(Info) ->
  case ets:first(Info) of
    '$end_of_table' -> true;
    _ -> false
  end.

generate_testcase(Q, Info, Python, Depth) ->
  case queue:out(Q) of
    {empty, Q} ->
      case is_info_empty(Info) of
        true  -> empty;
        false -> none_currently_available
      end;
    {{value, R}, Q1} ->
      case expand_state(Q1, R, Info, Python, Depth) of
        {error, Q2} -> generate_testcase(Q2, Info, Python, Depth);
        {ok, Ref, Inp, Q2} -> {ok, Ref, Inp, Q2}
      end
  end.

expand_state(Q, Ref, I, Python, Depth) ->
  [{Ref, Info}] = ets:lookup(I, Ref),
  ets:delete(I, Ref),
  F = maps:get(traceFile, Info),
  N = maps:get(next_constraint, Info),
  case cuter_solver:run(Python, maps:get(mappings, Info), F, N) of
    error ->
      Q1 = requeue_state(Q, Ref, I, Info, Depth),
      {error, Q1};
    Inp ->
      R = make_ref(),
      ets:insert(I, {R, #{next_constraint => N+1}}),
      Q1 = requeue_state(Q, Ref, I, Info, Depth),
      {ok, R, Inp, Q1}
  end.


requeue_state(Q, Ref, I, Info, Depth) ->
  N = maps:get(next_constraint, Info),
  L = maps:get(path_length, Info),
  X = N + 1,
  case X > L orelse X > Depth of
    true ->
      DataDir = maps:get(dir, Info),
      cuter_lib:clear_and_delete_dir(DataDir),
      Q;
    false ->
      ets:insert(I, {Ref, Info#{next_constraint := X}}),
      queue:in(Ref, Q)
  end.
  
