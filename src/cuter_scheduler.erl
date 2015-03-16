%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_scheduler).
-behaviour(gen_server).

%% External exports
-export([ %% external API
          start/2
        , stop/1
        , seed_execution/2
        , request_input/1
        , store_execution/3
        %% gen_server callbacks
        , init/1
        , terminate/2
        , code_change/3
        , handle_info/2
        , handle_call/3
        , handle_cast/2
       ]).

%% gen_server state datatype
-record(sts, {
    queue           :: queue:queue()
  , info            :: ets:tid()
  , python          :: string()
  , depth           :: integer()
  , waiting = none  :: from() | none
}).
-type state() :: #sts{}.
-type from()  :: {pid(), reference()}.

%% ----------------------------------------------------------------------------
%% External API
%% ----------------------------------------------------------------------------

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
-spec seed_execution(pid(), cuter_analyzer:info()) -> ok.
seed_execution(Scheduler, Info) ->
  gen_server:call(Scheduler, {seed_execution, Info}).

%% Request a new Input vertex for concolic execution
-spec request_input(pid()) -> {reference(), [any()]} | empty.
request_input(Scheduler) ->
  gen_server:call(Scheduler, request_input, 500000).

%% Store the information of a concolic execution
-spec store_execution(pid(), reference(), cuter_analyzer:info()) -> ok.
store_execution(Scheduler, Ref, Info) ->
  gen_server:call(Scheduler, {store_execution, Ref, Info}).

%% ----------------------------------------------------------------------------
%% gen_server callbacks
%% ----------------------------------------------------------------------------

%% init/1
-spec init([string() | integer(), ...]) -> {ok, state()}.
init([Python, Depth]) ->
  Q = queue:new(),
  I = ets:new(?MODULE, [ordered_set, protected]),
  {ok, #sts{queue = Q, info = I, python = Python, depth = Depth}}.

%% terminate/2
-spec terminate(any(), state()) -> ok.
terminate(_Reason, #sts{info = I}) ->
  ets:delete(I),
  ok.

%% code_change/3
-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.  % No change planned.

%% handle_info/2
-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(Msg, State) ->
  %% Just outputting unexpected messages for now
  io:format("[~s]: Unexpected message ~p~n", [?MODULE, Msg]),
  {noreply, State}.

%% handle_call/3
-spec handle_call({seed_execution, cuter_analyzer:info()}, from(), state()) -> {reply, ok, state()}
               ; (request_input, from(), state()) -> {reply, (empty | {reference(), [any()]}), state()}
               ; ({store_execution, reference(), cuter_analyzer:info()}, from(), state()) -> {reply, ok, state}.
%% Store the information of the first concolic execution
handle_call({seed_execution, Info}, _From, S=#sts{queue = Q, info = I}) ->
  Rf = make_ref(),
  Q1 = queue:in(Rf, Q),
  ExecInfo = Info#{nextRvs => 1},  % Add the next command to be reversed
  ets:insert(I, {Rf, ExecInfo}),   % Store the extended info
  cuter_pp:seed_execution(Rf, ExecInfo),
  {reply, ok, S#sts{queue = Q1}};
%% Ask for a new input to execute
handle_call(request_input, _From, S=#sts{queue = Q, info = I, python = P, depth = D}) ->
  cuter_pp:request_input(Q, I),
  case generate_new_testcase(Q, I, P, D) of
    empty ->
      cuter_pp:request_input_empty(I),
      {reply, empty, S};
    {ok, Rf, Inp, Q1} ->
      cuter_pp:request_input_success(Rf, Inp, Q1, I),
      {reply, {Rf, Inp}, S#sts{queue = Q1}}
  end;
%% Store the information of a concolic execution
handle_call({store_execution, Rf, Info}, _From, S=#sts{queue = Q, info = I, waiting = W}) ->
  cuter_pp:store_execution(Rf, Info, Q, I),
  [{Rf, PartialInfo}] = ets:lookup(I, Rf),
  Ln = maps:get(pathLength, Info),
  N = maps:get(nextRvs, PartialInfo),
  case N > Ln of  % Store the execution if there is a command to be reversed later
    true ->
      cuter_lib:clear_and_delete_dir(maps:get(dir, Info)),
      ets:delete(I, Rf),
      cuter_pp:store_execution_fail(N, Ln, I),
      {reply, ok, S};
    false ->
      ExecInfo = Info#{nextRvs => N},
      ets:insert(I, {Rf, ExecInfo}),  % Replaces the old information
      Q1 = queue:in(Rf, Q),
      cuter_pp:store_execution_success(N, Ln, Q, I),
      {reply, ok, S#sts{queue = Q1}}
  end.

%% handle_cast/2
-spec handle_cast(stop, state()) -> {stop, normal, state()}.
handle_cast(stop, State) ->
  {stop, normal, State}.

%% ----------------------------------------------------------------------------
%% Attempt to generate a new testcase
%% ----------------------------------------------------------------------------

%% Generate a new testcase from reversing a command in the trace
%% of the execution that is in the head of the queue
-spec generate_new_testcase(queue:queue(), ets:tid(), string(), integer()) -> {ok, reference(), [any()], queue:queue()} | empty.
generate_new_testcase(Q, Info, Python, Depth) ->
  case queue:out(Q) of
    {empty, Q} ->
      empty;
    {{value, Rf}, Q1} ->
      expand_state(Q1, Rf, Info, Python, Depth)
  end.

%% Attempt to reverse the Nth command in a stored trace
-spec expand_state(queue:queue(), reference(), ets:tid(), string(), integer()) -> {ok, reference(), [any()], queue:queue()} | empty.
expand_state(Q, Rf, I, Python, Depth) ->
  [{Rf, Info}] = ets:lookup(I, Rf),
  ets:delete(I, Rf),
  Fname = maps:get(traceFile, Info),
  Mapping = maps:get(mappings, Info),
  N = maps:get(nextRvs, Info),
  case cuter_solver:solve(Python, Mapping, Fname, N) of
    error ->
      io:format(".\n"),
      Q1 = requeue_state(Q, Rf, Info, I, Depth),
      generate_new_testcase(Q1, I, Python, Depth);
    {ok, Inp} ->
      R = make_ref(),
      PartialInfo = #{nextRvs => N+1},
      ets:insert(I, {R, PartialInfo}),
      Q1 = requeue_state(Q, Rf, Info, I, Depth),
      {ok, R, Inp, Q1}
  end.

%% Try to requeue an execution if there is another
%% reversible command in its trace
-spec requeue_state(queue:queue(), reference(), map(), ets:tid(), integer()) -> queue:queue().
requeue_state(Q, Rf, Info, I, Depth) ->
  N = maps:get(nextRvs, Info),
  L = maps:get(pathLength, Info),
  X = N + 1,
  case X > L orelse X > Depth of
    true ->
      DataDir = maps:get(dir, Info),
      cuter_lib:clear_and_delete_dir(DataDir),
      Q;
    false ->
      ets:insert(I, {Rf, Info#{nextRvs := X}}),  % Replace the old information
      queue:in(Rf, Q)
  end.

