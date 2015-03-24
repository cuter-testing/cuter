-module(cuter_scheduler_maxcover).
-behaviour(gen_server).

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

-include("include/cuter_macros.hrl").

-type input() :: [any()].
-type item()  :: {integer(), cuter_cerl:tagID(), exec_handle()}.
-type exec_handle() :: string().
-type exec_info() :: dict:dict(exec_handle(), #{ traceFile => file:name()
                                               , dataDir => file:filename_all()
                                               , mappings => [cuter_symbolic:mapping()]
                                               , input => undefined}).  %% TODO Populate this field
%% gen_server state datatype
-record(sts, {
    queue              :: queue:queue(item())
  , info               :: exec_info()
  , python             :: string()
  , depth              :: integer()
  , stored_mods = none :: cuter_analyzer:stored_modules() | none
  , tags_added_no = 0  :: integer()
  , visited_tags       :: cuter_analyzer:visited_tags()
  , running            :: dict:dict(exec_handle(), input())
  , erroneous          :: orddict:orddict()  %% TODO Populate this field
}).
-type state() :: #sts{}.
-type from()  :: {pid(), reference()}.



%% ============================================================================
%% External API
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
-spec seed_execution(pid(), cuter_analyzer:info()) -> ok.
seed_execution(Scheduler, Info) ->
  gen_server:call(Scheduler, {seed_execution, Info}).

%% Request a new Input vertex for concolic execution
-spec request_input(pid()) -> {exec_handle(), [any()], cuter_analyzer:stored_modules(), integer()} | empty.
request_input(Scheduler) ->
  gen_server:call(Scheduler, request_input, 500000).

%% Store the information of a concolic execution
-spec store_execution(pid(), exec_handle(), cuter_analyzer:info()) -> ok.
store_execution(Scheduler, Handle, Info) ->
  gen_server:call(Scheduler, {store_execution, Handle, Info}).

%% ============================================================================
%% gen_server callbacks
%% ============================================================================

%% init/1
-spec init([string() | integer(), ...]) -> {ok, state()}.
init([Python, Depth]) ->
  _ = set_execution_counter(0),
  {ok, #sts{queue = queue:new(), info = dict:new(), python = Python,
            depth = Depth, visited_tags = gb_sets:new(), running = dict:new()}}.

%% terminate/2
-spec terminate(any(), state()) -> ok.
terminate(_Reason, #sts{erroneous = _Err}) ->
%  cuter_pp:report_erroneous(Err),
  %% TODO clear dirs
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
               ; (request_input, from(), state()) -> {reply, (empty | {exec_handle(), input(), cuter_analyzer:stored_modules(), integer()}), state()}
               ; ({store_execution, exec_handle(), cuter_analyzer:info()}, from(), state()) -> {reply, ok, state}.
%% Store the information of the first concolic execution
handle_call({seed_execution, Info}, _From, S=#sts{info = AllInfo, depth = Depth}) ->
  Handle = fresh_execution_handle(),  %% A handle for an execution
%  io:format("[sched] Init Exec ~p~n", [Handle]),
  %% Generate the information of the execution
  I = #{ traceFile => maps:get(traceFile, Info)
       , dataDir => maps:get(dir, Info)
       , mappings => maps:get(mappings, Info)},
  %% Initialize the queue
  Rvs = maps:get(reversible, Info),
  Visited = maps:get(tags, Info),
  Items = generate_queue_items(Rvs, Handle, Visited, Depth),
%  io:format("[sched] Init Visited ~p~n", [gb_sets:to_list(Visited)]),
%  io:format("[sched] Init Queue ~p~n", [queue:from_list(Items)]),
  {reply, ok, S#sts{ queue = queue:from_list(Items)
                   , info = dict:store(Handle, I, AllInfo)
                   , stored_mods = maps:get(stored_mods, Info)      %% Code of loaded modules
                   , tags_added_no = maps:get(tags_added_no, Info)  %% Number of added tags
                   , visited_tags = Visited}};
%% Ask for a new input to execute
handle_call(request_input, _From, S=#sts{queue = Q, info = AllInfo, python = P, stored_mods = SMs, visited_tags = Vs, tags_added_no = TagsN, running = Rn}) ->
  case generate_new_input(Q, P, AllInfo, Vs) of
    empty -> {reply, empty, S};
    {ok, Handle, Input, Q1} ->
      {reply, {Handle, Input, SMs, TagsN}, S#sts{ queue = Q1
                                                , running = dict:store(Handle, Input, Rn)}}
  end;
%% Store the information of a concolic execution
handle_call({store_execution, Handle, Info}, _From, S=#sts{queue = Queue, info = AllInfo, stored_mods = SMs, visited_tags = Vs, depth = Depth, running = Rn}) ->
  %% Generate the information of the execution
  I = #{ traceFile => maps:get(traceFile, Info)
       , dataDir => maps:get(dir, Info)
       , mappings => maps:get(mappings, Info)},
  %% Update the stored code of modules
  StoredMods = orddict:merge(fun(_K, V1, _V2) -> V1 end, SMs, maps:get(stored_mods, Info)),
  %% Update the visited tags
  Visited = gb_sets:union(Vs, maps:get(tags, Info)),
  %% Update the queue
  Rvs = maps:get(reversible, Info),
  Items = generate_queue_items(Rvs, Handle, Visited, Depth),
  {reply, ok, S#sts{ queue = queue:join(Queue, queue:from_list(Items))
                   , info = dict:store(Handle, I, AllInfo)
                   , stored_mods = StoredMods
                   , tags_added_no = maps:get(tags_added_no, Info)  %% Number of added tags
                   , visited_tags = Visited
                   , running = dict:erase(Handle, Rn)}}.  %% Remove the handle from the running set

%% handle_cast/2
-spec handle_cast(stop, state()) -> {stop, normal, state()}.
handle_cast(stop, State) ->
  {stop, normal, State}.


%% ============================================================================
%% Generate new input
%% ============================================================================

-spec generate_new_input(queue:queue(item()), string(), exec_info(), cuter_analyzer:visited_tags()) ->
        {ok, exec_handle(), input(), queue:queue(item())} | empty.
generate_new_input(Queue, Python, Info, Visited) ->
  case locate_next_reversible(Queue, Visited) of
    empty -> empty;
    {ok, N, Handle, Queue1} ->
      I = dict:fetch(Handle, Info),
      File = maps:get(traceFile, I),
      Ms = maps:get(mappings, I),
%      io:format("[sched] Attempting ~w in ~p~n", [N, File]),
      case cuter_solver:solve(Python, Ms, File, N) of
        error ->
          io:format(".~n"),
          generate_new_input(Queue1, Python, Info, Visited);
        {ok, Input} ->
          H = fresh_execution_handle(),
          {ok, H, Input, Queue1}
      end
  end.

-spec locate_next_reversible(queue:queue(item()), cuter_analyzer:visited_tags()) ->
        {ok, integer(), exec_handle(), queue:queue(item())} | empty.
locate_next_reversible(Queue, Visited) ->
  case queue:out(Queue) of
    {empty, Queue} -> empty;
    {{value, {N, TagID, Handle}}, Queue1} ->
      case gb_sets:is_member(TagID, Visited) of
        true  -> locate_next_reversible(Queue1, Visited);
        false -> {ok, N, Handle, Queue1}
      end
  end.

%% ============================================================================
%% Generate handles for executions
%% ============================================================================

-spec set_execution_counter(integer()) -> integer() | undefined.
set_execution_counter(N) ->
  put(?EXECUTION_COUNTER_PREFIX, N).

-spec fresh_execution_handle() -> exec_handle().
fresh_execution_handle() ->
  N = get(?EXECUTION_COUNTER_PREFIX) + 1,
  put(?EXECUTION_COUNTER_PREFIX, N),
  "exec" ++ integer_to_list(N).

%% ============================================================================
%% Functions for tags
%% ============================================================================

-spec generate_queue_items(cuter_analyzer:reversible_with_tags(), exec_handle(), cuter_analyzer:visited_tags(), integer()) -> [item()].
generate_queue_items(Rvs, Handle, Visited, Depth) ->
  generate_queue_items(Rvs, Handle, Visited, Depth, []).

-spec generate_queue_items(cuter_analyzer:reversible_with_tags(), exec_handle(), cuter_analyzer:visited_tags(), integer(), [item()]) -> [item()].
generate_queue_items([], _Handle, _Visited, _Depth, Acc) ->
  lists:reverse(Acc);
generate_queue_items([R|Rs], Handle, Visited, Depth, Acc) ->
  case maybe_item(R, Handle, Visited, Depth) of
    false -> generate_queue_items(Rs, Handle, Visited, Depth, Acc);
    {ok, Item} -> generate_queue_items(Rs, Handle, Visited, Depth, [Item|Acc])
  end.

-spec maybe_item(cuter_analyzer:reversible_with_tag(), exec_handle(), cuter_analyzer:visited_tags(), integer()) -> {ok, item()} | false.
maybe_item({N, TagID}, Handle, Visited, Depth) ->
  case N > Depth orelse gb_sets:is_member(TagID, Visited) of
    true  -> false;
    false -> {ok, {N, TagID, Handle}}
  end.



