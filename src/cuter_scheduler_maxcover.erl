%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_scheduler_maxcover).
-behaviour(gen_server).

-export([ %% external API
          start/3
        , stop/1
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

-export_type([exec_handle/0]).

-type item() :: {boolean(), integer(), cuter_cerl:tagID(), exec_handle()}.
-type inp_queue_item() :: {integer(), cuter:input()}.
-type inp_queue() :: queue:queue(inp_queue_item()).
-type exec_handle() :: nonempty_string().
-type exec_info() :: #{ traceFile => file:name()
		      , dataDir => file:filename_all()
		      , mappings => [cuter_symbolic:mapping()]
		      , input => undefined}.  %% TODO Populate this field
-type exec_info_tab() :: dict:dict(exec_handle(), exec_info()).

%% gen_server state datatype
-record(sts, {
    tags_queue         :: cuter_minheap:minheap()
  , inputs_queue       :: inp_queue()
  , info_tab           :: exec_info_tab()
  , python             :: string()
  , depth              :: integer()
  , stored_mods        :: cuter_analyzer:stored_modules()
  , tags_added_no = 0  :: integer()
  , visited_tags       :: cuter_analyzer:visited_tags()
  , running            :: dict:dict(exec_handle(), cuter:input())
  , first_operation    :: dict:dict(exec_handle(), integer())
  , erroneous          :: cuter:erroneous_inputs()
}).
-type state() :: #sts{}.
-type from()  :: {pid(), reference()}.


%% ============================================================================
%% External API
%% ============================================================================

%% Start the Scheduler
-spec start(string(), integer(), cuter:input()) -> pid().
start(Python, Depth, SeedInput) ->
  case gen_server:start_link(?MODULE, [Python, Depth, SeedInput], []) of
    {ok, Scheduler} -> Scheduler;
    {error, R} -> exit({scheduler_start, R})
  end.

%% Stop the Scheduler
-spec stop(pid()) -> cuter:erroneous_inputs().
stop(Scheduler) ->
  gen_server:call(Scheduler, stop).

%% Request a new Input vertex for concolic execution
-spec request_input(pid()) -> {exec_handle(), cuter:input(), cuter_analyzer:stored_modules(), integer()} | empty.
request_input(Scheduler) ->
  gen_server:call(Scheduler, request_input, infinity).

%% Store the information of a concolic execution
-spec store_execution(pid(), exec_handle(), cuter_analyzer:info()) -> ok.
store_execution(Scheduler, Handle, Info) ->
  gen_server:call(Scheduler, {store_execution, Handle, Info}).

%% ============================================================================
%% gen_server callbacks
%% ============================================================================

%% init/1
-spec init([string() | integer() | cuter:input(), ...]) -> {ok, state()}.
init([Python, Depth, SeedInput]) ->
  _ = set_execution_counter(0),
  TagsQueue = cuter_minheap:new(fun erlang:'<'/2),
  InpQueue = queue:in({1, SeedInput}, queue:new()),
  {ok, #sts{info_tab = dict:new(), python = Python, depth = Depth, visited_tags = gb_sets:new(),
            stored_mods = orddict:new(), running = dict:new(), first_operation = dict:new(),
            erroneous = [], inputs_queue = InpQueue, tags_queue = TagsQueue}}.

%% terminate/2
-spec terminate(any(), state()) -> ok.
terminate(_Reason, #sts{tags_queue = TQ}) ->
  cuter_minheap:delete(TQ),
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
-spec handle_call(request_input, from(), state()) -> {reply, (empty | {exec_handle(), cuter:input(), cuter_analyzer:stored_modules(), integer()}), state()}
               ; ({store_execution, exec_handle(), cuter_analyzer:info()}, from(), state()) -> {reply, ok, state()}
               ; (stop, from(), state()) -> {stop, normal, cuter:erroneous_inputs(), state()}.

%% Ask for a new input to execute
handle_call(request_input, _From, S=#sts{tags_queue = TQ, info_tab = AllInfo, python = P, stored_mods = SMs, visited_tags = Vs, tags_added_no = TagsN,
                                         running = Rn, first_operation = FOp, inputs_queue = IQ}) ->
  case find_new_input(TQ, IQ, P, AllInfo, Vs) of
    empty -> {reply, empty, S};
    {ok, Handle, Input, NAllowed, Rem} ->
      {reply, {Handle, Input, SMs, TagsN}, S#sts{ running = dict:store(Handle, Input, Rn)
                                                , first_operation = dict:store(Handle, NAllowed, FOp)
                                                , inputs_queue = Rem}}
  end;

%% Store the information of a concolic execution
handle_call({store_execution, Handle, Info}, _From, S=#sts{tags_queue = TQ, info_tab = AllInfo, stored_mods = SMs, visited_tags = Vs, depth = Depth,
                                                           running = Rn, first_operation = FOp, erroneous = Err}) ->
  %% Generate the information of the execution
  I = #{ traceFile => maps:get(traceFile, Info)
       , dataDir => maps:get(dir, Info)
       , mappings => maps:get(mappings, Info)},
  %% Get the input & update the erroneous inputs
  Input = dict:fetch(Handle, Rn),
  NErr = update_erroneous(maps:get(runtime_error, Info), Input, Err),
  %% Update the stored code of modules
  StoredMods = orddict:merge(fun(_K, V1, _V2) -> V1 end, SMs, maps:get(stored_mods, Info)),
  %% Update the visited tags
  Visited = gb_sets:union(Vs, maps:get(tags, Info)),
  %% Update the queue
  N = dict:fetch(Handle, FOp),
  Rvs = maps:get(reversible, Info),
  Items = generate_queue_items(Rvs, Handle, Visited, N, Depth),
  lists:foreach(fun(Item) -> cuter_minheap:insert(Item, TQ) end, Items),
  {reply, ok, S#sts{ info_tab = dict:store(Handle, I, AllInfo)
                   , stored_mods = StoredMods
                   , tags_added_no = maps:get(tags_added_no, Info)  %% Number of added tags
                   , visited_tags = Visited
                   , running = dict:erase(Handle, Rn)  %% Remove the handle from the running set
                   , first_operation = dict:erase(Handle, FOp)
                   , erroneous = NErr}};

%% Stops the server.
handle_call(stop, _From, S=#sts{erroneous = Err}) ->
  {stop, normal, lists:reverse(Err), S}.

%% handle_cast/2
-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(Msg, State) ->
  %% Just outputting unexpected messages for now
  io:format("[~s]: Unexpected message ~p~n", [?MODULE, Msg]),
  {noreply, State}.


%% ============================================================================
%% Generate new input
%% ============================================================================

-spec find_new_input(cuter_minheap:minheap(), inp_queue(), string(), exec_info_tab(), cuter_analyzer:visited_tags()) ->
        {ok, exec_handle(), cuter:input(), integer(), inp_queue()} | empty.
find_new_input(TagsQueue, InpQueue, Python, Info, Visited) ->
  case queue:out(InpQueue) of
    %% Search in the inputs queue.
    {{value, {N, Input}}, RemInpQueue} ->
      H = fresh_execution_handle(),
      {ok, H, Input, N, RemInpQueue};
    %% Reverse a constraint and generate a new input.
    {empty, InpQueue} ->
      case generate_new_input(TagsQueue, Python, Info, Visited) of
        empty -> empty;
        {ok, H, Input, N} -> {ok, H, Input, N, InpQueue}
      end
  end.

-spec generate_new_input(cuter_minheap:minheap(), string(), exec_info_tab(), cuter_analyzer:visited_tags()) ->
        {ok, exec_handle(), cuter:input(), integer()} | empty.
generate_new_input(Queue, Python, Info, Visited) ->
  case locate_next_reversible(Queue, Visited) of
    empty -> empty;
    {ok, N, Handle} ->
      I = dict:fetch(Handle, Info),
      File = maps:get(traceFile, I),
      Ms = maps:get(mappings, I),
      %% io:format("[sched] Attempting ~w in ~p~n", [N, File]),
      case cuter_solver:solve(Python, Ms, File, N) of
        error ->
          cuter_pp:solving_failed_notify(),
          generate_new_input(Queue, Python, Info, Visited);
        {ok, Input} ->
          H = fresh_execution_handle(),
          {ok, H, Input, N+1}
      end
  end.

-spec locate_next_reversible(cuter_minheap:minheap(), cuter_analyzer:visited_tags()) -> {ok, integer(), exec_handle()} | empty.
locate_next_reversible(Queue, Visited) ->
  locate_next_reversible(Queue, Visited, cuter_minheap:heap_size(Queue)).

-spec locate_next_reversible(cuter_minheap:minheap(), cuter_analyzer:visited_tags(), integer()) -> {ok, integer(), exec_handle()} | empty.
locate_next_reversible(Queue, Visited, M) ->
  case cuter_minheap:take_min(Queue) of
    {error, empty_heap} -> empty;
    {true, N, _TagID, Handle} -> {ok, N, Handle};
    {false, N, TagID, Handle} ->
      %% Verify that the tag is not visited
      case gb_sets:is_element(TagID, Visited) of
        false -> {ok, N, Handle};
        true  ->
          case M of
            0 -> {ok, N, Handle};  %% Have seen all the entries at least once
            _ ->
              cuter_minheap:insert({true, N, TagID, Handle}, Queue),
              locate_next_reversible(Queue, Visited, M-1)
          end
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

-spec generate_queue_items(cuter_analyzer:reversible_with_tags(), exec_handle(), cuter_analyzer:visited_tags(), integer(), integer()) -> [item()].
generate_queue_items(Rvs, Handle, Visited, N, Depth) ->
  generate_queue_items(Rvs, Handle, Visited, N, Depth, []).

-spec generate_queue_items(cuter_analyzer:reversible_with_tags(), exec_handle(), cuter_analyzer:visited_tags(), integer(), integer(), [item()]) -> [item()].
generate_queue_items([], _Handle, _Visited, _N, _Depth, Acc) ->
  lists:reverse(Acc);
generate_queue_items([R|Rs], Handle, Visited, N, Depth, Acc) ->
  case maybe_item(R, Handle, Visited, N, Depth) of
    false -> generate_queue_items(Rs, Handle, Visited, N, Depth, Acc);
    {ok, Item} -> generate_queue_items(Rs, Handle, Visited, N, Depth, [Item|Acc])
  end.

-spec maybe_item(cuter_analyzer:reversible_with_tag(), exec_handle(), cuter_analyzer:visited_tags(), integer(), integer()) -> {ok, item()} | false.
maybe_item({Id, TagID}, Handle, Visited, N, Depth) ->
  case Id > Depth orelse Id < N of
    true  -> false;
    false -> {ok, {gb_sets:is_element(TagID, Visited), Id, TagID, Handle}}
  end.

%% ============================================================================
%% Erroenous inputs
%% ============================================================================

-spec update_erroneous(boolean(), cuter:input(), cuter:erroneous_inputs()) -> cuter:erroneous_inputs().
update_erroneous(false, _Input, Erroneous) -> Erroneous;
update_erroneous(true, Input, Erroneous) -> [Input | Erroneous].
