%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_merger).

-include("cuter_macros.hrl").

-export([merge_traces/2, validate_file/1]).

-export_type([goal/0]).

-record(sts, {
  currFd       :: file:io_device(),
  openFds      :: ets:tab(),
  pendingFiles :: [file:filename_all()],
  waitingFds   :: queue:queue(),
  logFd        :: file:io_device(),
  seenRefs     :: ets:tab(),
  goalRef      :: goal() | none,
  dirs         :: [cuter_analyzer:node_trace()]
}).
-type state() :: #sts{}.
-type goal()  :: {integer(), string()}.

-type validation() :: ok | {error, binary()}.
-type known_set()  :: gb_sets:set().

%% Merge the traces of an execution into one file.
-spec merge_traces(cuter_analyzer:raw_info(), file:name()) -> ok.
merge_traces(Info, MergedLogFile) ->
  Dirs = maps:get(traces, Info),
  Int = maps:get(int, Info), 
  Init = initialize(Dirs, Int, MergedLogFile),
  merge(Init).


-spec initialize([cuter_analyzer:node_trace()], cuter_analyzer:int_process(), file:name()) -> state().
initialize(Dirs, {INode, IPid}, MergedLogFile) ->
  F = logfile_from_pid(INode, IPid, Dirs),
  Pending = all_logfiles(Dirs, []) -- [F],
  {ok, Fd} = cuter_log:open_file(F, read),
  Open = ets:new(?MODULE, [set, protected]),
  ets:insert(Open, {F, Fd}),
  ets:insert(Open, {Fd, F}),
  {ok, LogFd} = cuter_log:open_file(MergedLogFile, write),
  #sts{
    currFd = Fd,
    openFds = Open,
    pendingFiles = Pending,
    waitingFds = queue:new(),
    logFd = LogFd,
    seenRefs = ets:new(?MODULE, [set, protected]),
    goalRef = none,
    dirs = Dirs
  }.

%% Get the name of a process's log file
-spec logfile_from_pid(node(), pid(), [cuter_analyzer:node_trace()]) -> file:name().
logfile_from_pid(Node, Pid, Dirs) ->
  Dir = proplists:get_value(Node, Dirs),
  cuter_lib:logfile_name(Dir, Pid).

-spec all_logfiles([cuter_analyzer:node_trace()], [file:filename_all()]) -> [file:filename_all()].
all_logfiles([], Acc) -> Acc;
all_logfiles([{_Node, Path}|Rest], Acc) ->
  Fs = cuter_lib:list_dir(Path),
  all_logfiles(Rest, Acc ++ Fs).


-spec merge(state()) -> ok.
merge(State=#sts{currFd = Fd, logFd = LogFd, openFds = Open, seenRefs = Seen}) ->
  case cuter_log:next_entry(Fd, true) of
    %% Reached the end of the file
    eof ->
      [{Fd, File}] = ets:lookup(Open, Fd),
      cuter_pp:file_finished(File),
      ets:delete(Open, Fd),
      ets:delete(Open, File),
      merge_next_file(State#sts{currFd = undefined});
    %% SPAWN
    {?NOT_CONSTRAINT, ?OP_SPAWN, Data} ->
      {?OP_SPAWN, [_ChildNode, _ChildPid, Rf]} = cuter_json:json_to_command(Data),
      Reached = {?OP_SPAWN, Rf},
      ets:insert(Seen, {Rf, ?OP_SPAWN}),
      check_for_goal(Reached, State);
    %% SPAWNED
    {?NOT_CONSTRAINT, ?OP_SPAWNED, Data} ->
      {?OP_SPAWNED, [ParentNode, ParentPid, Rf]} = cuter_json:json_to_command(Data),
      Goal = {?OP_SPAWN, Rf},
      cuter_pp:set_goal(Goal, "SPAWNED"),
      achieve_goal(Goal, ParentNode, ParentPid, State);
    %% MSG SEND
    {?NOT_CONSTRAINT, ?OP_MSG_SEND, Data} ->
      {?OP_MSG_SEND, [_DestNode, _DestPid, Rf]} = cuter_json:json_to_command(Data),
      Reached = {?OP_MSG_SEND, Rf},
      ets:insert(Seen, {Rf, ?OP_MSG_SEND}),
      check_for_goal(Reached, State);
    %% MSG RECEIVE
    {?NOT_CONSTRAINT, ?OP_MSG_RECEIVE, Data} ->
      {?OP_MSG_RECEIVE, [FromNode, FromPid, Rf]} = cuter_json:json_to_command(Data),
      Goal = {?OP_MSG_SEND, Rf},
      cuter_pp:set_goal(Goal, "MSG RCV"),
      achieve_goal(Goal, FromNode, FromPid, State);
    %% MSG CONSUME
    {?NOT_CONSTRAINT, ?OP_MSG_CONSUME, Data} ->
      {?OP_MSG_CONSUME, [_FromNode, _FromPid, Rf]} = cuter_json:json_to_command(Data),
      cuter_pp:consume_msg(Rf),
      ets:delete(Seen, Rf),
      merge(State);
    %% Any Command
    {Kind, Type, Data} ->
      cuter_log:write_data(LogFd, Kind, Type, Data),
      merge(State)
  end.

%%
%% Achieve a new goal (SPAWNED or MSG_RECEIVED)
%%

-spec achieve_goal(goal(), node(), pid(), state()) -> ok.
achieve_goal(Goal={OpCode, Rf}, Node, Pid, State=#sts{seenRefs = Seen, dirs = Dirs}) ->
  case ets:lookup(Seen, Rf) of
    %% The goal has already been achieved
    [{Rf, OpCode}] ->
      cuter_pp:goal_already_achieved(Goal),
      case OpCode of
        ?OP_SPAWN -> ets:delete(Seen, Rf);
        ?OP_MSG_SEND -> ok
      end,
      merge(State);
    %% Must achieve the new goal
    [] ->
      F = logfile_from_pid(Node, Pid, Dirs),
      cuter_pp:search_goal_in_file(F),
      achieve_goal_from_file(Goal, F, State)
  end.

-spec achieve_goal_from_file(goal(), file:filename_all(), state()) -> ok.
achieve_goal_from_file(NewGoal, F, State=#sts{currFd = Fd, openFds = Open, waitingFds = Waiting, pendingFiles = Pending, goalRef = Goal}) ->
  Q = queue:in_r({Goal, Fd}, Waiting),
  case ets:lookup(Open, F) of
    [{F, NextFd}] ->
      cuter_pp:change_to_file(F),
      merge(State#sts{currFd = NextFd, waitingFds = Q, goalRef = NewGoal});
    [] ->
      {ok, NewFd} = cuter_log:open_file(F, read),
      Fs = Pending -- [F],
      cuter_pp:open_file(F, Fs),
      ets:insert(Open, {F, NewFd}),
      ets:insert(Open, {NewFd, F}),
      merge(State#sts{currFd = NewFd, waitingFds = Q, pendingFiles = Fs, goalRef = NewGoal})
  end.


%%
%% Check if the goal is achieved (SPAWN or MSG_SEND)
%%

-spec check_for_goal(goal(), state()) -> ok.
%% No goal
check_for_goal(_Goal, State=#sts{goalRef = none}) ->
  merge(State);
%% Achieved goal
check_for_goal(Goal, State=#sts{currFd = Fd, waitingFds = Waiting, goalRef = Goal}) ->
  %% Get the file that set the goal
  {{value, {NextGoal, NextFd}}, Q1} = queue:out(Waiting),
  %% Add the current file to the waiting queue without a goal
  Q2 = queue:in({none, Fd}, Q1),
  %% start merging the next file
  cuter_pp:achieve_goal(Goal, NextGoal),
  merge(State#sts{currFd = NextFd, waitingFds = Q2, goalRef = NextGoal});
%% Did not achieve goal
check_for_goal(_Goal, State) ->
  merge(State).

%%
%% Reached the end of a file
%%

-spec merge_next_file(state()) -> ok.
merge_next_file(State=#sts{waitingFds = Waiting}) ->
  case queue:is_empty(Waiting) of
    true  -> merge_next_file_from_pending(State);
    false -> merge_next_file_from_waiting(State)
  end.

-spec merge_next_file_from_pending(state()) -> ok.
%% All files are successfully merged
merge_next_file_from_pending(#sts{openFds = Open, pendingFiles = [], seenRefs = Seen, logFd = LogFd}) ->
  ets:delete(Open),
  ets:delete(Seen),
  cuter_log:close_file(LogFd);
%% There are pending files to be merged
merge_next_file_from_pending(State=#sts{openFds = Open, pendingFiles = [F|Rest]}) ->
  cuter_pp:open_pending_file(F),
  {ok, Fd} = cuter_log:open_file(F, read),
  ets:insert(Open, {F, Fd}),
  ets:insert(Open, {Fd, F}),
  merge(State#sts{currFd = Fd, pendingFiles = Rest}).

-spec merge_next_file_from_waiting(state()) -> ok.
merge_next_file_from_waiting(State=#sts{openFds = Open, waitingFds = Waiting}) ->
  %% expect the next file would not have a goal
  %% if it had one, it would be achieved by the current file
  %% but the current file reached its end
  {{value, {none, Fd}}, Q} = queue:out(Waiting),
  case ets:lookup(Open, Fd) of
    [] -> merge_next_file(State#sts{waitingFds = Q});
    _  -> merge(State#sts{currFd = Fd, waitingFds = Q})
  end.

%%-----------------------------------------------------------------------------
%% Validate a merged trace file.
%% 
%% Ensure that a symbolic variable has been declared before used.
%%-----------------------------------------------------------------------------

-spec validate_file(file:filename_all()) -> validation().
validate_file(F) ->
  {ok, Fd} = cuter_log:open_file(F, read),
  Known = gb_sets:new(),
  validate_entries(Fd, Known).

-spec validate_entries(file:io_device(), known_set()) -> validation().
validate_entries(Fd, Known) ->
  case cuter_log:next_entry(Fd, true) of
    eof -> ok;
    {Kind, Tp, Data}=Entry ->
      {Tp, Args} = cuter_json:json_to_command(Data),
      case validate_entry(Kind, Tp, Args, Known) of
        error -> {error, Entry};
        {ok, MoreKnown} -> validate_entries(Fd, MoreKnown)
      end
  end.

-spec validate_entry(integer(), integer(), [any()], known_set()) -> {ok, known_set()} | error.
validate_entry(?NOT_CONSTRAINT, ?OP_PARAMS, Args, Known) ->
  add_symbolic_vars(Args, Known);
validate_entry(Kind, _Tp, Args, Known) when Kind =:= ?CONSTRAINT_TRUE; Kind =:= ?CONSTRAINT_FALSE ->
  lookup_symbolic_vars(Args, Known);
validate_entry(?NOT_CONSTRAINT, Tp, _Args, _Known)
  when Tp =:= ?OP_SPAWN; Tp =:= ?OP_SPAWNED;
       Tp =:= ?OP_MSG_SEND; Tp =:= ?OP_MSG_RECEIVE; Tp =:= ?OP_MSG_CONSUME ->
  error;
validate_entry(?NOT_CONSTRAINT, Tp, [A|Args], Known) when Tp =:= ?OP_UNFOLD_TUPLE; Tp =:= ?OP_UNFOLD_LIST ->
  case lookup_symbolic_vars([A], Known) of
    error -> error;
    {ok, Known} -> add_symbolic_vars(Args, Known)
  end;
validate_entry(?NOT_CONSTRAINT, Tp, [T1, T2], Known) when Tp =:= ?OP_ERLANG_HD_1; Tp =:= ?OP_ERLANG_TL_1 ->
  case lookup_symbolic_vars([T2], Known) of
    error -> error;
    {ok, Known} -> add_symbolic_vars([T1], Known)
  end.

-spec add_symbolic_vars([any()], known_set()) -> {ok, known_set()}.
add_symbolic_vars([], Known) ->
  {ok, Known};
add_symbolic_vars([V|Vs], Known) ->
  case cuter_symbolic:is_symbolic(V) of
    false -> add_symbolic_vars(Vs, Known);
    true  -> add_symbolic_vars(Vs, gb_sets:add(V, Known))
  end.

-spec lookup_symbolic_vars([any()], known_set()) -> {ok, known_set()} | error.
lookup_symbolic_vars([], Known) ->
  {ok, Known};
lookup_symbolic_vars([V|Vs], Known) ->
  case cuter_symbolic:is_symbolic(V) of
    false -> lookup_symbolic_vars(Vs, Known);
    true ->
      case gb_sets:is_member(V, Known) of
        false -> error;
        true  -> lookup_symbolic_vars(Vs, Known)
      end
  end.

