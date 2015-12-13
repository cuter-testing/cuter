%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_analyzer).

-export([get_result/1, get_mapping/1, get_traces/1, is_runtime_error/1,
         get_int_process/1, process_raw_execution_info/1, calculate_coverage/3,
         %% Constructor & accessors for #raw{}
         mk_raw_info/5, traces_of_raw_info/1, int_of_raw_info/1,
         %% Accessors for #info{}
         mappings_of_info/1, runtimeError_of_info/1, traceFile_of_info/1, reversible_of_info/1,
         dir_of_info/1, pathVertex_of_info/1]).

-include("include/cuter_macros.hrl").

-export_type([execution_result/0, node_trace/0, path_vertex/0,
              raw_info/0, info/0, reversible_with_tags/0, reversible_with_tag/0]).

-type execution_result() :: {success, any()} | {runtime_error, any()} | internal_error.
-type node_trace()  :: {atom(), string()}.
-type reversible_with_tag() :: {integer(), cuter_cerl:tagID()}.
-type reversible_with_tags() :: [reversible_with_tag()].
-type path_vertex() :: [?CONSTRAINT_TRUE_REPR | ?CONSTRAINT_FALSE_REPR].  % [$T | $F]

-record(raw, {
  mappings      :: [cuter_symbolic:mapping()],
  result        :: execution_result(),
  traces        :: [node_trace()],
  int           :: cuter_iserver:int_process(),
  dir           :: file:filename()
}).
-type raw_info() :: #raw{}.

-record(info, {
  dir          :: file:filename(),
  mappings     :: [cuter_symbolic:mapping()],
  pathLength   :: integer(),
  pathVertex   :: path_vertex(),
  reversible   :: reversible_with_tags(),
  runtimeError :: boolean(),
  traceFile    :: file:filename()
}).
-type info() :: #info{}.


-spec get_result(cuter_iserver:execution_status()) -> execution_result().
get_result({success, {Cv, _Sv}}) ->
  {success, Cv};
get_result({internal_error, _InternalError}) ->
  internal_error;
get_result({runtime_error, {_Node, _Pid, {Cv, _Sv}}}) ->
  {runtime_error, Cv}.

-spec is_runtime_error(execution_result()) -> boolean().
is_runtime_error(internal_error) -> false;
is_runtime_error({success, _Cv}) -> false;
is_runtime_error({runtime_error, _Cv}) -> true.

%% Assume that one node holds the mappings.
-spec get_mapping(cuter_iserver:logs()) -> [cuter_symbolic:mapping()].
get_mapping(Logs) ->
  Ms = cuter_iserver:mapping_of_logs(Logs),
  [{_Node, Mapping}] = lists:filter(fun({_N, Data}) -> Data =/= undefined end, Ms),
  Mapping.

-spec get_traces(cuter_iserver:logs()) -> [node_trace()].
get_traces(Logs) ->
  MonitorLogs = cuter_iserver:monitorLogs_of_logs(Logs),
  [get_trace_dir(Ls) || Ls <- MonitorLogs].

-spec get_trace_dir({node(), cuter_monitor:logs()}) -> node_trace().
get_trace_dir({Node, Logs}) ->
  Dir = cuter_monitor:dir_of_logs(Logs),
  {Node, Dir}.

-spec get_int_process(cuter_iserver:logs()) -> cuter_iserver:int_process().
get_int_process(Logs) ->
  Ints = cuter_iserver:int_of_logs(Logs),
  [{_Node, Int}] = lists:filter(fun({_N, Data}) -> Data =/= undefined end, Ints),
  Int.

-spec process_raw_execution_info(raw_info()) -> info().
process_raw_execution_info(Info) ->
  DataDir = Info#raw.dir,
  MergedTraceFile = cuter_lib:get_merged_tracefile(DataDir),
  cuter_merger:merge_traces(Info, MergedTraceFile),
  cuter_lib:clear_and_delete_dir(Info#raw.dir, MergedTraceFile),
  PathVertex = cuter_log:path_vertex(MergedTraceFile),
  Rvs = cuter_log:locate_reversible(MergedTraceFile),
  RvsCnt = length(Rvs),
%%  cuter_pp:reversible_operations(RvsCnt),
  #info{dir = DataDir,
        mappings = Info#raw.mappings,
        pathLength = RvsCnt,
        pathVertex = PathVertex,
        reversible = Rvs,
        runtimeError = is_runtime_error(Info#raw.result),
        traceFile = MergedTraceFile}.

%% ----------------------------------------------------------------------------
%% Constructor & accessors of #raw{}.
%% ----------------------------------------------------------------------------

%% Creates a raw_info record.
-spec mk_raw_info([cuter_symbolic:mapping()], execution_result(), [node_trace()], cuter_iserver:int_process(), file:filename()) -> raw_info().
mk_raw_info(Mappings, Result, Traces, Int, Dir) ->
  #raw{mappings = Mappings,
       result = Result,
       traces = Traces,
       int = Int,
       dir = Dir}.

%% Gets the traces from a raw_info record.
-spec traces_of_raw_info(raw_info()) -> [node_trace()].
traces_of_raw_info(RawInfo) ->
  RawInfo#raw.traces.

%% Gets the int_process from a raw_info record.
-spec int_of_raw_info(raw_info()) -> cuter_iserver:int_process().
int_of_raw_info(RawInfo) ->
  RawInfo#raw.int.

%% ----------------------------------------------------------------------------
%% Accessors for #info{}.
%% ----------------------------------------------------------------------------

-spec mappings_of_info(info()) -> [cuter_symbolic:mapping()].
mappings_of_info(Info) ->
  Info#info.mappings.

-spec runtimeError_of_info(info()) -> boolean().
runtimeError_of_info(Info) ->
  Info#info.runtimeError.

-spec traceFile_of_info(info()) -> file:filename().
traceFile_of_info(Info) ->
  Info#info.traceFile.

-spec reversible_of_info(info()) -> reversible_with_tags().
reversible_of_info(Info) ->
  Info#info.reversible.

-spec dir_of_info(info()) -> file:filename().
dir_of_info(Info) ->
  Info#info.dir.

-spec pathVertex_of_info(info()) -> path_vertex().
pathVertex_of_info(Info) ->
  Info#info.pathVertex.

%% ----------------------------------------------------------------------------
%% Calculate coverage.
%% ----------------------------------------------------------------------------

-spec calculate_coverage(boolean(), pid(), cuter_scheduler_maxcover:logs()) -> {float(), float()} | ok.
calculate_coverage(false, _, _) -> ok;
calculate_coverage(true, CodeServer, SchedulerLogs) ->
  io:format("~n~nCoverage~n"),
  %% Visited Tags.
  VisitedTags = cuter_scheduler_maxcover:get_visitedTags(SchedulerLogs),
  %% Feasible Tags (with compiler generated clauses).
  FeasibleTags = cuter_codeserver:get_feasible_tags(CodeServer, true),
  %% Feasible Tags (without compiler generated clauses).
  FeasibleTagsNoComp = cuter_codeserver:get_feasible_tags(CodeServer, false),
  %% Calculate the coverage (with compiler generated clauses).
  All = gb_sets:size(FeasibleTags),
  Diff = gb_sets:subtract(FeasibleTags, VisitedTags),
  NotVisited = gb_sets:size(Diff),
  Coverage = 100 * (All - NotVisited) / All,
  io:format("Covered ~p of ~p clauses (~.2f %).~n", [All - NotVisited, All, Coverage]),
  %% Calculate the coverage (without compiler generated clauses).
  AllNoComp = gb_sets:size(FeasibleTagsNoComp),
  DiffNoComp = gb_sets:subtract(FeasibleTagsNoComp, VisitedTags),
  NotVisitedNoComp = gb_sets:size(DiffNoComp),
  CoverageNoComp = 100 * (AllNoComp - NotVisitedNoComp) / AllNoComp,
  io:format("Covered ~p of ~p clauses [without compiler generated clauses] (~.2f %).~n",
    [AllNoComp - NotVisitedNoComp, AllNoComp, CoverageNoComp]),
  {Coverage, CoverageNoComp}.
