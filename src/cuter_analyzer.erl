%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_analyzer).

-export([get_result/1, get_mapping/1, get_traces/1, get_cached_modules/1,
         get_int_process/1, process_raw_execution_info/1, get_tags/1,
         get_no_of_tags_added/1,
         %% Constructor & accessors for #raw{}
         mk_raw_info/8, traces_of_raw_info/1, int_of_raw_info/1,
         %% Accessors for #info{}
         mappings_of_info/1, runtimeError_of_info/1, traceFile_of_info/1, reversible_of_info/1,
         dir_of_info/1, tags_of_info/1, cachedMods_of_info/1, pathVertex_of_info/1, tagsAddedNo_of_info/1]).

-include("include/cuter_macros.hrl").

-export_type([execution_result/0, node_trace/0, path_vertex/0,
              raw_info/0, info/0, visited_tags/0,
              reversible_with_tags/0, reversible_with_tag/0]).

-type execution_result() :: {success, any()} | {runtime_error, any()} | internal_error.
-type node_trace()  :: {atom(), string()}.
-type visited_tags() :: gb_sets:set(cuter_cerl:tagID()).
-type reversible_with_tag() :: {integer(), cuter_cerl:tagID()}.
-type reversible_with_tags() :: [reversible_with_tag()].
-type path_vertex() :: [?CONSTRAINT_TRUE_REPR | ?CONSTRAINT_FALSE_REPR].  % [$T | $F]

-record(raw, {
  mappings      :: [cuter_symbolic:mapping()],
  result        :: execution_result(),
  traces        :: [node_trace()],
  int           :: cuter_iserver:int_process(),
  dir           :: file:filename(),
  tags          :: visited_tags(),
  cachedMods    :: cuter_codeserver:cached_modules(),
  tags_added_no :: integer()
}).
-type raw_info() :: #raw{}.

-record(info, {
  dir          :: file:filename(),
  mappings     :: [cuter_symbolic:mapping()],
  pathLength   :: integer(),
  pathVertex   :: path_vertex(),
  reversible   :: reversible_with_tags(),
  runtimeError :: boolean(),
  cachedMods   :: cuter_codeserver:cached_modules(),
  tags         :: visited_tags(),
  tagsAddedNo  :: integer(),
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

-spec get_tags(cuter_iserver:logs()) -> visited_tags().
get_tags(Logs) ->
  CodeLogs = cuter_iserver:codeLogs_of_logs(Logs),
  AllNodes = [cuter_codeserver:visitedTags_of_logs(Data) || {_Node, Data} <- CodeLogs],
  lists:foldl(fun(X, Ts) -> gb_sets:union(X, Ts) end,
              gb_sets:new(),
              AllNodes).

-spec get_cached_modules(cuter_iserver:logs()) -> cuter_codeserver:cached_modules().
get_cached_modules(Logs) ->
  CodeLogs = cuter_iserver:codeLogs_of_logs(Logs),
  AllNodes = [cuter_codeserver:cachedMods_of_logs(Data) || {_Node, Data} <- CodeLogs],
  %% FIXME Now expects just one node.
  hd(AllNodes).

-spec get_no_of_tags_added(cuter_iserver:logs()) -> cuter_codeserver:counter().
get_no_of_tags_added(Logs) ->
  CodeLogs = cuter_iserver:codeLogs_of_logs(Logs),
  AllNodes = [cuter_codeserver:tagsAddedNo_of_logs(Data) || {_Node, Data} <- CodeLogs],
  %% FIXME Now expects just one node.
  hd(AllNodes).

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
        cachedMods = Info#raw.cachedMods,
        tags = Info#raw.tags,
        tagsAddedNo = Info#raw.tags_added_no,
        traceFile = MergedTraceFile}.

%% ----------------------------------------------------------------------------
%% Constructor & accessors of #raw{}.
%% ----------------------------------------------------------------------------

%% Creates a raw_info record.
-spec mk_raw_info([cuter_symbolic:mapping()], execution_result(), [node_trace()], cuter_iserver:int_process(),
                  file:filename(), visited_tags(), cuter_codeserver:cached_modules(), integer()) -> raw_info().
mk_raw_info(Mappings, Result, Traces, Int, Dir, Tags, CachedMods, TagsN) ->
  #raw{mappings = Mappings,
       result = Result,
       traces = Traces,
       int = Int,
       dir = Dir,
       tags = Tags,
       cachedMods = CachedMods,
       tags_added_no = TagsN}.

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

-spec tags_of_info(info()) -> visited_tags().
tags_of_info(Info) ->
  Info#info.tags.

-spec cachedMods_of_info(info()) -> cuter_codeserver:cached_modules().
cachedMods_of_info(Info) ->
  Info#info.cachedMods.

-spec pathVertex_of_info(info()) -> path_vertex().
pathVertex_of_info(Info) ->
  Info#info.pathVertex.

-spec tagsAddedNo_of_info(info()) -> integer().
tagsAddedNo_of_info(Info) ->
  Info#info.tagsAddedNo.
