%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_analyzer).

-include("include/cuter_macros.hrl").

-export([get_result/1, get_mapping/1, get_traces/1, get_stored_modules/1,
         get_int_process/1, process_raw_execution_info/1, get_tags/1,
         get_no_of_tags_added/1, mk_raw_info/8, traces_of_raw_info/1,
         int_of_raw_info/1]).

-export_type([execution_result/0, node_trace/0, path_vertex/0, int_process/0,
              raw_info/0, info/0, visited_tags/0, stored_modules/0,
              reversible_with_tags/0, reversible_with_tag/0]).

-type execution_result() :: {success, any()} | {runtime_error, any()} | internal_error.
-type node_trace()  :: {atom(), string()}.
-type int_process() :: {node(), pid()}.
-type visited_tags() :: gb_sets:set(cuter_cerl:tagID()).
-type reversible_with_tag() :: {integer(), cuter_cerl:tagID()}.
-type reversible_with_tags() :: [reversible_with_tag()].
-type stored_modules() :: orddict:orddict().
-type path_vertex() :: [?CONSTRAINT_TRUE_REPR | ?CONSTRAINT_FALSE_REPR].  % [$T | $F]


-record(raw, {
  mappings      :: [cuter_symbolic:mapping()],
  result        :: execution_result(),
  traces        :: [node_trace()],
  int           :: int_process(),
  dir           :: file:filename(),
  tags          :: visited_tags(),
  stored_mods   :: stored_modules(),
  tags_added_no :: integer()
}).
-type raw_info() :: #raw{}.

-type info() :: #{mappings => [cuter_symbolic:mapping()],
                  runtime_error => boolean(),
                  traceFile => file:filename_all(),
                  pathLength => integer(),
                  reversible => reversible_with_tags(),
                  dir => file:filename_all(),
                  tags => visited_tags(),
                  stored_mods => stored_modules(),
                  path_vertex => path_vertex(),
                  tags_added_no => integer()}.


-spec get_result(cuter_iserver:execution_status()) -> execution_result().
get_result({success, {Cv, _Sv}}) ->
  {success, Cv};
get_result({internal_error, _What, _Node, _Why}) ->
  internal_error;
get_result({runtime_error, _Node, _Pid, {Cv, _Sv}}) ->
  {runtime_error, Cv}.

-spec is_runtime_error(execution_result()) -> boolean().
is_runtime_error(internal_error) -> false;
is_runtime_error({success, _Cv}) -> false;
is_runtime_error({runtime_error, _Cv}) -> true.

-spec get_mapping(orddict:orddict()) -> [cuter_symbolic:mapping()].
get_mapping([{_Node, Data}|Info]) ->
  case proplists:get_value(mapping, Data, not_found) of
    not_found -> get_mapping(Info);
    Ms -> Ms
  end.

-spec get_traces(orddict:orddict()) -> [node_trace()].
get_traces(Info) -> [get_trace_dir(I) || I <- Info].

get_trace_dir({Node, Data}) ->
  Logs = proplists:get_value(monitor_logs, Data),
  Dir = proplists:get_value(dir, Logs),
  {Node, Dir}.

-spec get_int_process(orddict:orddict()) -> int_process().
get_int_process([{_Node, Data}|Info]) ->
  case proplists:get_value(int, Data, not_found) of
    not_found -> get_mapping(Info);
    Pid -> Pid
  end.

-spec get_tags(orddict:orddict()) -> visited_tags().
get_tags(Info) ->
  lists:foldl(fun(X, Ts) -> gb_sets:union(X, Ts) end,
              gb_sets:new(),
              [get_tags_of_node(I) || I <- Info]).

get_tags_of_node({_Node, Data}) ->
  Logs = proplists:get_value(code_logs, Data),
  proplists:get_value(visited_tags, Logs).

-spec get_stored_modules(orddict:orddict()) -> stored_modules().
get_stored_modules(Info) ->
  AllNodes = [get_stored_modules_of_node(I) || I <- Info],
  %% FIXME Now expects just one node.
  hd(AllNodes).

get_stored_modules_of_node({_Node, Data}) ->
  Logs = proplists:get_value(code_logs, Data),
  proplists:get_value(stored_mods, Logs).

-spec get_no_of_tags_added(orddict:orddict()) -> integer().
get_no_of_tags_added(Info) ->
  AllNodes = [get_no_of_tags_added_of_node(I) || I <- Info],
  %% FIXME Now expects just one node.
  hd(AllNodes).

get_no_of_tags_added_of_node({_Node, Data}) ->
  Logs = proplists:get_value(code_logs, Data),
  proplists:get_value(tags_added_no, Logs).

%% Creates a raw_info record.
-spec mk_raw_info([cuter_symbolic:mapping()], execution_result(), [node_trace()], int_process(),
                  file:filename(), visited_tags(), stored_modules(), integer()) -> raw_info().
mk_raw_info(Mappings, Result, Traces, Int, Dir, Tags, StoredMods, TagsN) ->
  #raw{mappings = Mappings,
       result = Result,
       traces = Traces,
       int = Int,
       dir = Dir,
       tags = Tags,
       stored_mods = StoredMods,
       tags_added_no = TagsN}.

%% Gets the traces from a raw_info record.
-spec traces_of_raw_info(raw_info()) -> [node_trace()].
traces_of_raw_info(RawInfo) ->
  RawInfo#raw.traces.

%% Gets the int_process from a raw_info record.
-spec int_of_raw_info(raw_info()) -> int_process().
int_of_raw_info(RawInfo) ->
  RawInfo#raw.int.

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
  #{dir => DataDir,
    runtime_error => is_runtime_error(Info#raw.result),
    mappings => Info#raw.mappings,
    traceFile => MergedTraceFile,
    pathLength => RvsCnt,
    reversible => Rvs,
    tags => Info#raw.tags,
    stored_mods => Info#raw.stored_mods,
    path_vertex => PathVertex,
    tags_added_no => Info#raw.tags_added_no}.


