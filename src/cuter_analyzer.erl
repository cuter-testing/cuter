%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_analyzer).

-include("include/cuter_macros.hrl").

-export([get_result/1, get_mapping/1, get_traces/1,
         get_int_process/1, process_raw_execution_info/1]).

-export_type([execution_result/0, node_trace/0, path_vertex/0, int_process/0,
              raw_info/0, info/0]).

-type execution_result() :: {success, any()} | {runtime_error, any()} | internal_error.
-type node_trace()  :: {atom(), string()}.
-type int_process() :: {node(), pid()}.
-type path_vertex() :: [?CONSTRAINT_TRUE_REPR | ?CONSTRAINT_FALSE_REPR].  % [$T | $F]

-type raw_info() :: #{mappings => [cuter_symbolic:mapping()],
                      traces => [cuter_analyzer:node_trace()],
                      int => cuter_analyzer:int_process(),
                      dir => file:filename_all()}.

-type info() :: #{mappings => [cuter_symbolic:mapping()],
                  traceFile => file:filename_all(),
                  pathLength => integer(),
                  dir => file:filename_all()}.


-spec get_result(cuter_iserver:execution_status()) -> execution_result().
get_result({success, {Cv, _Sv}}) ->
  {success, Cv};
get_result({internal_error, _What, _Node, _Why}) ->
  internal_error;
get_result({runtime_error, _Node, _Pid, {Cv, _Sv}}) ->
  {runtime_error, Cv}.

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


-spec process_raw_execution_info(raw_info()) -> info().
process_raw_execution_info(Info) ->
%  io:format("[RAW INFO] ~p~n", [Info]),
  DataDir = maps:get(dir, Info),
  MergedTraceFile = cuter_lib:get_merged_tracefile(DataDir),
  cuter_merger:merge_traces(Info, MergedTraceFile),
  cuter_lib:clear_and_delete_dir(maps:get(dir, Info), MergedTraceFile),
  PathVertex = cuter_log:path_vertex(MergedTraceFile),
  cuter_pp:path_vertex(PathVertex),
  RvsCnt = cuter_log:count_reversible(MergedTraceFile),
%%  cuter_pp:reversible_operations(RvsCnt),
  #{dir => DataDir,
    mappings => maps:get(mappings, Info),
    traceFile => MergedTraceFile,
    pathLength => RvsCnt}.


