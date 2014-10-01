%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_analyzer).

-include("cuter_macros.hrl").

-export([get_result/1, get_mapping/1, get_traces/1, get_path_vertices/1,
         path_length/1, file_constraints/2]).

-export_type([execution_result/0, node_trace/0, path_vertex/0]).

-type execution_result() :: {success, any()} | {runtime_error, any()} | internal_error.
-type node_trace() :: {atom(), string()}.
-type path_vertex() :: [?CONSTRAINT_TRUE_REPR | ?CONSTRAINT_FALSE_REPR]. %% [$T | $F]

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




-spec get_path_vertices([node_trace()]) -> [{atom(), path_vertex()}].
get_path_vertices(Info) -> [node_path_vertices(I) || I <- Info].

node_path_vertices({Node, Dir}) ->
  Vs = proc_path_vertices(Dir),
  {Node, Vs}.

proc_path_vertices(Dir) ->
  Fs = cuter_lib:list_dir(Dir),
  [cuter_log:path_vertex(F) || F <- Fs].


-spec path_length([{atom(), path_vertex()}]) -> integer().
path_length(Vs) ->
  Xs = [node_path_length(V) || V <- Vs],
  lists:foldl(fun(X, Acc) -> X + Acc end, 0, Xs).

node_path_length({_Node, Ps}) ->
  lists:foldl(fun(X, Acc) -> X + Acc end, 0, [length(P) || P <- Ps]).


-spec file_constraints([node_trace()], integer()) -> [{file:filename(), integer(), integer(), boolean()}].
file_constraints([{_Node, Dir}|_], N) ->
  [F|_] = cuter_lib:list_dir(Dir),
  V = cuter_log:path_vertex(F),
  case N < length(V) of
    true  -> [{F, 1, N, true}];
    false -> [{F, 1, length(V), true}]
  end.
