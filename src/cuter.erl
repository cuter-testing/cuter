%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter).

-export([run/4, test_run/3]).

-include("concolic_flags.hrl").

-define(TRACEDIR(BaseDir), BaseDir ++ "/traces").
-define(COREDIR(BaseDir), BaseDir ++ "/core").
-define(PYTHON_CALL, ?PYTHON_PATH ++ " -u " ++ ?PRIV ++ "/erlang_port.py").

%% ------------------------------------------------------------------
%% Run function
%% ------------------------------------------------------------------

-spec run(atom(), atom(), [term()], pos_integer()) -> ok.

run(M, F, As, Depth) ->
  error_logger:tty(false),  %% Disable error_logger
  io:format("Testing ~p:~p/~p ...~n", [M, F, length(As)]),
  {TmpDir, E, S} = init(Depth),
  pprint_input(As),
  CR = concolic_execute(M, F, As, TmpDir, E, Depth),
  {DataDir, Traces, Mapping} = prepare_execution_info(S, CR),
  ok = concolic_scheduler:initial_execution(S, DataDir, Traces, Mapping),
  loop(M, F, TmpDir, E+1, S, Depth).

loop(M, F, TmpDir, E, S, Depth) ->
  case concolic_scheduler:request_input(S) of
    empty ->
      concolic_scheduler:stop(S),
      _ = file:del_dir(filename:absname(TmpDir)),
      ok;
    {R, As} ->
      pprint_input(As),
      CR = concolic_execute(M, F, As, TmpDir, E, Depth),
      {DataDir, Traces, Mapping} = prepare_execution_info(S, CR),
      ok = concolic_scheduler:store_execution(S, R, DataDir, Traces, Mapping),
      loop(M, F, TmpDir, E+1, S, Depth)
  end.

init(Depth) ->
  process_flag(trap_exit, true),
  TmpDir = "temp",
  E = 0,
  S = concolic_scheduler:start(?PYTHON_CALL, Depth),
  {TmpDir, E, S}.

prepare_execution_info(S, {'internal_error', IError}) ->
  io:format("Internal Error in Concolic Execution : ~p~n", [IError]),
  concolic_scheduler:stop(S),
  exit(normal);
prepare_execution_info(_S, {'ok', {Result, DataDir, Traces, Mapping}}) ->
  report_execution_status(Result),
  report_exec_vertices(Traces),
  report_trace_contents(Traces),
  {DataDir, Traces, Mapping}.

%% Run function for testing
-spec test_run(atom(), atom(), [term()]) -> concolic_analyzer:ret().

test_run(M, F, As) ->
  process_flag(trap_exit, true),
  TmpDir = "temp",
  {ok, {R, DataDir, _, _}} = concolic_execute(M, F, As, TmpDir, 0, 1000),
  _ = concolic_analyzer:clear_and_delete_dir(DataDir),
  _ = file:del_dir(filename:absname(TmpDir)),
  R.

%% ------------------------------------------------------------------
%% Concolic Execution
%% ------------------------------------------------------------------

%% Concolic Execution of an M, F, As
concolic_execute(M, F, As, Dir, E, Depth) ->
  DataDir = Dir ++ "/exec" ++ integer_to_list(E),
  CoreDir = ?COREDIR(DataDir),    %% Directory to store .core files
  TraceDir = ?TRACEDIR(DataDir),  %% Directory to store traces
  Concolic = concolic:init_server(M, F, As, CoreDir, TraceDir, Depth),
  R = wait_for_execution(Concolic),
  analyze(R),
  case concolic_analyzer:get_result(R) of
    {'internal_error', _IError} = IE -> IE;
    Result ->
      {ok, {Result, DataDir, concolic_analyzer:get_traces(R), concolic_analyzer:get_mapping(R)}}
  end.

wait_for_execution(Concolic) ->
  receive
    {'EXIT', Concolic, Why} -> {'internal_concolic_error', node(), Why};
    {Concolic, Results} -> Results
  end.

%% ------------------------------------------------------------------
%% Report Results
%% ------------------------------------------------------------------

pprint_input([H|T]) ->
  lists:foreach(fun(_) -> io:format("-") end, lists:seq(1,50)),
  io:format("~nInput: ~w", [H]),
  lists:foreach(fun(X) -> io:format(", ~w", [X]) end, T),
  io:format("~n").

report_execution_status({ok, {Cv, _}}) -> io:format(" Result: ~w~n", [Cv]);
report_execution_status({error, CR}) -> io:format(" Runtime Error: ~w~n", [CR]).

report_exec_vertices([]) -> ok;
report_exec_vertices([{_Node, Fs}|Rest]) ->
  F = fun(X) ->
    V = concolic_encdec:path_vertex(X),
    io:format(" Path Vertex: ~p~n", [V])
  end,
  lists:foreach(F, Fs),
  report_exec_vertices(Rest).

-ifdef(PRINT_TRACES).
report_trace_contents(Traces) ->
  lists:foreach(fun report_trace_contents_node/1, Traces).

report_trace_contents_node({_Node, TraceFiles}) ->
  F = fun(X) ->
    io:format("Contents of ~p~n", [X]),
    ok = concolic_analyzer:print_trace(X)
  end,
  lists:foreach(F, TraceFiles).
-else.
report_trace_contents(_) -> ok.
-endif.

-ifdef(PRINT_ANALYSIS).
analyze({'internal_concolic_error', _Node, Error}) ->
  io:format("%%   Internal ConcServer error : ~p~n", [Error]);
analyze({'internal_codeserver_error', Node, Results}) ->
  io:format("%%   Internal CodeServer Error in node ~p~n", [Node]),
  report(Results);
analyze({'internal_traceserver_error', Node, Results}) ->
  io:format("%%   Internal TraceServer Error in node ~p~n", [Node]),
  report(Results);
analyze({'runtime_error', Node, Results}) ->
  io:format("%%   Runtime error in Node ~p~n", [Node]),
  report(Results);
analyze({'ok', _Node, Results}) ->
  report(Results).

report(R) ->
  L = orddict:to_list(R),
  lists:foreach(fun report_node/1, L).
  
report_node({N, R}) ->
  io:format("%% Node ~w~n", [N]),
  lists:foreach(fun report_result/1, R).
  
report_result({'result', {CR, SR}}) ->
  io:format("%%   Concrete Result = ~p~n", [CR]),
  io:format("%%   Symbolic Result = ~p~n", [SR]);
report_result({'mapping', R}) ->
  io:format("%%   Mapping = ~p~n", [R]);
report_result({'runtime_error', {_Node, Who, {CErr, SErr}}}) ->
  io:format("%%   Runtime Error in ~p~n", [Who]),
  io:format("%%   Concrete Error = ~p~n", [CErr]),
  io:format("%%   Symbolic Error = ~p~n", [SErr]);
report_result({'clogs', Logs}) ->
  io:format("%%   Loaded ~w Modules: ~w~n", [length(Logs), Logs]);
report_result({'tlogs', Logs}) ->
  io:format("%%   Monitored Processes : ~w~n", [proplists:get_value('procs', Logs)]),
  io:format("%%   Traces Directory : ~p~n", [proplists:get_value('dir', Logs)]);
report_result({'codeserver_error', Error}) ->
  io:format("%%   CodeServer Error = ~p~n", [Error]);
report_result({'traceserver_error', Error}) ->
  io:format("%%   TraceServer Error = ~p~n", [Error]);
report_result(X) ->
  io:format("Unexpected ~w~n", [X]).
-else.
analyze(_) -> ok.
-endif.

