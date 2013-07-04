-module(coordinator).
-export([run/3, test_run/3]).

%-define(PRINT_ANALYSIS, ok). %% Prints an execution analysis
%-define(PRINT_TRACE, ok).  %% Pretty Prints all traces

-define(TRACEDIR(BaseDir), BaseDir ++ "/traces").
-define(COREDIR(BaseDir), BaseDir ++ "/core").
-define(PYTHON_CALL, "python -u priv/erlang_port.py").

%% ============================================================================
%% External exports
%% ============================================================================

-spec run(atom(), atom(), [term()]) -> ok.
run(M, F, As) ->
  io:format("Testing ~p:~p/~p ...~n", [M, F, length(As)]),
  {TmpDir, E, S} = init(),
  pprint_input(As),
  CR = concolic_execute(M, F, As, TmpDir, E),
  {DataDir, Traces, Mapping} = analyze_execution_info(CR),
  ok = concolic_scheduler:initial_execution(S, DataDir, Traces, Mapping),
  loop(M, F, TmpDir, E+1, S).

loop(M, F, TmpDir, E, S) ->
  case concolic_scheduler:request_input(S) of
    empty ->
      _ = file:del_dir(filename:absname(TmpDir)),
      ok;
    {R, As} ->
      pprint_input(As),
      CR = concolic_execute(M, F, As, TmpDir, E),
      {DataDir, Traces, Mapping} = analyze_execution_info(CR),
      ok = concolic_scheduler:store_execution(S, R, DataDir, Traces, Mapping),
      loop(M, F, TmpDir, E+1, S)
  end.

init() ->
  process_flag(trap_exit, true),
  TmpDir = "temp",
  E = 0,
  S = concolic_scheduler:start(?PYTHON_CALL),
  {TmpDir, E, S}.


analyze_execution_info({'internal_error', IError}) ->
  io:format("Internal Error in Concolic Execution : ~p~n", [IError]);
analyze_execution_info({'ok', {Result, DataDir, Traces, Mapping}}) ->
  report_execution_status(Result),
  
%  io:format("~p~n", [Traces]),
  report_exec_vertices(Traces),
  
  {DataDir, Traces, Mapping}.




%% Concolic Execution of an M, F, As
concolic_execute(M, F, As, Dir, E) ->
  DataDir = Dir ++ "/exec" ++ integer_to_list(E),
  CoreDir = ?COREDIR(DataDir),    %% Directory to store .core files
  TraceDir = ?TRACEDIR(DataDir),  %% Directory to store traces
  Concolic = concolic:init_server(M, F, As, CoreDir, TraceDir),
  R = wait_for_execution(Concolic),
  analyze(R),
  case concolic_analyzer:get_result(R) of
    {'internal_error', _IError} = IE -> IE;
    Result ->
%      io:format("~p~n", [R]),
      {'ok', {Result, DataDir, concolic_analyzer:get_traces(R), concolic_analyzer:get_mapping(R)}}
  end.


wait_for_execution(Concolic) ->
  receive
    {'EXIT', Concolic, Why} -> {'internal_concolic_error', node(), Why};
    {Concolic, Results} -> Results
  end.




-spec test_run(atom(), atom(), [term()]) -> concolic_analyzer:ret().
test_run(M, F, As) ->
  process_flag(trap_exit, true),
  TmpDir = "temp",
  {ok, {R, DataDir, _, _}} = concolic_execute(M, F, As, TmpDir, 0),
  _ = concolic_analyzer:clear_and_delete_dir(DataDir),
  _ = file:del_dir(filename:absname(TmpDir)),
  R.

%% ============================================================================
%% Internal functions
%% ============================================================================

%% ------------------------------------------------------------------
%% Report Results
%% ------------------------------------------------------------------

pprint_input([H|T]) ->
  lists:foreach(fun(_) -> io:format("-") end, lists:seq(1,50)),
  io:format("~nInput: ~w~n", [H]),
  lists:foreach(fun(X) -> io:format(", ~w", [X]) end, T).

report_execution_status({ok, {Cv, _}}) -> io:format(" Result: ~w~n", [Cv]);
report_execution_status({error, CR}) -> io:format(" Runtime Error: ~w~n", [CR]).

report_exec_vertices([]) -> ok;
report_exec_vertices([{_Node, Fs}|Rest]) ->
%  io:format("~p~n", [Node]),
  F = fun(X) ->
    V = concolic_encdec:path_vertex(X),
%    io:format("~p -> ~p~n", [X, V])
    io:format(" Path Vertex: ~p~n", [V])
  end,
  lists:foreach(F, Fs),
  report_exec_vertices(Rest).

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

%% ------------------------------------------------------------------

%% temporary deleting all traces
%clear_dir(D) ->
%  case filelib:is_regular(D) of
%    true ->
%      {'ok', F} = concolic_encdec:open_file(D, 'read'),
%      print_trace(F, D),
%      concolic_encdec:close_file(F),
%      delete_trace(D);
%    false ->
%      case file:del_dir(D) of
%        'ok' -> 'ok';
%        {error, eexist} ->
%          {'ok', L} = file:list_dir(D),
%          LL = [D ++ "/" ++ X || X <- L],
%          lists:foreach(fun clear_dir/1, LL),
%          file:del_dir(D);
%        _ -> 'ok'
%      end
%  end.

%-ifdef(PRINT_TRACE).
%print_trace(F, D) ->
%  io:format("%% Contents of ~p~n", [D]),
%  concolic_encdec:pprint(F).
%-else.
%print_trace(_F, _D) ->
%  'ok'.
%-endif.

%-ifdef(DELETE_TRACE).
%delete_trace(F) ->
%  file:delete(F).
%-else.
%delete_trace(_F) ->
%  'ok'.
%-endif.

