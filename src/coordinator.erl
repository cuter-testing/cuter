-module(coordinator).
-export([run/3, test_run/3]).

%-define(PRINT_TRACE, ok).  %% Pretty Prints all traces
-define(DELETE_TRACE, ok).  %% Deletes all traces

-define(TRACEDIR(BaseDir), BaseDir ++ "/traces").
-define(COREDIR(BaseDir), BaseDir ++ "/core").

-type internal_error() :: 'internal_concolic_error'
                        | 'internal_codeserver_error'
                        | 'internal_traceserver_error'.
-type result() :: {'ok', node(), concolic:exec_info()}
                | {'runtime_error', node(), concolic:exec_info()}
                | {internal_error(), term()}.
-type ret()    :: {'ok', concolic_eval:result()}         %% Successful Execution
                | {'error', term()}                      %% Runtime Error
                | {'internal_error', internal_error()}.  %% Internal Error

%% ============================================================================
%% External exports
%% ============================================================================


-spec run(atom(), atom(), [term()]) -> ret().
run(M, F, As) ->
  io:format("Testing ~p:~p/~p.~n", [M, F, length(As)]),
  process_flag(trap_exit, true),
  E = 0,
  TmpDir = "temp",
  io:format("Input ~p~n", [As]),
  CR = concolic_execute(M, F, As, TmpDir, E),
  
  {DataDir, Traces, Mapping} = analyze_execution_info(CR),
  
  %% SIMPLIFICATION - ASSUME SEQUENCIAL PROGRAM
  [File] = proplists:get_value(node(), Traces),
  _ = z3_solve(File, 2, Mapping),
%  io:format("New Args : ~p~n", [As2]),
  
  _ = delete_execution_trace(DataDir),
  %% Directory will only be deleted if it's empty
  _ = file:del_dir(filename:absname(TmpDir)).


analyze_execution_info({'internal_error', IError}) ->
  io:format("Internal Error in Concolic Execution : ~p~n", [IError]);
analyze_execution_info({'ok', {Result, DataDir, Traces, Mapping}}) ->
  report_execution_status(Result),
  
%  io:format("~p~n", [Traces]),
  report_exec_vertices(Traces),
  
  {DataDir, Traces, Mapping}.


z3_solve(F, I, _Ms) ->
  io:format("Reversing ~p constraint~n", [I]),
  P = python:start(),
  python:exec(P, "python -u priv/erlang_port.py"),
  python:load_file(P, {F, 1, I}),
  Chk = python:check_model(P),
  io:format("Chk = ~p~n", [Chk]),
  case Chk of
    <<"sat">> ->
      M = python:get_model(P),
      io:format("Model = ~p~n", [M]);
    _ ->
      ok
  end,
  python:stop(P).


%% Concolic Execution of an M, F, As
concolic_execute(M, F, As, Dir, E) ->
  DataDir = Dir ++ "/exec" ++ integer_to_list(E),
  CoreDir = ?COREDIR(DataDir),    %% Directory to store .core files
  TraceDir = ?TRACEDIR(DataDir),  %% Directory to store traces
  Concolic = concolic:init_server(M, F, As, CoreDir, TraceDir),
  R = wait_for_execution(Concolic),
  analyze(R),
  case get_result(R) of
    {'internal_error', _IError} = IE -> IE;
    Result ->
%      io:format("~p~n", [R]),
      {'ok', {Result, DataDir, get_traces_dir(R), get_mapping(R)}}
  end.


wait_for_execution(Concolic) ->
  receive
    {'EXIT', Concolic, Why} -> {'internal_concolic_error', node(), Why};
    {Concolic, Results} -> Results
  end.




-spec test_run(atom(), atom(), [term()]) -> ret().
test_run(M, F, As) ->
  process_flag(trap_exit, true),
  TmpDir = "temp",
  {ok, {R, DataDir, _, _}} = concolic_execute(M, F, As, TmpDir, 0),
  _ = delete_execution_trace(DataDir),
  _ = file:del_dir(filename:absname(TmpDir)),
  R.

%% ============================================================================
%% Internal functions
%% ============================================================================

get_mapping({X, _Node, Result}) when X =:= 'ok'; X =:= 'runtime_error' ->
  {ok, Info} = orddict:find(node(), Result),
  {ok, proplists:get_value('mapping', Info)};
get_mapping({_Error, _Node, _R}) -> 'internal_error'.
  
%% Retrieve the outcome of the concolic execution
%% from the resulting execution information
-spec get_result(result()) -> ret().

get_result({'ok', Node, R}) ->
  {ok, Info} = orddict:find(Node, R),
  {ok, proplists:get_value('result', Info)};
get_result({'runtime_error', Node, R}) ->
  {ok, Info} = orddict:find(Node, R),
  {Node, _Who, {CErr, _Serr}} = proplists:get_value('runtime_error', Info),
  {'error', CErr};
get_result({Error, _Node, _R}) ->
  {'internal_error', Error}.



%% Delete the trace files / folders of an execution
delete_execution_trace(D) ->
  {ok, CWD} = file:get_cwd(),
  clear_dir(CWD ++ "/" ++ D).

%% Create a proplist with the trace files in the form:
%% [{Node, Files}] where Node :: node(), Files :: [file:name()]
-spec get_traces_dir(result()) -> [{node(), [file:name()]}].

get_traces_dir({_Status, _Node, Results}) ->
  Ns = orddict:to_list(Results),
  [{N, trace_files(R)} || {N, R} <- Ns].

trace_files(R) ->
  Logs = proplists:get_value('tlogs', R),
  Dir = proplists:get_value('dir', Logs),
  {ok, Fs} = file:list_dir(Dir),
  lists:map(fun(F) -> Dir ++ "/" ++ F end, Fs).
  
%% ------------------------------------------------------------------
%% Report Results
%% ------------------------------------------------------------------

report_execution_status({ok, _}) -> io:format("Passed~n");
report_execution_status(_) -> io:format("Execution Error~n").


report_exec_vertices([]) -> ok;
report_exec_vertices([{Node, Fs}|Rest]) ->
  io:format("~p~n", [Node]),
  F = fun(X) ->
    V = concolic_encdec:path_vertex(X),
    io:format("~p -> ~p~n", [X, V])
  end,
  lists:foreach(F, Fs),
  report_exec_vertices(Rest).



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

%% ------------------------------------------------------------------

%% temporary deleting all traces
clear_dir(D) ->
  case filelib:is_regular(D) of
    true ->
      {'ok', F} = concolic_encdec:open_file(D, 'read'),
      print_trace(F, D),
      concolic_encdec:close_file(F),
      delete_trace(D);
    false ->
      case file:del_dir(D) of
        'ok' -> 'ok';
        {error, eexist} ->
          {'ok', L} = file:list_dir(D),
          LL = [D ++ "/" ++ X || X <- L],
          lists:foreach(fun clear_dir/1, LL),
          file:del_dir(D);
        _ -> 'ok'
      end
  end.

-ifdef(PRINT_TRACE).
print_trace(F, D) ->
  io:format("%% Contents of ~p~n", [D]),
  concolic_encdec:pprint(F).
-else.
print_trace(_F, _D) ->
  'ok'.
-endif.

-ifdef(DELETE_TRACE).
delete_trace(F) ->
  file:delete(F).
-else.
delete_trace(_F) ->
  'ok'.
-endif.

