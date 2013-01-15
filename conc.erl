-module(conc).
-export([run/3]).

-define(STOP_SERVER(Server), gen_server:call(Server, terminate)).
-define(PROFILING_FLAG, false).

run(M, F, As) ->
  
  %% Temporary directory to store Core Erlang code
  CoreDir = filename:absname("core_temp"),
  %% Start Code and Trace Servers
  CodeServer = conc_cserver:init_codeserver(CoreDir),
  TraceServer = conc_tserver:init_traceserver(),
  
  Start = now(),
  profiling_start(?PROFILING_FLAG),
 
  %% Concrete Evaluation of MFA and Execution Logs
  Result = conc_eval:i(M, F, As, CodeServer, TraceServer),
  receive
    {TraceServer, TLogs} -> 
      ?STOP_SERVER(CodeServer)
  end,
  receive
    {CodeServer, CLogs} ->
      ok
  end,
  
  profiling_stop(?PROFILING_FLAG),
  End  = now(),
  Time = timer:now_diff(End, Start),
  
  %% Print Results
  io:format("---------------------~n"),
  io:format("Execution Information~n"),
  io:format("---------------------~n"),
  report_result(Result),
  report_tlogs(TLogs),
  report_clogs(CLogs),
  io:format("Time elapsed = ~w secs~n", [Time/1000000]),
  ok.

  
profiling_start(true) ->
  eprof:start(),
  eprof:start_profiling([self()]);
profiling_start(false) ->
  ok.
  
profiling_stop(true) ->
  eprof:stop_profiling(),
  eprof:analyze(),
  eprof:stop();
profiling_stop(false) ->
  ok.
  
report_result({ok, Result}) ->
  io:format("Result = ~p~n", [Result]);
report_result({error, {Who, Error}}) ->
  io:format("Runtime Error in ~p = ~p~n", [Who, Error]).
  
report_tlogs(Logs) ->
  io:format("Monitored Processes : ~w~n", [proplists:get_value(procs, Logs)]).
  
report_clogs(Logs) ->
  io:format("Loaded ~w Modules: ~w~n", [length(Logs), Logs]).
