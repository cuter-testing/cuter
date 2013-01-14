-module(conc).
-export([run/3]).

-define(PROFILING_FLAG, false).

run(M, F, As) ->
  
  %% Start Code and Trace Servers
  Dir = filename:absname("core"),
  {ok, CodeServer} = gen_server:start_link(conc_cserver, [Dir], []),
  {ok, TraceServer} = gen_server:start_link(conc_tserver, [self()], []),
  
  Start = now(),
  profiling_start(?PROFILING_FLAG),
 
  %% Concrete Evaluation of MFA
  Result = conc_eval:i(M, F, As, CodeServer, TraceServer),
  receive
    {TraceServer, Msg} -> 
      gen_server:call(CodeServer, terminate)
  end,
  
  profiling_stop(?PROFILING_FLAG),
  End  = now(),
  Time = timer:now_diff(End, Start),
  
  %% Print Results
  io:format("Result = ~p~n", [Result]),
  io:format("Time elapsed = ~w secs~n", [Time/1000000]),
  {ok, Msg}.

  
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
