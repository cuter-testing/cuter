-module(conc).
-compile([export_all]).

start(M, F, As) ->
  
  %% Start Code and Trace Servers
  Dir = filename:absname("core"),
  {ok, CodeServer} = gen_server:start_link(conc_cserver, [Dir], []),  % Start code server
  {ok, TraceServer} = gen_server:start_link(conc_tserver, [self()], []),           % Start trace server
  
  Start = now(),
%  eprof:start(),
%  eprof:start_profiling([self()]),
  Result = conc_eval:i(M, F, As, CodeServer, TraceServer),
  io:format("Result = ~p~n", [Result]),
  receive
    {TraceServer, Msg} ->
%      eprof:stop_profiling(),
%      eprof:analyze(),
%      eprof:stop(),
      gen_server:call(CodeServer, terminate),
      End  = now(),
      Time = timer:now_diff(End, Start),
      io:format("Time elapsed = ~w secs~n", [Time/1000000]),
      {ok, Msg}
  end.

