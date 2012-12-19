-module(conc).
-compile([export_all]).

-include_lib("compiler/src/core_parse.hrl").

start(Mod, Fun, Args)
  when is_atom(Mod); is_list(Mod) ->
  
    %% Start Code and Trace Servers
    {ok, CodeServer} = gen_server:start_link(conc_cserver, [{limited, [foo]}], []),  % Start code server
    {ok, TraceServer} = gen_server:start_link(conc_tserver, [self()], []),           % Start trace server
    
    StartArgs = [Mod, Fun, Args, concrete, external, self(), CodeServer, TraceServer, true],
    spawn(conc_eval, eval, StartArgs),
%    io:format("[conc]: Spawned ~p~n", [P]),
%    receive
%      B ->
%        io:format("[conc]: Concrete Eval of ~p:~p(~p) =~n~p~n", [Mod, Fun, Args, B])
%    end,

%    gen_server:call(TraceServer, terminate),

    receive
      {TraceServer, Msg} ->
        gen_server:call(CodeServer, terminate),
        {ok, Msg}
    end;
%    {ok, Res};

start(_, _, _) ->
  {error, invalid_module_name}.

%    A = gen_server:call(CodeServer, {is_stored, Mod}),
%    io:format("[conc]: Reply to {is_stored, ~p}: ~p~n", [Mod, A]),
%    Msg = gen_server:call(CodeServer, {load, Mod}),
%    io:format("[conc]: Reply to {load, ~p}: ~p~n", [Mod, Msg]),

