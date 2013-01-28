-module('_conc').
-compile(export_all).

-behaviour(gen_server).

%% External exports
-export([init_concolic/4]).

%% gen_server callbacks
-export([init/1, terminate/2]).


-record(state, {
  coord,
  cpids,
  tpids,
  results,
  int
}).


%%====================================================================
%% External exports
%%====================================================================

init_concolic(M, F, As, CoreDir) ->
  case gen_server:start_link(?MODULE, [M, F, As, CoreDir, self()], []) of
    {ok, Concolic}  -> Concolic;
    {error, Reason} -> {error, Reason}
  end.
  
%%====================================================================
%% gen_server callbacks
%%====================================================================

init([M, F, As, CoreDir, Coord]) ->
  process_flag(trap_exit, true),
  Node = node(),
  CodeServer = conc_cserver:init_codeserver(CoreDir),
  TraceServer = conc_tserver:init_traceserver(),
  {Ipid, Iref} = conc_eval:i(M, F, As, CodeServer, TraceServer),
  InitState = #state{
    coord = Coord,
    cpids = [{Node, CodeServer}],
    tpids = [{Node, TraceServer}],
    results = orddict:new(),
    int = {Ipid, Iref}
  },
  {ok, InitState}.
  
terminate(_Reason, State) ->
  Coord = State#state.coord,
  Results = State#state.results,
  Coord ! Results.
  
  
  
  
  


