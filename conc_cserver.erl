-module(conc_cserver).
-behaviour(gen_server).

%% External exports
%-export([call/1, cast/1]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3, handle_info/2,
  handle_call/3, handle_cast/2]).

%% gen_server state datatype
-record(state, {
  %%-- Module database -------------------
  %% Creates an ETS table where it stores:
  %% {Key, Value} -> {Module, ModuleDb}
  %% ModDb :: ets:tid()
  db,      % ETS table :: ets:tid()
  intMode  % modules that can be loaded // all, [modules]
}).



%%====================================================================
%% External exports
%%====================================================================

%call(Request) ->
%  gen_server:call(?MODULE, Request).
  
%cast(Request) ->
%  gen_server:cast(?MODULE, Request).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Mode]) ->
  Db = ets:new(?MODULE, [ordered_set, protected]),
  case Mode of
    global ->
      {ok, #state{db=Db, intMode=all}};
    {limited, ModList} when is_list(ModList) ->
      {ok, #state{db=Db, intMode=ModList}}
  end.
  
terminate(_Reason, State) ->
  Db = State#state.db,
  delete_stored_modules(Db),
  ets:delete(Db),
  ok.
  
code_change(_OldVsn, State, _Extra) ->
  %% No change planned.
  {ok, State}.
  
handle_info(Msg, State) ->
  %% Just outputting unexpected messages for now
  io:format("[conc_server]: Unexpected message ~p~n", [Msg]),
  {noreply, State}.
  
%% Load a Module into the Db
handle_call({load, Mod}, _From, State) ->

  case can_be_loaded(Mod, State) of
    true ->
      %% Create an ETS table to store the code of the module
      Db = State#state.db,
      ModDb = ets:new(Mod, [ordered_set, public]),
      ets:insert(Db, {Mod, ModDb}),
      
      %% Load the code of the module
      Reply = conc_load:load(Mod, ModDb),
      
      %% Reply
      case Reply of
        {ok, Mod} ->
          {reply, {ok, Mod, ModDb}, State};
        _ ->
          {reply, Reply, State}
      end;
      
    false ->
      {reply, unloadable, State}
  end;
  
%% Check if a Module is Stored in the Db
handle_call({is_stored, Mod}, _From, State) ->
  case can_be_loaded(Mod, State) of
    true ->
      Db = State#state.db,
      Reply = 
        case ets:lookup(Db, Mod) of
          [] -> false;
          [{Mod, ModDb}] -> {true, ModDb}
        end,
      {reply, Reply, State};
    false ->
      {reply, unloadable, State}
  end;
  
%% Stopping gen_server
handle_call(terminate, _From, State) ->
  {stop, normal, stopped, State}.
  
handle_cast(_Msg, State) ->
  {noreply, State}.

%%====================================================================
%% Internal functions
%%====================================================================

can_be_loaded(Mod, State) ->
  Mode = State#state.intMode,
  case Mode of
    all ->
      true;
    ModList ->
      lists:member(Mod, ModList)
  end.

delete_stored_modules(Db) ->
  DeleteOne = 
    fun ({Mod, ModDb}, Acc) ->
      ets:delete(ModDb),
      [Mod | Acc]
    end,
  ets:foldl(DeleteOne, [], Db).
  

