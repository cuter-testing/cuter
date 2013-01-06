-module(conc_cserver).
-behaviour(gen_server).

%% External exports
%-export([call/1, cast/1]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3, handle_info/2,
  handle_call/3, handle_cast/2]).

%% gen_server state datatype
-record(state, {
  %%-- Modules' database -------------------
  %% Creates an ETS table where it stores:
  %% {Key, Value} -> {Module, ModuleDb}
  %% ModDb :: ets:tid()
  db,      % ETS table :: ets:tid()
  intMode, % modules that can be loaded // all, [modules]
  dir      % Directory where .core files are saved
}).



%%====================================================================
%% External exports
%%====================================================================


%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Mode, Dir]) ->
  Db = ets:new(?MODULE, [ordered_set, protected]),
  case Mode of
    global ->
      {ok, #state{db=Db, intMode=all, dir=Dir}};
    {limited, ModList} when is_list(ModList) ->
      {ok, #state{db=Db, intMode=ModList, dir=Dir}}
  end.
  
terminate(_Reason, State) ->
  Db = State#state.db,
  Dir = State#state.dir,
  %% Delete all created ETS tables
  delete_stored_modules(Db),
  ets:delete(Db),
  %% Clean up .core files dir
  delete_stored_core_files(Dir),
  ok.
  
code_change(_OldVsn, State, _Extra) ->
  %% No change planned.
  {ok, State}.
  
handle_info(Msg, State) ->
  %% Just outputting unexpected messages for now
  io:format("[conc_cserver]: Unexpected message ~p~n", [Msg]),
  {noreply, State}.
  
%% Handle a "Load a Module into the Db" call
%%   Case                           Reply
%% --------                       ---------
%% Module is already loaded  -->  {ok, {Mod, ModDb}
%% Module is now loaded      -->  {ok, {Mod, ModDb}
%% Module is unloadable      -->  unloadable
%% Module is non_existing    -->  non_existing
handle_call({load, Mod}, _From, State) ->

  case is_mod_stored(Mod, State) of
    {true, ModDb} ->
      {reply, {ok, {Mod, ModDb}}, State};
    false ->
      %% Create an ETS table to store the code of the module
      Db = State#state.db,
      ModDb = ets:new(Mod, [ordered_set, protected]),
      ets:insert(Db, {Mod, ModDb}),
      
      %% Load the code of the module
      Dir = State#state.dir,
      Reply = conc_load:load(Mod, ModDb, Dir),
      
      %% Reply
      case Reply of
        {ok, Mod} ->
          {reply, {ok, {Mod, ModDb}}, State};
        _ ->
          {reply, Reply, State}
      end;
    unloadable ->
      {reply, unloadable, State};
    non_existing ->
      {reply, non_existing, State}
  end;
  
%% Handle a "Is a Module stored in the Db?" call
handle_call({is_stored, Mod}, _From, State) ->
  {reply, is_mod_stored(Mod, State), State};
  
%% Handle a "Stop code server" call
handle_call(terminate, _From, State) ->
  {stop, normal, stopped, State}.
  
handle_cast(_Msg, State) ->
  %% Not handling any casts
  {noreply, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% Check if a Module is stored in the Db
%%   Case                           Reply
%% --------                       ---------
%% Module is already loaded  -->  {true, ModDb}
%% Module is not loaded yet  -->  false
%% Module is unloadable      -->  unloadable
%% Module does not exist     -->  non_existing
is_mod_stored(Mod, State) ->
  case can_be_loaded(Mod, State) of
    true ->
      Db = State#state.db,
      case ets:lookup(Db, Mod) of
        [] -> false;
        [{Mod, ModDb}] -> {true, ModDb}
      end;
    false ->
      case code:which(Mod) of
        non_existing ->
          non_existing;
        _ -> 
          unloadable
      end
  end.

%% can_be_loaded(Mod, State) -> boolean()
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
  
delete_stored_core_files(Dir) ->
  {ok, Filenames} = file:list_dir(Dir),
  lists:map(
    fun(File) ->
      ok = file:delete(Dir ++ "/" ++ File)
    end,
  Filenames).
  

