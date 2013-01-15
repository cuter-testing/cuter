-module(conc_cserver).
-behaviour(gen_server).

%% External exports
-export([init_codeserver/1]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3, handle_info/2,
  handle_call/3, handle_cast/2]).

%% gen_server state datatype
-record(state, {
  %%-- Modules' database -------------------
  %% Creates an ETS table where it stores:
  %% {Key, Value} -> {Module, ModuleDb}
  %% ModDb :: ets:tid()
  db,      %% ETS table :: ets:tid()
  dir,     %% Directory where .core files are saved
  super    %% Supervisor process :: pid()
  
}).

%%====================================================================
%% External exports
%%====================================================================

init_codeserver(CoreDir) ->
  {ok, CodeServer} = gen_server:start_link(?MODULE, [CoreDir, self()], []),
  CodeServer.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Dir, Super]) ->
  Db = ets:new(?MODULE, [ordered_set, protected]),
  {ok, #state{db=Db, dir=Dir, super=Super}}.
  
terminate(_Reason, State) ->
  Db = State#state.db,
  Dir = State#state.dir,
  Super = State#state.super,
  %% Delete all created ETS tables
  LoadedMods = delete_stored_modules(Db),
  ets:delete(Db),
  %% Clean up .core files dir
  delete_stored_core_files(Dir),
  %% Send statistics to supervisor
  Super ! {self(), LoadedMods},
  ok.
  
code_change(_OldVsn, State, _Extra) ->
  %% No change planned.
  {ok, State}.
  
handle_info(Msg, State) ->
  %% Just outputting unexpected messages for now
  io:format("[conc_cserver]: Unexpected message ~p~n", [Msg]),
  {noreply, State}.
  
%% Handle a "Load a Module into the Db" call
%%   Case                             Reply
%% --------                         ---------
%% Module M is already loaded  -->  {ok, MDb}
%% Module M is not loaded yet  -->  false
%% Module M is preloaded       -->  preloaded
%% Module M is cover_compiled  -->  cover_compiled
%% Module M does not exist     -->  non_existing
handle_call({load, M}, _From, State) ->
%  io:format("[load]: Got request for module : ~p~n", [M]),
  case is_mod_stored(M, State) of
    {true, MDb} ->
      {reply, {ok, MDb}, State};
    false ->
      %% Create an ETS table to store the code of the module
      Db = State#state.db,
      MDb = ets:new(M, [ordered_set, protected]),
      ets:insert(Db, {M, MDb}),
      
      %% Load the code of the module
      Dir = State#state.dir,
      Reply = conc_load:load(M, MDb, Dir),
      
      %% Reply
      case Reply of
        {ok, M} ->
%          io:format("[load]: Loaded module ~p~n", [M]),
          {reply, {ok, MDb}, State};
        _ ->
          {reply, Reply, State}
      end;
    preloaded ->
      {reply, preloaded, State};
    cover_compiled ->
      {reply, cover_compiled, State};
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
%%   Case                             Reply
%% --------                         ---------
%% Module M is already loaded  -->  {true, MDb}
%% Module M is not loaded yet  -->  false
%% Module M is preloaded       -->  preloaded
%% Module M is cover_compiled  -->  cover_compiled
%% Module M does not exist     -->  non_existing
is_mod_stored(M, State) ->
  Db = State#state.db,
  case ets:lookup(Db, M) of
    [{M, MDb}] ->
      {true, MDb};
    [] ->
      case code:which(M) of
        non_existing   -> non_existing;
        preloaded      -> preloaded;
        cover_compiled -> cover_compiled;
        _Path          -> false
      end
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
      file:delete(Dir ++ "/" ++ File)
    end,
  Filenames),
  file:del_dir(Dir).
