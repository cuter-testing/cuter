%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(concolic_cserver).
-behaviour(gen_server).

%% External exports
-export([init_codeserver/2, terminate/1, load/2]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3, handle_info/2,
         handle_call/3, handle_cast/2]).

%% gen_server state datatype
-record(state, {
  %%-- Modules' database -------------------
  %% Creates an ETS table where it stores:
  %% {Key, Value} -> {Module, ModuleDb}
  %%   Module   :: atom()
  %%   ModuleDb :: ets:tab()
  db    :: ets:tab(),      %% Database of the modules and their stored code
  dir   :: string(),       %% Directory where .core files are saved
  super :: pid()           %% Concolic Server (supervisor) process
}).
-type state() :: #state{}.

-type load_request() :: {'load', atom()}.
-type load_reply() :: {'ok', ets:tab()} | concolic_load:compile_error() | 'preloaded' | 'cover_compiled' | 'non_existing'.

%%====================================================================
%% External exports
%%====================================================================

%% Initialize a CodeServer
-spec init_codeserver(CoreDir :: string(), Super :: pid()) -> CodeServer :: pid().

init_codeserver(CoreDir, Super) ->
  case gen_server:start(?MODULE, [CoreDir, Super], []) of
    {ok, CodeServer} -> CodeServer;
    {error, Reason}  -> exit({codeserver_init, Reason})
  end.
  
%% Terminate a CodeServer
-spec terminate(CodeServer :: pid()) -> 'ok'.

terminate(CodeServer) ->
  gen_server:cast(CodeServer, {terminate, self()}).
  
%% Request the ETS table where the code of a module M is stored
-spec load(CodeServer :: pid(), M :: atom()) -> Msg :: load_reply().

load(CodeServer, M) ->
  gen_server:call(CodeServer, {load, M}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% ---------------------------------------------------------------------
%% gen_server callback : init/1
%% ---------------------------------------------------------------------
-spec init([(string() | pid())]) -> {'ok', state()}.

init([Dir, Super]) when is_list(Dir) ->
  link(Super),
  Db = ets:new(?MODULE, [ordered_set, protected]),
  U = erlang:ref_to_list(erlang:make_ref()),
  CoreDir = filename:absname(Dir ++ "/core-" ++ U),
  {ok, #state{db=Db, dir=CoreDir, super=Super}}.

%% ---------------------------------------------------------------------
%% gen_server callback : terminate/2
%% ---------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: state()) -> 'ok'.

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
  ok = concolic:send_clogs(Super, LoadedMods).
  
%% ---------------------------------------------------------------------
%% gen_server callback : code_change/3
%% ---------------------------------------------------------------------
-spec code_change(term(), State :: state(), term()) -> {'ok', State :: state()}.
  
code_change(_OldVsn, State, _Extra) ->
  %% No change planned.
  {ok, State}.
  
%% ---------------------------------------------------------------------
%% gen_server callback : handle_info/2
%% ---------------------------------------------------------------------
-spec handle_info(Msg :: term(), state()) -> {'noreply', state()}.
  
handle_info(Msg, State) ->
  %% Just outputting unexpected messages for now
  io:format("[~s]: Unexpected message ~p~n", [?MODULE, Msg]),
  {noreply, State}.
  
%% ---------------------------------------------------------------------
%% gen_server callback : handle_call/3
%% ---------------------------------------------------------------------
-spec handle_call(Req :: load_request(), From :: {pid(), reference()}, State :: state()) ->
  {'reply', Reply :: load_reply(), NewState :: state()}.
  
%% Handle a "Load a Module into the Db" call
%%   Case                             Reply
%% --------                         ---------
%% Module M is already loaded  -->  {ok, MDb}
%% Module M is not loaded yet  -->  false
%% Module M is preloaded       -->  preloaded
%% Module M is cover_compiled  -->  cover_compiled
%% Module M does not exist     -->  non_existing
handle_call({load, M}, _From, State) ->
  %%  io:format("[load]: Got request for module : ~p~n", [M]),
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
      Reply = concolic_load:load(M, MDb, Dir),
      case Reply of
        {ok, M} ->
	  %%  io:format("[load (~w)]: Loaded module ~p~n", [node(), M]),
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
  end.
  
%% ---------------------------------------------------------------------
%% gen_server callback : handle_cast/2
%% ---------------------------------------------------------------------
-spec handle_cast(Msg :: term(), state()) -> {'stop', 'normal', state()} | {'noreply', state()}.
  
%% Cast Request : {terminate, FromWho}
handle_cast({terminate, FromWho}, State) ->
  Super = State#state.super,
  case FromWho =:= Super of
    true  -> {stop, normal, State};
    false -> {noreply, State}
  end.

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
-spec is_mod_stored(M :: atom(), State :: state()) ->
  {'true', MDb :: ets:tab()} | 'false' | 'preloaded' | 'cover_compiled' | 'non_existing'.
  
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

%% Delete all ETS tables that contain the code of modules
-spec delete_stored_modules(Db :: ets:tab()) -> Mods :: [atom()].
  
delete_stored_modules(Db) ->
  DeleteOne = 
    fun ({Mod, ModDb}, Acc) ->
      ets:delete(ModDb),
      [Mod | Acc]
    end,
  ets:foldl(DeleteOne, [], Db).
  
%% Delete all the created .core files during the execution
-spec delete_stored_core_files(Dir :: file:name()) -> 'ok'.
  
delete_stored_core_files(Dir) ->
  case file:list_dir(Dir) of
    {ok, FileNames} ->
      lists:foreach(fun(FN) -> file:delete(Dir ++ "/" ++ FN) end, FileNames),
      ok = file:del_dir(Dir);
    {error, enoent} ->
      ok
  end.
  
