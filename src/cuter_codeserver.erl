%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_codeserver).
-behaviour(gen_server).

%% external exports
-export([start/2, start/4, stop/1, load/2, unsupported_mfa/2, retrieve_spec/2, visit_tag/2,
         merge_dumped_cached_modules/2, no_cached_modules/0, initial_branch_counter/0,
         lookup_in_module_cache/2, insert_in_module_cache/3]).
%% gen_server callbacks
-export([init/1, terminate/2, code_change/3, handle_info/2, handle_call/3, handle_cast/2]).
%% counter of branches
-export([set_branch_counter/1, generate_tag/0]).

-include("include/cuter_macros.hrl").

-export_type([cached_modules/0, counter/0, module_cache/0]).

%% Macros
-define(BRANCH_COUNTER_PREFIX, '__branch_count').

-type counter() :: non_neg_integer().
-type cached_module_data() :: any().
-type cached_modules() :: dict:dict(module(), cached_module_data()).

-type module_cache() :: ets:tid().
-type cache() :: ets:tid().

%% Internal type declarations
-type load_reply() :: {ok, ets:tid()} | cuter_cerl:compile_error() | {error, (preloaded | cover_compiled | non_existing)}.
-type spec_reply() :: {ok, cuter_types:erl_spec()} | error.

%% Server's state
%% ---------------
%%
%% super :: pid()
%%   The supervisor process that spawned the codeserver
%% db :: ets:tid()
%%   Acts as a reference table for looking up the ETS table that holds a module's extracted code.
%%   It stores tuples {Module :: module(), ModuleDb :: ets:tid()}.
%% waiting :: orddict()
%%   The processes that await a response and their requests.
%%   Each element in the dictionary is {{Request :: atom(), Info :: tuple()}, Process :: pid()}
%% workers :: [pid()]
%%   PIDs of all worker processes

-record(st, {
  super                          :: pid(),
  db                             :: cache(),
  waiting = orddict:new()        :: [{{atom(), tuple()}, pid()}],
  workers = []                   :: [pid()],
  unsupported_mfas = sets:new()  :: sets:set(mfa()),
  tags = gb_sets:new()           :: gb_sets:set(cuter_cerl:tagID()),
  with_pmatch                    :: boolean()
}).
-type state() :: #st{}.

%% ============================================================================
%% External exports (Public API)
%% ============================================================================

%% Starts a CodeServer in the local node.
-spec start(pid(), boolean()) -> pid().
start(Super, WithPmatch) ->
  start(Super, no_cached_modules(), initial_branch_counter(), WithPmatch).

%% Start a CodeServer
-spec start(pid(), cached_modules(), counter(), boolean()) -> pid().
start(Super, StoredMods, TagsN, WithPmatch) ->
  case gen_server:start(?MODULE, [Super, StoredMods, TagsN, WithPmatch], []) of
    {ok, CodeServer} -> CodeServer;
    {error, Reason}  -> exit({codeserver_start, Reason})
  end.

%% Stop a CodeServer
-spec stop(pid()) -> ok.
stop(CodeServer) ->
  gen_server:cast(CodeServer, {stop, self()}).

%% Request the ETS table where the code of module M is stored
-spec load(pid(), module()) -> load_reply().
load(CodeServer, M) ->
  gen_server:call(CodeServer, {load, M}).

%% Log an MFA that cannot be symbolically evaluated.
-spec unsupported_mfa(pid(), mfa()) -> ok.
-ifdef(LOG_UNSUPPORTED_MFAS).
unsupported_mfa(CodeServer, MFA) ->
  gen_server:cast(CodeServer, {unsupported_mfa, MFA}).
-else.
unsupported_mfa(_, _) -> ok.
-endif.

%% Retrieves the spec of a given MFA.
-spec retrieve_spec(pid(), mfa()) -> spec_reply().
retrieve_spec(CodeServer, MFA) ->
  gen_server:call(CodeServer, {get_spec, MFA}).

%% Reports visiting a tag.
-spec visit_tag(pid(), cuter_cerl:tag()) -> ok.
visit_tag(CodeServer, Tag) ->
  gen_server:cast(CodeServer, {visit_tag, Tag}).

%% ============================================================================
%% gen_server callbacks (Server Implementation)
%% ============================================================================

%% gen_server callback : init/1
-spec init([pid() | cached_modules() | counter() | boolean(), ...]) -> {ok, state()}.
init([Super, CachedMods, TagsN, WithPmatch]) ->
  link(Super),
  Db = ets:new(?MODULE, [ordered_set, protected]),
  add_cached_modules(Db, CachedMods),
  _ = set_branch_counter(TagsN), %% Initialize the counter for the branch enumeration.
  {ok, #st{db = Db, super = Super, with_pmatch = WithPmatch}}.

%% gen_server callback : terminate/2
-spec terminate(any(), state()) -> ok.
terminate(_Reason, #st{db = Db, super = Super, unsupported_mfas = Ms, tags = Tags}) ->
  Cnt = get_branch_counter(),
  CachedMods = dump_cached_modules(Db),
  Logs = [ {loaded_mods, drop_module_cache(Db)}  % Delete all created ETS tables
         , {unsupported_mfas, sets:to_list(Ms)}
         , {visited_tags, Tags}
         , {stored_mods, CachedMods}
         , {tags_added_no, Cnt}
         ],
  ok = cuter_iserver:code_logs(Super, orddict:from_list(Logs)),  % Send logs to the supervisor
  ok.

%% gen_server callback : code_change/3
-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.  %% No change planned.

%% gen_server callback : handle_info/2
-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(Msg, State) ->
  %% Just outputting unexpected messages for now
  io:format("[~s]: Unexpected message ~p~n", [?MODULE, Msg]),
  {noreply, State}.

%% gen_server callback : handle_call/3
-spec handle_call({load, module()}, {pid(), reference()}, state()) -> {reply, load_reply(), state()}
               ; ({get_spec, mfa()}, {pid(), reference()}, state()) -> {reply, spec_reply(), state()}.
handle_call({load, M}, _From, State) ->
  {reply, try_load(M, State), State};
handle_call({get_spec, {M, F, A}=MFA}, _From, State) ->
  case try_load(M, State) of
    {ok, MDb} ->
      case cuter_cerl:retrieve_spec(MDb, {F, A}) of
        error ->
          cuter_pp:error_retrieving_spec(MFA, not_found),
          {reply, error, State};
        {ok, CerlSpec} ->
          Types = cuter_cerl:get_stored_types(MDb),
          case cuter_types:parse_spec({F, A}, CerlSpec, Types) of
            {error, has_remote_types} ->
              cuter_pp:error_retrieving_spec(MFA, has_remote_types),
              {reply, error, State};
            {error, recursive_type} ->
              cuter_pp:error_retrieving_spec(MFA, recursive_type),
              {reply, error, State};
            {error, unsupported_type, Name} ->
              cuter_pp:error_retrieving_spec(MFA, {unsupported_type, Name}),
              {reply, error, State};
            {ok, _Spec}=OK ->
              {reply, OK, State}
          end
      end;
    Msg ->
      cuter_pp:error_retrieving_spec(MFA, Msg),
      {reply, error, State}
  end.

%% gen_server callback : handle_cast/2
-spec handle_cast({stop, pid()}, state()) -> {stop, normal, state()} | {noreply, state()}
               ; ({unsupported_mfa, mfa()}, state()) -> {noreply, state()}
               ; ({visit_tag, cuter_cerl:tag()}, state()) -> {noreply, state()}.
handle_cast({unsupported_mfa, MFA}, State=#st{unsupported_mfas = Ms}) ->
  Ms1 = sets:add_element(MFA, Ms),
  {noreply, State#st{unsupported_mfas = Ms1}};
handle_cast({stop, FromWho}, State=#st{super = Super}) ->
  case FromWho =:= Super of
    true  -> {stop, normal, State};
    false -> {noreply, State}
  end;
handle_cast({visit_tag, Tag}, State=#st{tags = Tags}) ->
  Ts = gb_sets:add_element(cuter_cerl:id_of_tag(Tag), Tags),
  {noreply, State#st{tags = Ts}}.

%% ============================================================================
%% Internal functions (Helper methods)
%% ============================================================================

%% Populates the DB with the stored modules provided.
-spec add_cached_modules(cache(), cached_modules()) -> ok.
add_cached_modules(Db, CachedMods) ->
  dict:fold(
    fun(M, Info, CDb) ->
      MDb = ets:new(M, [ordered_set, protected]),
      ets:insert(MDb, Info),
      ets:insert(CDb, {M, MDb}),
      CDb
    end,
    Db,
    CachedMods
  ),
  ok.

-spec try_load(module(), state()) -> load_reply().
try_load(M, State) ->
  case is_mod_stored(M, State) of
    {true, MDb}     -> {ok, MDb};
    {false, eexist} -> load_mod(M, State); %% Load module M
    {false, Msg}    -> {error, Msg}
  end.

%% Load a module's code
-spec load_mod(module(), state()) -> {ok, module_cache()} | cuter_cerl:compile_error().
load_mod(M, #st{db = Db, with_pmatch = WithPmatch}) ->
  MDb = ets:new(M, [ordered_set, protected]),  %% Create an ETS table to store the code of the module
  ets:insert(Db, {M, MDb}),                    %% Store the tid of the ETS table
  Reply = cuter_cerl:load(M, MDb, fun generate_tag/0, WithPmatch),  %% Load the code of the module
  case Reply of
    {ok, M} -> {ok, MDb};
    _ -> Reply
  end.

%% Check if a Module is stored in the Db
-spec is_mod_stored(atom(), state()) -> {true, module_cache()} | {false, eexist | loaded_ret_atoms()}.
is_mod_stored(M, #st{db = Db}) ->
  case ets:lookup(Db, M) of
    [{M, MDb}] -> {true, MDb};
    [] ->
      case code:which(M) of
        non_existing   -> {false, non_existing};
        preloaded      -> {false, preloaded};
        cover_compiled -> {false, cover_compiled};
        _Path          -> {false, eexist}
      end
  end.

%% ============================================================================
%% Manage module cache
%% ============================================================================

%% Looks up data in a module's cache.
-spec lookup_in_module_cache(any(), module_cache()) -> any().
lookup_in_module_cache(Key, Cache) ->
  [{Key, Value}] = ets:lookup(Cache, Key),
  Value.

%% Inserts data in a module's cache.
-spec insert_in_module_cache(any(), any(), module_cache()) -> ok.
insert_in_module_cache(Key, Data, Cache) ->
  true = ets:insert(Cache, {Key, Data}),
  ok.

%% Drops the cache of the CodeServer.
%% Deletes all modules' caches and the mapping of a module to its cache.
-spec drop_module_cache(cache()) -> [module()].
drop_module_cache(Db) ->
  ModNames = ets:foldl(fun({M, MDb}, Ms) -> ets:delete(MDb), [M|Ms] end, [], Db),
  ets:delete(Db),
  ModNames.

%% Dumps the cached modules' info into a dict.
-spec dump_cached_modules(cache()) -> cached_modules().
dump_cached_modules(Db) ->
  ets:foldl(fun({M, MDb}, Stored) -> dict:store(M, ets:tab2list(MDb), Stored) end, dict:new(), Db).

%% Creates an empty modules' cache.
-spec no_cached_modules() -> cached_modules().
no_cached_modules() -> dict:new().

%% Merges two caches.
-spec merge_dumped_cached_modules(cached_modules(), cached_modules()) -> cached_modules().
merge_dumped_cached_modules(Cached1, Cached2) ->
  dict:merge(fun(_Mod, Data1, _Data2) -> Data1 end, Cached1, Cached2).

%% ============================================================================
%% Counter for branch enumeration
%% ============================================================================

-spec initial_branch_counter() -> 0.
initial_branch_counter() -> 0.

-spec get_branch_counter() -> counter().
get_branch_counter() ->
  get(?BRANCH_COUNTER_PREFIX).

-spec set_branch_counter(counter()) -> counter() | undefined.
set_branch_counter(N) ->
  put(?BRANCH_COUNTER_PREFIX, N).

-spec generate_tag() -> cuter_cerl:tag().
generate_tag() ->
  N = get(?BRANCH_COUNTER_PREFIX) + 1,
  put(?BRANCH_COUNTER_PREFIX, N),
  cuter_cerl:tag_from_id(N).
