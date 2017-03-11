%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_codeserver).

-behaviour(gen_server).

%% external exports
-export([start/4, start/6, stop/1, load/2, unsupported_mfa/2, retrieve_spec/2,
         get_feasible_tags/2, get_logs/1, get_whitelist/1, get_visited_tags/1,
         visit_tag/2, calculate_callgraph/2,
         %% Work with module cache
         merge_dumped_cached_modules/2, modules_of_dumped_cache/1,
         lookup_in_module_cache/2, insert_in_module_cache/3,
         no_cached_modules/0,
         %% Access logs
         cachedMods_of_logs/1, visitedTags_of_logs/1, tagsAddedNo_of_logs/1,
         unsupportedMfas_of_logs/1, loadedMods_of_logs/1]).
%% gen_server callbacks
-export([init/1, terminate/2, code_change/3,
         handle_info/2, handle_call/3, handle_cast/2]).
%% Counter of branches & Tag generator.
-export([set_branch_counter/1, get_branch_counter/0, initial_branch_counter/0,
         generate_tag/0]).

-include("include/cuter_macros.hrl").

-export_type([cached_modules/0, counter/0, module_cache/0, logs/0]).

%% Macros
-define(BRANCH_COUNTER_PREFIX, '__branch_count').

-type counter() :: non_neg_integer().
-type cached_module_data() :: any().
-type cached_modules() :: dict:dict(module(), cached_module_data()).

-type module_cache() :: ets:tid().
-type cache() :: ets:tid().

-type modules() :: [cuter:mod()].
-type mfas() :: [mfa()].
-type tags() :: gb_sets:set(cuter_cerl:tagID()).

%% The logs of a CodeServer.
-record(logs, {
  cachedMods      :: cached_modules(),
  loadedMods      :: modules(),
  tagsAddedNo     :: counter(),
  visitedTags     :: tags(),
  unsupportedMfas :: mfas()
}).
-type logs() :: #logs{}.

%% Internal type declarations
-type load_reply() :: {ok, module_cache()} | cuter_cerl:compile_error() | {error, (preloaded | cover_compiled | non_existing)}.
-type spec_reply() :: {ok, cuter_types:erl_spec()} | error.
-type from() :: {pid(), reference()}.

%% Finding the remote dependencies of a spec.
-type remote_type()     :: {cuter:mod(), atom(), byte()}.
-type module_deps()     :: ordsets:ordset(cuter:mod()).
-type visited_remotes() :: ordsets:ordset(remote_type()).

%% Server's state
%% ---------------
%%
%% db :: ets:tid()
%%   Acts as a reference table for looking up the ETS table that holds a module's extracted code.
%%   It stores tuples {Module :: module(), ModuleDb :: ets:tid()}.
%% super :: pid()
%%   The supervisor process that spawned the codeserver.
%% tags :: tags()
%%   The visited tags.
%% waiting :: orddict()
%%   The processes that await a response and their requests.
%%   Each element in the dictionary is {{Request :: atom(), Info :: tuple()}, Process :: pid()}.
%% withPmatch :: boolean()
%%   Whether to use pattern matching compilation optimization or not.
%% workers :: [pid()]
%%   PIDs of all worker processes.
%% unsupportedMfas :: sets:set(mfa())
%%   The set of mfa() that are not supported for symbolic execution but were
%%   encountered during the concolic executions.

-record(st, {
  db                           :: cache(),
  super                        :: pid(),
  tags = gb_sets:new()         :: tags(),
  waiting = orddict:new()      :: [{{atom(), tuple()}, pid()}],
  withPmatch                   :: boolean(),
  workers = []                 :: [pid()],
  unsupportedMfas = sets:new() :: sets:set(mfa()),
  whitelist                    :: cuter_mock:whitelist(),
  callgraph                    :: cuter_callgraph:callgraph() | 'undefined',
  normalizeTypes               :: boolean()
}).
-type state() :: #st{}.

%% ----------------------------------------------------------------------------
%% Public API
%% ----------------------------------------------------------------------------

%% Starts a CodeServer in the local node.
-spec start(pid(), boolean(), cuter_mock:whitelist(), boolean()) -> pid().
start(Super, WithPmatch, Whitelist, NormalizeTypes) ->
  start(Super, no_cached_modules(), initial_branch_counter(), WithPmatch, Whitelist, NormalizeTypes).

%% Starts a CodeServer in the local node with an initialized
%% modules' cache and tag counter.
-spec start(pid(), cached_modules(), counter(), boolean(), cuter_mock:whitelist(), boolean()) -> pid().
start(Super, StoredMods, TagsN, WithPmatch, Whitelist, NormalizeTypes) ->
  case gen_server:start(?MODULE, [Super, StoredMods, TagsN, WithPmatch, Whitelist, NormalizeTypes], []) of
    {ok, CodeServer} -> CodeServer;
    {error, Reason}  -> exit({codeserver_start, Reason})
  end.

%% Stops a CodeServer.
-spec stop(pid()) -> ok.
stop(CodeServer) ->
  gen_server:cast(CodeServer, {stop, self()}).

%% Requests a module's cache.
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

%% Gets the visited tags.
-spec get_visited_tags(pid()) -> tags().
get_visited_tags(CodeServer) ->
  gen_server:call(CodeServer, get_visited_tags).

%% Gets the logs of the CodeServer.
-spec get_logs(pid()) -> logs().
get_logs(CodeServer) ->
  gen_server:call(CodeServer, get_logs).

%% Gets the whitelisted MFAs.
-spec get_whitelist(pid()) -> cuter_mock:whitelist().
get_whitelist(CodeServer) ->
  gen_server:call(CodeServer, get_whitelist).

%% Calculates the callgraph from some MFAs.
-spec calculate_callgraph(pid(), [mfa()]) -> ok | error.
calculate_callgraph(CodeServer, Mfas) ->
  gen_server:call(CodeServer, {calculate_callgraph, Mfas}).

%% Gets the feasible tags.
-spec get_feasible_tags(pid(), cuter_cerl:node_types()) -> cuter_cerl:visited_tags().
get_feasible_tags(CodeServer, NodeTypes) ->
  gen_server:call(CodeServer, {get_feasible_tags, NodeTypes}).

%% ----------------------------------------------------------------------------
%% gen_server callbacks (Server Implementation)
%% ----------------------------------------------------------------------------

%% gen_server callback : init/1
-spec init([pid() | cached_modules() | counter() | boolean() | cuter_mock:whitelist(), ...]) -> {ok, state()}.
init([Super, CachedMods, TagsN, WithPmatch, Whitelist, NormalizeTypes]) ->
  link(Super),
  Db = ets:new(?MODULE, [ordered_set, protected]),
  add_cached_modules(Db, CachedMods),
  _ = set_branch_counter(TagsN), %% Initialize the counter for the branch enumeration.
  {ok, #st{ db = Db
          , super = Super
          , withPmatch = WithPmatch
          , whitelist = Whitelist
          , normalizeTypes = NormalizeTypes}}.

%% gen_server callback : terminate/2
-spec terminate(any(), state()) -> ok.
terminate(_Reason, #st{db = Db}) ->
  _ = drop_module_cache(Db),
  ok.

%% gen_server callback : code_change/3
-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.  %% No change planned.

%% gen_server callback : handle_info/2
-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(_Msg, State) ->
  {noreply, State}.

%% gen_server callback : handle_call/3
-spec handle_call({load, module()}, from(), state()) -> {reply, load_reply(), state()}
               ; ({get_spec, mfa()}, from(), state()) -> {reply, spec_reply(), state()}
               ; (get_visited_tags, from(), state()) -> {reply, tags(), state()}
               ; (get_logs, from(), state()) -> {reply, logs(), state()}
               ; (get_whitelist, from(), state()) -> {reply, cuter_mock:whitelist(), state()}
               ; ({get_feasible_tags, cuter_cerl:node_types()}, from(), state()) -> {reply, cuter_cerl:visited_tags(), state()}
               ; ({calculate_callgraph, [mfa()]}, from(), state()) -> {reply, ok, state()}
               .
handle_call({load, M}, _From, State) ->
  {reply, try_load(M, State), State};
handle_call({get_spec, {M, F, A}=MFA}, _From, #st{normalizeTypes = NormalizeTypes}=State) ->
  case try_load(M, State) of
    {ok, MDb} ->
      case cuter_cerl:retrieve_spec(MDb, {F, A}) of
        error ->
          cuter_pp:error_retrieving_spec(MFA, not_found),
          {reply, error, State};
        {ok, CerlSpec} ->
          DepMods = load_all_deps_of_spec(CerlSpec, M, MDb, State),
          Fn = fun(Mod) ->
              {ok, ModDb} = try_load(Mod, State),
              StoredTypes = cuter_cerl:get_stored_types(ModDb),
              {Mod, StoredTypes}
            end,
          ManyStoredTypes = [Fn(Mod) || Mod <- DepMods],
          Parsed = cuter_types:parse_spec(MFA, CerlSpec, ManyStoredTypes, NormalizeTypes),
          cuter_pp:parsed_spec(Parsed),
          {reply, {ok, Parsed}, State}
      end;
    Msg ->
      cuter_pp:error_retrieving_spec(MFA, Msg),
      {reply, error, State}
  end;
handle_call(get_visited_tags, _From, State=#st{tags = Tags}) ->
  {reply, Tags, State};
handle_call(get_logs, _From, State=#st{db = Db, unsupportedMfas = Ms, tags = Tags}) ->
  Cnt = get_branch_counter(),
  CachedMods = dump_cached_modules(Db),
  Logs = mk_logs(modules_in_cache(Db), sets:to_list(Ms), Tags, CachedMods, Cnt),
  {reply, Logs, State};
handle_call(get_whitelist, _From, State=#st{whitelist = Whitelist}) ->
  {reply, Whitelist, State};
handle_call({get_feasible_tags, NodeTypes}, _From, State=#st{callgraph = Callgraph}) ->
  Tags = get_feasible_tags(Callgraph, NodeTypes, State),
  {reply, Tags, State};
handle_call({calculate_callgraph, Mfas}, _From, State=#st{whitelist = Whitelist}) ->
  case cuter_callgraph:get_callgraph(Mfas, Whitelist) of
    {error, _Reason} ->
      {reply, error, State};
    {ok, Callgraph} ->
      LoadFn = fun(M) ->
                  cuter_pp:loading_visited_module(M),
                  try_load(M, State)
                end,
      cuter_callgraph:foreachModule(LoadFn, Callgraph),
      {reply, ok, State#st{callgraph = Callgraph}}
  end.

%% gen_server callback : handle_cast/2
-spec handle_cast({stop, pid()}, state()) -> {stop, normal, state()} | {noreply, state()}
               ; ({unsupported_mfa, mfa()}, state()) -> {noreply, state()}
               ; ({visit_tag, cuter_cerl:tag()}, state()) -> {noreply, state()}.
handle_cast({unsupported_mfa, MFA}, State=#st{unsupportedMfas = Ms}) ->
  Ms1 = sets:add_element(MFA, Ms),
  {noreply, State#st{unsupportedMfas = Ms1}};
handle_cast({stop, FromWho}, State=#st{super = Super}) ->
  case FromWho =:= Super of
    true  -> {stop, normal, State};
    false -> {noreply, State}
  end;
handle_cast({visit_tag, Tag}, State=#st{tags = Tags}) ->
  Ts = gb_sets:add_element(cuter_cerl:id_of_tag(Tag), Tags),
  {noreply, State#st{tags = Ts}}.

%% ----------------------------------------------------------------------------
%% Traverse a spec and load all the modules that it depends on (via the usage
%% of remote types). Then return a list of these modules.
%% ----------------------------------------------------------------------------

-spec load_all_deps_of_spec(cuter_types:stored_spec_value(), cuter:mod(), module_cache(), state()) -> [cuter:mod()].
load_all_deps_of_spec(CerlSpec, Module, MDb, State) ->
  LocalTypesCache = cuter_cerl:get_stored_types(MDb),
  % Get the remote dependencies of the spec.
  ToBeVisited = cuter_types:find_remote_deps_of_spec(CerlSpec, LocalTypesCache),
  InitDepMods = ordsets:add_element(Module, ordsets:new()),
  % Find iteratively the dependencies of the found dependencies.
  case load_all_deps(ToBeVisited, State, ordsets:new(), InitDepMods) of
    error -> [];
    {ok, DepMods} -> DepMods
  end.

-spec load_all_deps([remote_type()], state(), visited_remotes(), module_deps()) -> error | {ok, [cuter:mod()]}.
load_all_deps([], _State, _VisitedRemotes, DepMods) ->
  {ok, ordsets:to_list(DepMods)};
load_all_deps([Remote={M, TypeName, Arity}|Rest], State, VisitedRemotes, DepMods) ->
  case ordsets:is_element(Remote, VisitedRemotes) of
    true ->
      load_all_deps(Rest, State, VisitedRemotes, DepMods);
    false ->
      case try_load(M, State) of
        {ok, MDb} ->
          DepMods1 = ordsets:add_element(M, DepMods),
          VisitedRemotes1 = ordsets:add_element(Remote, VisitedRemotes),
          LocalTypesCache = cuter_cerl:get_stored_types(MDb),
          case dict:find({type, TypeName, Arity}, LocalTypesCache) of
            error ->
              %% TODO Report that we cannot access the definition of the type.
              error;
            {ok, {Type, _Params}} ->
              %% Get the remote dependencies of the type.
              Deps = cuter_types:find_remote_deps_of_type(Type, LocalTypesCache),
              %% Queue the ones that we haven't encountered yet.
              NewRemotes = [R || R <- Deps,
                not ordsets:is_element(R, VisitedRemotes1)],
              load_all_deps(NewRemotes ++ Rest, State, VisitedRemotes1, DepMods1)
          end;
        _Msg ->
          %% TODO Report that we cannot load the module.
          error
      end
  end.

%% ----------------------------------------------------------------------------
%% Get the feasible tags of all the mfas in the callgraph.
%% ----------------------------------------------------------------------------

get_feasible_tags(Callgraph, NodeTypes, State) ->
  VisitedMfas = cuter_callgraph:get_visited_mfas(Callgraph),
  FoldFn = fun(Mfa, Acc) -> gb_sets:union(get_feasible_tags_of_mfa(Mfa, NodeTypes, State), Acc) end,
  gb_sets:fold(FoldFn, gb_sets:new(), VisitedMfas).

get_feasible_tags_of_mfa({M,F,A}=Mfa, NodeTypes, State=#st{whitelist = Whitelist}) ->
  case erlang:is_builtin(M, F, A) orelse is_overriding(Mfa) orelse cuter_mock:is_whitelisted(Mfa, Whitelist) of
    true  -> gb_sets:new();
    false ->
      case is_mod_stored(M, State) of
        {false, _} ->
%          io:format("MODULE NOT FOUND MODULE ~p!~n", [M]),
          gb_sets:new();
        {true, Cache} ->
%          io:format("Looking Up ~p~n", [Mfa]),
          {ok, {Def, _}} = lookup_in_module_cache(Mfa, Cache),
          cuter_cerl:collect_feasible_tags(Def, NodeTypes)
      end
  end.

is_overriding({M,_,_}) ->
  lists:member(M, cuter_mock:overriding_modules()).

%% ----------------------------------------------------------------------------
%% Load a module to the cache
%% ----------------------------------------------------------------------------

-spec try_load(module(), state()) -> load_reply().
try_load(M, State) ->
  case is_mod_stored(M, State) of
    {true, Cache}   -> {ok, Cache};
    {false, eexist} -> load_mod(M, State); %% Load module M
    {false, Msg}    -> {error, Msg}
  end.

%% Load a module's code
-spec load_mod(module(), state()) -> {ok, module_cache()} | cuter_cerl:compile_error().
load_mod(M, #st{db = Db, withPmatch = WithPmatch}) ->
  Cache = ets:new(M, [ordered_set, protected]),  %% Create an ETS table to store the code of the module
  ets:insert(Db, {M, Cache}),                    %% Store the tid of the ETS table
  Reply = cuter_cerl:load(M, Cache, fun generate_tag/0, WithPmatch),  %% Load the code of the module
  case Reply of
    {ok, M} -> {ok, Cache};
    _ -> Reply
  end.

%% Check if a Module is stored in the Db
-spec is_mod_stored(module(), state()) -> {true, module_cache()} | {false, eexist | loaded_ret_atoms()}.
is_mod_stored(M, #st{db = Db}) ->
  case ets:lookup(Db, M) of
    [{M, Cache}] -> {true, Cache};
    [] ->
      case code:which(M) of
        non_existing   -> {false, non_existing};
        preloaded      -> {false, preloaded};
        cover_compiled -> {false, cover_compiled};
        _Path          -> {false, eexist}
      end
  end.

%% ----------------------------------------------------------------------------
%% Manage the logs of the CodeServer
%% ----------------------------------------------------------------------------

-spec mk_logs(modules(), mfas(), tags(), cached_modules(), counter()) -> logs().
mk_logs(LoadedMods, UnsupportedMFAs, VisitedTags, CachedMods, TagsAddedNo) ->
  #logs{ cachedMods = CachedMods
       , loadedMods = LoadedMods
       , tagsAddedNo = TagsAddedNo
       , visitedTags = VisitedTags
       , unsupportedMfas = UnsupportedMFAs}.

-spec cachedMods_of_logs(logs()) -> cached_modules().
cachedMods_of_logs(Logs) ->
  Logs#logs.cachedMods.

-spec loadedMods_of_logs(logs()) -> modules().
loadedMods_of_logs(Logs) ->
  Logs#logs.loadedMods.

-spec tagsAddedNo_of_logs(logs()) -> counter().
tagsAddedNo_of_logs(Logs) ->
  Logs#logs.tagsAddedNo.

-spec visitedTags_of_logs(logs()) -> tags().
visitedTags_of_logs(Logs) ->
  Logs#logs.visitedTags.

-spec unsupportedMfas_of_logs(logs()) -> mfas().
unsupportedMfas_of_logs(Logs) ->
  Logs#logs.unsupportedMfas.

%% ----------------------------------------------------------------------------
%% Manage module cache
%% ----------------------------------------------------------------------------

%% Looks up data in a module's cache.
-spec lookup_in_module_cache(any(), module_cache()) -> {ok, any()} | error.
lookup_in_module_cache(Key, Cache) ->
  case ets:lookup(Cache, Key) of
    [] -> error;
    [{Key, Value}] -> {ok, Value}
  end.

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

%% Returns the names of all the modules in the cache.
-spec modules_in_cache(cache()) -> [module()].
modules_in_cache(Db) ->
  ets:foldl(fun({M, _MDb}, Ms) -> [M|Ms] end, [], Db).

%% Dumps the cached modules' info into a dict.
-spec dump_cached_modules(cache()) -> cached_modules().
dump_cached_modules(Db) ->
  Fun = fun({M, MDb}, Stored) -> dict:store(M, ets:tab2list(MDb), Stored) end,
  ets:foldl(Fun, dict:new(), Db).

%% Creates an empty modules' cache.
-spec no_cached_modules() -> cached_modules().
no_cached_modules() -> dict:new().

%% Merges two caches.
-spec merge_dumped_cached_modules(cached_modules(), cached_modules()) -> cached_modules().
merge_dumped_cached_modules(Cached1, Cached2) ->
  dict:merge(fun(_Mod, Data1, _Data2) -> Data1 end, Cached1, Cached2).

%% Gets the names of the modules in a cache dump.
-spec modules_of_dumped_cache(cached_modules()) -> modules().
modules_of_dumped_cache(Cached) ->
  dict:fetch_keys(Cached).

%% Populates the DB with the stored modules provided.
-spec add_cached_modules(cache(), cached_modules()) -> ok.
add_cached_modules(Db, CachedMods) ->
  Fun = fun(M, Info, CDb) ->
	    MDb = ets:new(M, [ordered_set, protected]),
	    ets:insert(MDb, Info),
	    ets:insert(CDb, {M, MDb}),
	    CDb
	end,
  dict:fold(Fun, Db, CachedMods),
  ok.

%% ----------------------------------------------------------------------------
%% Counter for branch enumeration
%% ----------------------------------------------------------------------------

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
