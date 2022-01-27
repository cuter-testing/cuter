%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_codeserver).

-behaviour(gen_server).

%% external exports
-export([start/0, stop/1, load/2, unsupported_mfa/2, retrieve_spec/2,
         get_feasible_tags/2, get_logs/1, get_whitelist/1, get_visited_tags/1,
         visit_tag/2, calculate_callgraph/2,
         %% Work with module cache
         merge_dumped_cached_modules/2, modules_of_dumped_cache/1,
	 %% Code annotations
	 annotate_for_possible_errors/1,
         %% Access logs
         cachedMods_of_logs/1, visitedTags_of_logs/1, tagsAddedNo_of_logs/1,
         unsupportedMfas_of_logs/1, loadedMods_of_logs/1]).
%% gen_server callbacks
-export([init/1, terminate/2, code_change/3,
         handle_info/2, handle_call/3, handle_cast/2]).
%% Counter of branches & Tag generator.
-export([get_branch_counter/0, init_branch_counter/0, generate_tag/0]).

-include("include/cuter_macros.hrl").

-export_type([cached_modules/0, codeserver/0, counter/0, logs/0]).

%% Macros
-define(BRANCH_COUNTER_PREFIX, '__branch_count').

-type counter() :: non_neg_integer().
-type cached_module_data() :: any().
-type cached_modules() :: dict:dict(module(), cached_module_data()).

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
-type load_reply() :: {ok, cuter_cerl:kmodule()} | cuter_cerl:compile_error() | {error, (preloaded | cover_compiled | non_existing)}.
-type spec_reply() :: {ok, cuter_types:erl_spec()} | error.
-type from() :: {pid(), reference()}.

%% Finding the remote dependencies of a spec.
-type remote_type()     :: {cuter:mod(), atom(), byte()}.
-type module_deps()     :: ordsets:ordset(cuter:mod()).
-type visited_remotes() :: ordsets:ordset(remote_type()).

-type codeserver() :: pid().
-type codeserver_args() :: #{}.

%% Server's state
-record(st, {
  %% Acts as a reference table for looking up the ETS table that holds a module's extracted code.
  %% It stores tuples {Module :: module(), ModuleDb :: ets:tid()}.
  db :: cache(),
  %% The visited tags.
  tags = gb_sets:new() :: tags(),
  %% The set of mfa() that are not supported for symbolic execution but were
  %% encountered during the concolic executions.
  unsupportedMfas = sets:new() :: sets:set(mfa()),
  %% The whitelisted MFAs that should be treated as BIFs.
  whitelist :: cuter_mock:whitelist(),
  %% The computed callgraph from the entry points.
  callgraph :: cuter_callgraph:callgraph() | 'undefined'
}).
-type state() :: #st{}.

%% ----------------------------------------------------------------------------
%% Public API
%% ----------------------------------------------------------------------------

%% Starts a code server in the local node.
-spec start() -> codeserver().
start() ->
  case gen_server:start_link(?MODULE, #{}, []) of
    {ok, CodeServer} -> CodeServer;
    {error, Reason}  -> exit({codeserver_start, Reason})
  end.

%% Stops a code server.
-spec stop(codeserver()) -> ok.
stop(CodeServer) ->
  gen_server:cast(CodeServer, stop).

%% Requests a module's cache.
-spec load(codeserver(), module()) -> load_reply().
load(CodeServer, M) ->
  gen_server:call(CodeServer, {load, M}).

%% Log an MFA that cannot be symbolically evaluated.
-spec unsupported_mfa(codeserver(), mfa()) -> ok.
-ifdef(LOG_UNSUPPORTED_MFAS).
unsupported_mfa(CodeServer, MFA) ->
  gen_server:cast(CodeServer, {unsupported_mfa, MFA}).
-else.
unsupported_mfa(_, _) -> ok.
-endif.

%% Retrieves the spec of a given MFA.
-spec retrieve_spec(codeserver(), mfa()) -> spec_reply().
retrieve_spec(CodeServer, MFA) ->
  gen_server:call(CodeServer, {get_spec, MFA}).

%% Reports visiting a tag.
-spec visit_tag(codeserver(), cuter_cerl:tag()) -> ok.
visit_tag(CodeServer, Tag) ->
  gen_server:cast(CodeServer, {visit_tag, Tag}).

%% Gets the visited tags.
-spec get_visited_tags(codeserver()) -> tags().
get_visited_tags(CodeServer) ->
  gen_server:call(CodeServer, get_visited_tags).

%% Gets the logs of the CodeServer.
-spec get_logs(codeserver()) -> logs().
get_logs(CodeServer) ->
  gen_server:call(CodeServer, get_logs).

%% Gets the whitelisted MFAs.
-spec get_whitelist(codeserver()) -> cuter_mock:whitelist().
get_whitelist(CodeServer) ->
  gen_server:call(CodeServer, get_whitelist).

%% Calculates the callgraph from some MFAs.
-spec calculate_callgraph(codeserver(), [mfa()]) -> ok | error.
calculate_callgraph(CodeServer, Mfas) ->
  gen_server:call(CodeServer, {calculate_callgraph, Mfas}).

%% Gets the feasible tags.
-spec get_feasible_tags(codeserver(), cuter_cerl:node_types()) -> cuter_cerl:visited_tags().
get_feasible_tags(CodeServer, NodeTypes) ->
  gen_server:call(CodeServer, {get_feasible_tags, NodeTypes}).

%% Annotates the code for possible errors.
-spec annotate_for_possible_errors(codeserver()) -> ok.
annotate_for_possible_errors(CodeServer) ->
  gen_server:call(CodeServer, annotate_for_possible_errors).

%% ----------------------------------------------------------------------------
%% gen_server callbacks (Server Implementation)
%% ----------------------------------------------------------------------------

%% gen_server callback : init/1
-spec init(codeserver_args()) -> {ok, state()}.
init(_Args) ->
  {ok, Whitelist} = cuter_config:fetch(?WHITELISTED_MFAS),
  Db = ets:new(?MODULE, [ordered_set, protected]),
  init_branch_counter(),
  {ok, #st{ db = Db, whitelist = Whitelist }}.

%% gen_server callback : terminate/2
-spec terminate(any(), state()) -> ok.
terminate(_Reason, #st{db = Db}) ->
  Fn = fun({_M, Kmodule}, ok) ->
      cuter_cerl:destroy_kmodule(Kmodule)
    end,
  ok = ets:foldl(Fn, ok, Db),
  ets:delete(Db),
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
	       ; (annotate_for_possible_errors, from(), state()) -> {reply, ok, state()}
               .
handle_call({load, M}, _From, State) ->
  {reply, try_load(M, State), State};
handle_call({get_spec, {M, _F, _A}=Mfa}, _From, State) ->
  case try_load(M, State) of
    {ok, Kmodule} ->
      case cuter_cerl:kmodule_mfa_spec(Kmodule, Mfa) of
        error ->
          cuter_pp:error_retrieving_spec(Mfa, not_found),
          {reply, error, State};
        {ok, CerlSpec} ->
          DepMods = load_all_deps_of_spec(CerlSpec, M, Kmodule, State),
          Fn = fun(Mod) ->
              {ok, KM} = try_load(Mod, State),
              StoredTypes = cuter_cerl:kmodule_types(KM),
              {Mod, StoredTypes}
            end,
          ManyStoredTypes = [Fn(Mod) || Mod <- DepMods],
          Parsed = cuter_types:parse_spec(Mfa, CerlSpec, ManyStoredTypes),
          cuter_pp:parsed_spec(Parsed),
          {reply, {ok, Parsed}, State}
      end;
    Msg ->
      cuter_pp:error_retrieving_spec(Mfa, Msg),
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
  end;
handle_call(annotate_for_possible_errors, _From, State=#st{db = Db}) ->
  Fn = fun({_M, Kmodule}, KfunAcc) ->
      KfunMappings = cuter_cerl:kmodule_mfas_with_kfuns(Kmodule),
      TrivialMergeFn = fun(_K, V1, _V2) -> V1 end,
      dict:merge(TrivialMergeFn, KfunAcc, KfunMappings)
    end,
  Fn2 = fun({_M, Kmodule}, Acc) ->
	    [Kmodule|Acc]
	end,
  Kmodules = ets:foldl(Fn2, [], Db),
  {ok, EntryPoint} = cuter_config:fetch(?ENTRY_POINT),
  MfasToKfuns = ets:foldl(Fn, dict:new(), Db),
  %io:format("Before Specs~n"),
  %io:format("ast: ~p~n", [cuter_cerl:kfun_code(dict:fetch(EntryPoint, MfasToKfuns))]),
  MfasToSpecs = cuter_types:parse_specs(Kmodules),
  %io:format("Before Preprocess~n"),
  UpdatedKfuns = cuter_maybe_error_annotation:preprocess(EntryPoint, MfasToKfuns, MfasToSpecs, true),
  RFn = fun({M, F, A}, Kfun, _Acc) ->
	   [{_M, Kmodule}] = ets:lookup(Db, M),
	    cuter_cerl:kmodule_update_kfun(Kmodule, {M, F, A}, Kfun)
	end,
  dict:fold(RFn, ok, UpdatedKfuns),
  %io:format("spec: ~p~n", [dict:find(EntryPoint, MfasToSpecs)]),
  %io:format("ast: ~p~n", [cuter_cerl:kfun_code(dict:fetch(EntryPoint, UpdatedKfuns))]),
  {reply, ok, State}.


%% gen_server callback : handle_cast/2
-spec handle_cast(stop, state()) -> {stop, normal, state()} | {noreply, state()}
               ; ({unsupported_mfa, mfa()}, state()) -> {noreply, state()}
               ; ({visit_tag, cuter_cerl:tag()}, state()) -> {noreply, state()}.
handle_cast({unsupported_mfa, MFA}, State=#st{unsupportedMfas = Ms}) ->
  Ms1 = sets:add_element(MFA, Ms),
  {noreply, State#st{unsupportedMfas = Ms1}};
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast({visit_tag, Tag}, State=#st{tags = Tags}) ->
  Ts = gb_sets:add_element(cuter_cerl:id_of_tag(Tag), Tags),
  {noreply, State#st{tags = Ts}}.

%% ----------------------------------------------------------------------------
%% Traverse a spec and load all the modules that it depends on (via the usage
%% of remote types). Then return a list of these modules.
%% ----------------------------------------------------------------------------

-spec load_all_deps_of_spec(cuter_types:stored_spec_value(), module(), cuter_cerl:kmodule(), state()) -> [cuter:mod()].
load_all_deps_of_spec(CerlSpec, Module, Kmodule, State) ->
  LocalTypesCache = cuter_cerl:kmodule_types(Kmodule),
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
        {ok, Kmodule} ->
          DepMods1 = ordsets:add_element(M, DepMods),
          VisitedRemotes1 = ordsets:add_element(Remote, VisitedRemotes),
          LocalTypesCache = cuter_cerl:kmodule_types(Kmodule),
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
          gb_sets:new();
        {true, Kmodule} ->
          {ok, Kfun} = cuter_cerl:kmodule_kfun(Kmodule, Mfa),
          Code = cuter_cerl:kfun_code(Kfun),
          cuter_cerl:collect_feasible_tags(Code, NodeTypes)
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
-spec load_mod(module(), state()) -> {ok, cuter_cerl:kmodule()} | cuter_cerl:compile_error().
load_mod(M, #st{db = Db}) ->
  WithPmatch =
    case cuter_config:fetch(?DISABLE_PMATCH) of
      {ok, true} ->
        false;
      _ ->
        true
    end,
  Reply = cuter_cerl:load(M, fun generate_tag/0, WithPmatch),  %% Load the code of the module
  case Reply of
    {ok, Kmodule} ->
      ets:insert(Db, {M, Kmodule}),
      {ok, Kmodule};
    _ ->
      Reply
  end.

%% Check if a Module is stored in the Db
-spec is_mod_stored(module(), state()) -> {true, cuter_cerl:kmodule()} | {false, eexist | loaded_ret_atoms()}.
is_mod_stored(M, #st{db = Db}) ->
  case ets:lookup(Db, M) of
    [{M, Kmodule}] ->
      {true, Kmodule};
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

%% Returns the names of all the modules in the cache.
-spec modules_in_cache(cache()) -> [module()].
modules_in_cache(Db) ->
  ets:foldl(fun({M, _Kmodule}, Ms) -> [M|Ms] end, [], Db).

%% Dumps the cached modules' info into a dict.
-spec dump_cached_modules(cache()) -> cached_modules().
dump_cached_modules(Db) ->
  Fun = fun({M, MDb}, Stored) -> dict:store(M, ets:tab2list(MDb), Stored) end,
  ets:foldl(Fun, dict:new(), Db).

%% Merges two caches.
-spec merge_dumped_cached_modules(cached_modules(), cached_modules()) -> cached_modules().
merge_dumped_cached_modules(Cached1, Cached2) ->
  dict:merge(fun(_Mod, Data1, _Data2) -> Data1 end, Cached1, Cached2).

%% Gets the names of the modules in a cache dump.
-spec modules_of_dumped_cache(cached_modules()) -> modules().
modules_of_dumped_cache(Cached) ->
  dict:fetch_keys(Cached).

%% ----------------------------------------------------------------------------
%% Counter for branch enumeration
%% ----------------------------------------------------------------------------

%% Initializes the branch counter to 0.
-spec init_branch_counter() -> ok.
init_branch_counter() ->
  _ = put(?BRANCH_COUNTER_PREFIX, 0),
  ok.

-spec get_branch_counter() -> counter().
get_branch_counter() ->
  get(?BRANCH_COUNTER_PREFIX).

-spec generate_tag() -> cuter_cerl:tag().
generate_tag() ->
  N = get(?BRANCH_COUNTER_PREFIX) + 1,
  put(?BRANCH_COUNTER_PREFIX, N),
  cuter_cerl:tag_from_id(N).
