%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_codeserver).
-behaviour(gen_server).

%% external exports
-export([start/3, stop/1, load/2, unsupported_mfa/2, retrieve_spec/2, visit_tag/2]).
%% gen_server callbacks
-export([init/1, terminate/2, code_change/3, handle_info/2, handle_call/3, handle_cast/2]).
%% counter of branches
-export([set_branch_counter/1, generate_tag/0]).

-include("include/cuter_macros.hrl").

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

-record(state, {
  super                          :: pid(),
  db                             :: ets:tab(),
  waiting = orddict:new()        :: [{{atom(), tuple()}, pid()}],
  workers = []                   :: [pid()],
  unsupported_mfas = sets:new()  :: sets:set(mfa()),
  tags = gb_sets:new()           :: gb_sets:set(cuter_cerl:tagID())
}).
-type state() :: #state{}.

%% ============================================================================
%% External exports (Public API)
%% ============================================================================

%% Start a CodeServer
-spec start(pid(), cuter_analyzer:stored_modules(), integer()) -> pid().
start(Super, StoredMods, TagsN) ->
  case gen_server:start(?MODULE, [Super, StoredMods, TagsN], []) of
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
-spec init([pid() | cuter_analyzer:stored_modules() | integer(), ...]) -> {ok, state()}.
init([Super, StoredMods, TagsN]) ->
  link(Super),
  Db = ets:new(?MODULE, [ordered_set, protected]),
  populate_db_with_mods(Db, StoredMods),
  set_branch_counter(TagsN), %% Initialize the counter for the branch enumeration.
  {ok, #state{db = Db, super = Super}}.

%% gen_server callback : terminate/2
-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
  Db = State#state.db,
  Super = State#state.super,
  Ms = State#state.unsupported_mfas,
  Tags = State#state.tags,
  Cnt = get_branch_counter(),
  StoredMods = stored_mods(Db),
  Logs = [ {loaded_mods, delete_stored_modules(Db)}  % Delete all created ETS tables
         , {unsupported_mfas, sets:to_list(Ms)}
         , {visited_tags, Tags}
         , {stored_mods, StoredMods}
         , {tags_added_no, Cnt}
         ],
  ets:delete(Db),
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
      try
        CerlSpec = cuter_cerl:retrieve_spec(MDb, {F, A}),
        ErlSpec = cuter_types:parse_spec(CerlSpec),
        {reply, {ok, ErlSpec}, State}
      catch
        error:E ->
          cuter_pp:error_retrieving_spec(MFA, E),
          {reply, error, State};
        throw:E ->
          cuter_pp:error_retrieving_spec(MFA, E),
          {reply, error, State}
      end;
    Msg ->
      cuter_pp:error_retrieving_spec(MFA, Msg),
      {reply, error, State}
  end.

%% gen_server callback : handle_cast/2
-spec handle_cast({stop, pid()}, state()) -> {stop, normal, state()} | {noreply, state()}
               ; ({unsupported_mfa, mfa()}, state()) -> {noreply, state()}
               ; ({visit_tag, cuter_cerl:tag()}, state()) -> {noreply, state()}.
handle_cast({unsupported_mfa, MFA}, State=#state{unsupported_mfas = Ms}) ->
  Ms1 = sets:add_element(MFA, Ms),
  {noreply, State#state{unsupported_mfas = Ms1}};
handle_cast({stop, FromWho}, State) ->
  case FromWho =:= State#state.super of
    true  -> {stop, normal, State};
    false -> {noreply, State}
  end;
handle_cast({visit_tag, Tag}, State=#state{tags = Tags}) ->
%  io:format("[code] ~p~n", [Tag]),
  Ts = gb_sets:add_element(cuter_cerl:id_of_tag(Tag), Tags),
  {noreply, State#state{tags = Ts}}.

%% ============================================================================
%% Internal functions (Helper methods)
%% ============================================================================

%% Populates the DB with the stored modules provided.
-spec populate_db_with_mods(ets:tid(), cuter_analyzer:stored_modules()) -> ok.
populate_db_with_mods(Db, StoredMods) ->
  orddict:fold(
    fun(M, Info, CDb) ->
      MDb = ets:new(M, [ordered_set, protected]),
      ets:insert(MDb, Info),
      ets:insert(CDb, {M, MDb}),
      CDb
    end,
    Db,
    StoredMods
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
-spec load_mod(module(), state()) -> {ok, ets:tid()} | cuter_cerl:compile_error().
load_mod(M, #state{db = Db}) ->
  MDb = ets:new(M, [ordered_set, protected]),  %% Create an ETS table to store the code of the module
  ets:insert(Db, {M, MDb}),                    %% Store the tid of the ETS table
  Reply = cuter_cerl:load(M, MDb, fun generate_tag/0),        %% Load the code of the module
  case Reply of
    {ok, M} -> {ok, MDb};
    _ -> Reply
  end.

%% Check if a Module is stored in the Db
-spec is_mod_stored(atom(), state()) -> {true, ets:tab()} | {false, eexist | loaded_ret_atoms()}.
is_mod_stored(M, State) ->
  Db = State#state.db,
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

%% Delete all ETS tables that contain the code of modules
-spec delete_stored_modules(ets:tid()) -> [module()].
delete_stored_modules(Db) ->
  ets:foldl(fun({M, MDb}, Ms) -> ets:delete(MDb), [M|Ms] end, [], Db).

-spec stored_mods(ets:tid()) -> orddict:orddict().
stored_mods(Db) ->
  ets:foldl(fun({M, MDb}, Stored) -> orddict:store(M, ets:tab2list(MDb), Stored) end, orddict:new(), Db).

%% ============================================================================
%% Counter for branch enumeration
%% ============================================================================

-spec get_branch_counter() -> integer().
get_branch_counter() ->
  get(?BRANCH_COUNTER_PREFIX).

-spec set_branch_counter(integer()) -> any().
set_branch_counter(N) ->
  put(?BRANCH_COUNTER_PREFIX, N).

-spec generate_tag() -> cuter_cerl:tag().
generate_tag() ->
  N = get(?BRANCH_COUNTER_PREFIX) + 1,
  put(?BRANCH_COUNTER_PREFIX, N),
  cuter_cerl:tag_from_id(N).
