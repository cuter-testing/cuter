-module(conc_eval).
-compile([export_all]).

-include_lib("compiler/src/core_parse.hrl").
-include("conc_lib.hrl").

%%--------------------------------------------------------------------------
%% i(M, F, A, CodeServer, TraceServer) -> Val
%%   M :: atom()
%%   F :: atom()
%%   A :: [term()]
%%   CodeServer :: pid()
%%   TraceServer :: pid()
%%   Val :: term()
%% Wrapper exported function that spawns an interpreter process
%% and returns the value of MFA
%%--------------------------------------------------------------------------
i(M, F, A, CodeServer, TraceServer) ->
  Root = self(),
  R = erlang:make_ref(),
  Args = [{named, {M, F}}, A, concrete, external, CodeServer, TraceServer],
  I = fun() ->
    register_to_trace(TraceServer, Root),
    Val = apply(conc_eval, eval, Args),
    Root ! {R, Val}
  end,
  erlang:spawn(I),
  receive
    {R, Val} -> Val
  end.

%%-----------------------------------------------------------------------------------
%% eval(FunInfo, As, Mode, CallType, CodeServer, TraceServer, Register) -> Val
%%   FunInfo :: {named, {M, F}} | {lambda, Closure} | {letrec_func, {M, F, Def, Env}
%%    M :: atom()
%%    F :: atom()
%%    Closure :: fun()
%%    Def :: #c_def{}
%%    Env :: conc_lib:environment()
%%   As :: [term()]
%%   Mode :: concrete | symbolic
%%   CallType :: local | external
%%   CodeServer :: pid()
%%   TraceServer :: pid()
%%   CallerPid :: pid()
%%    Parent :: pid()
%%   Val :: term()
%% Interprets and traces functions as Core Erlang ASTs(concretely or symbolically)
%%-----------------------------------------------------------------------------------

%% Concrete Evaluation of MFA

%% Handle spawn/1, spawn/2, spawn/3, spawn/4,
%% spawn_link/1 spawn_link/2, spawn_link/3, spawn_link/4
eval({named, {erlang, F}}, As, concrete, _CallType, CodeServer, TraceServer)
  when F =:= spawn; F =:= spawn_link ->
    ChildPid = 
      case As of
        [Fun] ->
          EvalArgs = [{lambda, Fun}, [], concrete, local, CodeServer, TraceServer],
          Child = register_and_apply(TraceServer, self(), EvalArgs),
          erlang:F(Child);
        [Node, Fun] ->
          EvalArgs = [{lambda, Fun}, [], concrete, local, CodeServer, TraceServer],
          Child = register_and_apply(TraceServer, self(), EvalArgs),
          erlang:F(Node, Child);
        [Mod, Fun, Args] ->
          Call = find_call_type(erlang, Mod),
          EvalArgs = [{named, {Mod, Fun}}, Args, concrete, Call, CodeServer, TraceServer],
          Child = register_and_apply(TraceServer, self(), EvalArgs),
          erlang:F(Child);
        [Node, Mod, Fun, Args] ->
          Call = find_call_type(erlang, Mod),
          EvalArgs = [{named, {Mod, Fun}}, Args, concrete, Call, CodeServer, TraceServer],
          Child = register_and_apply(TraceServer, self(), EvalArgs),
          erlang:F(Node, Child);
        _ ->
          exception(error, {undef, {erlang, spawn, length(As)}})
      end,
    receive
      {ChildPid, registered} -> ChildPid
    end;
    
%% Handle spawn_monitor/1, spawn_monitor/3
eval({named, {erlang, spawn_monitor}}, As, concrete, _CallType, CodeServer, TraceServer) ->
  EvalArgs =
    case As of
      [Fun] ->
        [{lambda, Fun}, [], concrete, local, CodeServer, TraceServer];
      [Mod, Fun, Args] ->
        Call = find_call_type(erlang, Mod),
        [{named, {Mod, Fun}}, Args, concrete, Call, CodeServer, TraceServer];
      _ ->
        exception(error, {undef, {erlang, spawn_monitor, length(As)}})
    end,
  Child = register_and_apply(TraceServer, self(), EvalArgs),
  {ChildPid, ChildRef} = erlang:spawn_monitor(Child),
  receive
    {ChildPid, registered} -> {ChildPid, ChildRef}
  end;
  
%% Handle spawn_opt/2, spawn_opt/3, spawn_opt/4, spawn_opt/5
eval({named, {erlang, spawn_opt}}, As, concrete, _CallType, CodeServer, TraceServer) ->
  {ChildPid, ChildRef} = 
    case As of
      [Fun, Opts] ->
        EvalArgs = [{lambda, Fun}, [], concrete, local, CodeServer, TraceServer],
        Child = register_and_apply(TraceServer, self(), EvalArgs),
        erlang:spawn_opt(Child, Opts);
      [Node, Fun, Opts] ->
        EvalArgs = [{lambda, Fun}, [], concrete, local, CodeServer, TraceServer],
        Child = register_and_apply(TraceServer, self(), EvalArgs),
        erlang:spawn_opt(Node, Child, Opts);
      [Mod, Fun, Args, Opts] ->
        Call = find_call_type(erlang, Mod),
        EvalArgs = [{named, {Mod, Fun}}, Args, concrete, Call, CodeServer, TraceServer],
        Child = register_and_apply(TraceServer, self(), EvalArgs),
        erlang:spawn_opt(Child, Opts);
      [Node, Mod, Fun, Args, Opts] ->
        Call = find_call_type(erlang, Mod),
        EvalArgs = [{named, {Mod, Fun}}, Args, concrete, Call, CodeServer, TraceServer],
        Child = register_and_apply(TraceServer, self(), EvalArgs),
        erlang:spawn_opt(Node, Child, Opts);
      _ ->
        exception(error, {undef, {erlang, spawn_opt, length(As)}})
    end,
  receive
    {ChildPid, registered} -> {ChildPid, ChildRef}
  end;
  
  
%% Handle apply/2, apply/3
eval({named, {erlang, apply}}, As, concrete, _CallType, CodeServer, TraceServer) ->
  EvalArgs =
    case As of
      [Fun, Args] ->
        [{lambda, Fun}, Args, concrete, local, CodeServer, TraceServer];
      [Mod, Fun, Args] ->
        Call = find_call_type(erlang, Mod),
        [{named, {Mod, Fun}}, Args, concrete, Call, CodeServer, TraceServer];
      _ ->
        exception(error, {undef, {erlang, apply, length(As)}})
    end,
  erlang:apply(conc_eval, eval, EvalArgs);

%% Handle make_fun/3  
eval({named, {erlang, make_fun}}, As, concrete, _CallType, CodeServer, TraceServer) ->
  case As of
    [M, F, Arity] ->
      make_fun(M, F, Arity, concrete, CodeServer, TraceServer);
    _ ->
      exception(error, {undef, {erlang, make_fun, length(As)}})
  end;

%% Handle an MFA
eval({named, {M, F}}, As, concrete, CallType, CodeServer, TraceServer) ->
  Arity = length(As),
  case conc_lib:is_bif(M, F, Arity) of
    true ->
      apply(M, F, As);
    false ->
      case get_module_db(M, CodeServer) of
        preloaded ->
          apply(M, F, As);
        {ok, MDb} ->
          Key = {M, F, Arity},
          {Def, Exported} = retrieve_function(Key, MDb),
          check_exported(Exported, CallType, Key),
          Env = bind_parameters(As, Def#c_fun.vars, conc_lib:new_environment()),
          eval_expr(M, concrete, CodeServer, TraceServer, Def#c_fun.body, Env)
      end
  end;
  
%% Handle a Closure
eval({lambda, Closure}, As, concrete, _CallType, _CodeServer, _TraceServer) ->
  %% No need to bind func params, will be done by closure
  erlang:apply(Closure, As);
  
%% Handle a function bound in a letrec expression
eval({letrec_func, {M, _F, Def, Env}}, As, concrete, _CallType, CodeServer, TraceServer) ->
  NewEnv = bind_parameters(As, Def#c_fun.vars, Env),
  eval_expr(M, concrete, CodeServer, TraceServer, Def#c_fun.body, NewEnv).


%%--------------------------------------------------------------------
%% exception(Class, Reason)
%%   Class :: error | exit | throw
%%   Reason :: term()
%% Raises the exception.
%%--------------------------------------------------------------------
exception(Class, Reason) ->
  erlang:Class(Reason).


%%--------------------------------------------------------------------
%% eval_expr(M, Mode, CodeServer, TraceServer, Expr, Env) -> Sem
%%   M :: atom()
%%   Mode :: concrete | symbolic
%%   CodeServer :: pid()
%%   TraceServer :: pid()
%%   Expr :: cerl()
%%   Env :: conc_lib:environment()
%%   Sem :: conc_lib:semantic_value()
%% Evaluates Core Erlang ASTs in record format
%%--------------------------------------------------------------------

%% c_apply
eval_expr(M, concrete, CodeServer, TraceServer, {c_apply, _Anno, Op, Args}, Env) ->
  OpVal = eval_expr(M, concrete, CodeServer, TraceServer, Op, Env),
  ArgsVal = lists:map(
    fun(Arg) -> %% Will create closures where appropriate
      case eval_expr(M, concrete, CodeServer, TraceServer, Arg, Env) of
        {func, {F, Arity}} ->
          create_closure(M, F, Arity, concrete, CodeServer, TraceServer);
        {letrec_func, {Mod, F, Arity, Def, E}} ->
          create_closure(Mod, F, Arity, concrete, CodeServer, TraceServer, Def, E());
        ArgVal ->
          ArgVal
      end
    end,
    Args
  ),
  case OpVal of %% Check eval_expr(..., #c_var{}, ...) output for reference
    {func, {Func, _Arity}} -> 
      eval({named, {M, Func}}, ArgsVal, concrete, local, CodeServer, TraceServer);
    {letrec_func, {Mod, Func, _Arity, Def, E}} ->
      eval({letrec_func, {Mod, Func, Def, E()}}, ArgsVal, concrete, local, CodeServer, TraceServer);
    Closure ->
      eval({lambda, Closure}, ArgsVal, concrete, local, CodeServer, TraceServer)
  end;
  
%%c_binary
eval_expr(_M, concrete, _CodeServer, _TraceServer, {c_binary, _Anno, _Segments}, _Env) ->
  io:format("c_binary not implemented yet!!~n"),
  exception(error, c_binary);
  
%%c_bitstring
eval_expr(_M, concrete, _CodeServer, _TraceServer, {c_bitstring, _Anno, _Val, _Size, _Unit, _Type, _Flags}, _Env) ->
  io:format("c_bitstring not implemented yet!!~n"),
  exception(error, c_bitstring);
  
%% c_call
eval_expr(M, concrete, CodeServer, TraceServer, {c_call, _Anno, Mod, Fun, Args}, Env) ->
  ModVal = eval_expr(M, concrete, CodeServer, TraceServer, Mod, Env),
  FunVal = eval_expr(M, concrete, CodeServer, TraceServer, Fun, Env),
  ArgsVal = lists:map(
    fun(Arg) ->
      case eval_expr(M, concrete, CodeServer, TraceServer, Arg, Env) of
        {func, {F, Arity}} ->
          create_closure(M, F, Arity, concrete, CodeServer, TraceServer);
        {letrec_func, {Mod, F, Arity, Def, E}} ->
          create_closure(Mod, F, Arity, concrete, CodeServer, TraceServer, Def, E());
        ArgVal ->
          ArgVal
      end
    end,
    Args
  ),
  eval({named, {ModVal, FunVal}}, ArgsVal, concrete, find_call_type(M, ModVal), CodeServer, TraceServer);
  
%% c_case
eval_expr(M, concrete, CodeServer, TraceServer, {c_case, _Anno, Arg, Clauses}, Env) ->
  ArgVal = eval_expr(M, concrete, CodeServer, TraceServer, Arg, Env),
  {Body, NewEnv, _Cnt} = find_clause(M, concrete, 'case', CodeServer, TraceServer, Clauses, ArgVal, Env),
  eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv);
  
%% c_catch
eval_expr(M, concrete, CodeServer, TraceServer, {c_catch, _Anno, Body}, Env) ->
  try
    eval_expr(M, concrete, CodeServer, TraceServer, Body, Env)
  catch
    throw:Throw ->
      Throw;
    exit:Exit ->
      {'EXIT', Exit};
    error:Error ->
      %% CAUTION! Will not include stacktrace info
      {'EXIT', {Error, []}}
  end;

%% c_cons
eval_expr(M, concrete, CodeServer, TraceServer, {c_cons, _Anno, Hd, Tl}, Env) ->
  HdVal = eval_expr(M, concrete, CodeServer, TraceServer, Hd, Env),
  TlVal = eval_expr(M, concrete, CodeServer, TraceServer, Tl, Env),
  [HdVal | TlVal];
  
%% c_fun
eval_expr(M, concrete, CodeServer, TraceServer, {c_fun, _Anno, Vars, Body}, Env) ->
  Arity = length(Vars),
  make_fun(M, lambda, Arity, concrete, CodeServer, TraceServer, Vars, Body, Env, false);
  
%% c_let
eval_expr(M, concrete, CodeServer, TraceServer, {c_let, _Anno, Vars, Arg, Body}, Env) ->
  Degree = length(Vars),
  case Degree of
    1 ->
      Args = [eval_expr(M, concrete, CodeServer, TraceServer, Arg, Env)];
    _ ->
      {valuelist, Args, Degree} = eval_expr(M, concrete, CodeServer, TraceServer, Arg, Env)
  end,
  NewEnv = bind_parameters(Args, Vars, Env),
  eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv);
  
%% c_letrec
eval_expr(M, concrete, CodeServer, TraceServer, {c_letrec, _Anno, Defs, Body}, Env) ->
  H = fun(F) -> fun() ->
    lists:foldl(
      fun({Func, Def}, E) ->
        conc_lib:add_binding(Func#c_var.name, {letrec_func, {M, Def, F}}, E)
      end,
      Env, Defs
    )
  end end,
  NewEnv = (y(H))(),
  eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv);
  
%% c_literal
eval_expr(_M, concrete, _CodeServer, _TraceServer, {c_literal, _Anno, Val}, _Env) ->
  Val;
  
%% c_primop
eval_expr(M, concrete, CodeServer, TraceServer, {c_primop, _Anno, Name, Args}, Env) ->
  %% Evaluate primop arguments
  ArgsVal = lists:map(
    fun(Arg) ->
      eval_expr(M, concrete, CodeServer, TraceServer, Arg, Env)
    end,
    Args
  ),
  %% Determine the primop
  PrimOp = Name#c_literal.val,
  %% Call the local function TODO needs case for each primop
  case PrimOp of
    'raise' ->
      [Class, Reason] = ArgsVal,
      eval({named, {erlang, Class}}, [Reason], concrete, local, CodeServer, TraceServer);
    'match_fail' -> %% TODO fix
      [Reason] = ArgsVal,
      eval({named, {erlang, error}}, [{badmatch, Reason}], concrete, local, CodeServer, TraceServer)
  end;
  
%%c_receive
eval_expr(M, concrete, CodeServer, TraceServer, {c_receive, _Anno, Clauses, Timeout, Action}, Env) ->
  TimeoutVal = eval_expr(M, concrete, CodeServer, TraceServer, Timeout, Env),
  true = check_timeout(TimeoutVal),
  %% Start Timer
  Start = erlang:now(),
  {messages, Mailbox} = erlang:process_info(self(), messages),
  Message = find_message(M, concrete, CodeServer, TraceServer, Clauses, Env, Mailbox),
  case Message of
    {Msg, Body, NewEnv, _Cnt} ->
      receive Msg -> ok end,
      eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv);
    false ->
      CurrMsgs = length(Mailbox),
      find_message_loop(M, concrete, CodeServer, TraceServer, Clauses, TimeoutVal, Action, Env, Start, CurrMsgs)
  end;
  
%% c_try
eval_expr(M, concrete, CodeServer, TraceServer, {c_try, _Anno, Arg, Vars, Body, Evars, Handler}, Env) ->
  try
    Degree = length(Vars),
    case Degree of
      1 ->
        ArgVal = [eval_expr(M, concrete, CodeServer, TraceServer, Arg, Env)];
      _ ->
        {valuelist, ArgVal, Degree} = eval_expr(M, concrete, CodeServer, TraceServer, Arg, Env)
    end,
    NewEnv = bind_parameters(ArgVal, Vars, Env),
    eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv)
  catch
    Class:Reason ->
      %% CAUTION!! 3rd Var, which should be stacktrace, is substituted by exception class
      Vals = [Class, Reason, Class],
      ExcEnv = bind_parameters(Vals, Evars, Env),
      eval_expr(M, concrete, CodeServer, TraceServer, Handler, ExcEnv)
  end;
  
%% c_seq
eval_expr(M, concrete, CodeServer, TraceServer, {c_seq, _Anno, Arg, Body}, Env) ->
  _ArgVal = eval_expr(M, concrete, CodeServer, TraceServer, Arg, Env),
  eval_expr(M, concrete, CodeServer, TraceServer, Body, Env);

%% c_tuple
eval_expr(M, concrete, CodeServer, TraceServer, {c_tuple, _Anno, Es}, Env) ->
  EsVal = lists:map(
    fun(E) -> eval_expr(M, concrete, CodeServer, TraceServer, E, Env) end,
    Es
  ),
  list_to_tuple(EsVal);
  
%% c_values
eval_expr(M, concrete, CodeServer, TraceServer, {c_values, _Anno, Es}, Env) ->
  Degree = length(Es),
  EsVal = lists:map(
    fun(E) -> eval_expr(M, concrete, CodeServer, TraceServer, E, Env) end,
    Es
  ),
  #valuelist{values=EsVal, degree=Degree};
  
%% c_var
eval_expr(_M, concrete, _CodeServer, _TraceServer, {c_var, _Anno, Name}, Env)
  when is_tuple(Name) ->
    %% If Name is a function
    case conc_lib:get_value(Name, Env) of
      Closure when is_function(Closure) -> %% Closure
        Closure;
      {ok, {letrec_func, {Mod, Def, E}}} ->  %% Fun bound in a letrec
        {Fun, Arity} = Name,
        {letrec_func, {Mod, Fun, Arity, Def, E}};
      error ->  %% either local in module or external
        {Fun, Arity} = Name,
        {func, {Fun, Arity}}
    end;
eval_expr(_M, concrete, _CodeServer, _TraceServer, {c_var, _Anno, Name}, Env) ->
  %% If it's a variable then return its value
  {ok, Value} = conc_lib:get_value(Name, Env),
  Value.
  

%%-------------------------------------------------------------------------------------------------
%% find_message_loop(M, Mode, CodeServer, TraceServer, Clauses, Timeout, Action, Env, Start, Msgs)
%%   M :: atom()
%%   Mode :: concrete | symbolic
%%   CodeServer :: pid()
%%   TraceServer :: pid()
%%   Clauses :: [#c_clause{}]
%%   Timeout :: infinity | non_neg_integer()
%%   Action :: cerl()
%%   Env :: conc_lib:environment()
%%   Start :: timestamp()
%%   Msgs :: non_neg_integer()
%% Emulates the waiting loop of the receive expression
%%-------------------------------------------------------------------------------------------------
find_message_loop(M, concrete, CodeServer, TraceServer, Clauses, infinity, Action, Env, Start, Msgs) ->
  run_message_loop(M, concrete, CodeServer, TraceServer, Clauses, infinity, Action, Env, Start, Msgs);

find_message_loop(M, concrete, CodeServer, TraceServer, Clauses, Timeout, Action, Env, Start, Msgs) ->
  Now = now(),
  Passed = timer:now_diff(Now, Start) / 1000,
  case Passed >= Timeout of
    true ->
      eval_expr(M, concrete, CodeServer, TraceServer, Action, Env);
    false ->
      run_message_loop(M, concrete, CodeServer, TraceServer, Clauses, Timeout, Action, Env, Start, Msgs)
  end.

%% Helper function run_message_loop/10
run_message_loop(M, concrete, CodeServer, TraceServer, Clauses, Timeout, Action, Env, Start, Msgs) ->
  erlang:yield(),
  {message_queue_len, CurrMsgs} = erlang:process_info(self(), message_queue_len),
  case CurrMsgs > Msgs of
    false ->
      find_message_loop(M, concrete, CodeServer, TraceServer, Clauses, Timeout, Action, Env, Start, Msgs);
    true ->
      {messages, Mailbox} = erlang:process_info(self(), messages),
      NewMsgs = lists:nthtail(Msgs, Mailbox),
      Message = find_message(M, concrete, CodeServer, TraceServer, Clauses, Env, NewMsgs),
      case Message of
        {Msg, Body, NewEnv, _Cnt} ->
          receive Msg -> ok end,
          eval_expr(M, concrete, CodeServer, TraceServer, Body, NewEnv);
        false ->
          find_message_loop(M, concrete, CodeServer, TraceServer, Clauses, Timeout, Action, Env, Start, CurrMsgs)
      end
  end.


%%-----------------------------------------------------------------------------
%% find_message(M, Mode, CodeServer, TraceServer, Cls, Env, Msgs)
%%   M :: atom()
%%   Mode :: concrete | symbolic
%%   CodeServer :: pid()
%%   TraceServer :: pid()
%%   Clauses :: [#c_clause{}]
%%   Timeout :: infinity | non_neg_integer()
%%   Action :: cerl()
%%   Env :: conc_lib:environment()
%%   Start :: timestamp()
%%   Mailbox :: [term()]
%% Iterates over a list of messages Mailbox and tries to match each one with
%% the appropriate clause from the list Cls. Returns the Body that will be
%% evaluated next, the new environment (that includes the mappings), and
%% the matched message.
%%-----------------------------------------------------------------------------
find_message(_M, concrete, _CodeServer, _TraceServer, _Clauses, _Env, []) ->
  false;
find_message(M, concrete, CodeServer, TraceServer, Clauses, Env, [Msg | Mailbox]) ->
  case find_clause(M, concrete, 'receive', CodeServer, TraceServer, Clauses, Msg, Env) of
    false ->
      find_message(M, concrete, CodeServer, TraceServer, Clauses, Env, Mailbox);
    {Body, NewEnv, Cnt} ->
      {Msg, Body, NewEnv, Cnt}
  end.
 

%%----------------------------------------------------------------------------
%% find_clause(M, Mode, CodeServer, TraceServer, Cls, Val, Env) -> Match
%%   M :: atom()
%%   Mode :: concrete | symbolic
%%   CodeServer :: pid()
%%   TraceServer :: pid()
%%   Cls :: [#c_clause{}]
%%   Val :: #semantic{}
%%   Env :: conc_lib:environment()
%%   Match = {Body, NewEnv}
%%     Body :: cerl()
%%     NewEnv :: conc_lib:environment()
%% Matches an evaluated switch expression Val with the appropriate
%% clause from the list Cls and returns the Body that will be
%% evaluated next and the new environment (that includes the mappings)
%%----------------------------------------------------------------------------
find_clause(M, concrete, Type, CodeServer, TraceServer, Clauses, Val, Env) ->
  find_clause(M, concrete, Type, CodeServer, TraceServer, Clauses, Val, Env, 1).
  
%% Helper function find_clause/8
find_clause(_M, concrete, _Type,_CodeServer, _TraceServer, [], _Val, _Env, _Cnt) ->
  %% only may happen at receive expressions,
  %% there is a match_fail clause at case expressions
  false;
find_clause(M, concrete, Type, CodeServer, TraceServer, [Cl|Cls], Val, Env, Cnt) ->
  Match = match_clause(M, concrete, Type, CodeServer, TraceServer, Cl, Val, Env, Cnt),
  case Match of
    false ->
      find_clause(M, concrete, Type, CodeServer, TraceServer, Cls, Val, Env, Cnt+1);
    {true, {Body, NewEnv, Cnt}} ->
      {Body, NewEnv, Cnt}
  end.
  
%%----------------------------------------------------------------------------
%% match_clause(M, Mode, CodeServer, TraceServer, Cl, Val, Env, Cnt) -> Match
%%   M :: atom()
%%   Mode :: concrete | symbolic
%%   CodeServer :: pid()
%%   TraceServer :: pid()
%%   Cl  :: #c_clause{}
%%   Val :: #semantic{}
%%   Env :: conc_lib:environment()
%%   Cnt :: non_neg_integer()
%%   Match = {true, Body, NewEnv} | false
%%     Body :: cerl()
%%     NewEnv :: conc_lib:environment()
%% Matches a clause Cl with an evaluated switch expression Val.
%% If the match succeeds, it returns the Body that will be evaluated and
%% the new environment (that includes the added mappings).
%% If the match fails, it returns false.
%%----------------------------------------------------------------------------
match_clause(M, concrete, Type, CodeServer, TraceServer, {c_clause, _Anno, Pats, Guard, Body}, ArgVal, Env, Cnt) ->
  case is_patlist_compatible(Pats, ArgVal) of
    false ->
      false;
    true ->
      Degree = length(Pats),
      EVals = 
        case Degree of
          1 ->
            [ArgVal];
          _ ->
            {valuelist, Vals, Degree} = ArgVal,
            Vals
        end,
      Match = pattern_match_all(concrete, Type, TraceServer, Pats, EVals),
      case Match of
        false ->
          false;
        {true, Ms} ->
          NewEnv = conc_lib:add_mappings_to_environment(Ms, Env),
          GuardVal = eval_expr(M, concrete, CodeServer, TraceServer, Guard, NewEnv),
          case GuardVal of
            false ->
              false;
            true ->
              {true, {Body, NewEnv, Cnt}}
          end
      end
  end.
  
  
%%----------------------------------------------------------------------
%% pattern_match(Mode, TraceServer, Pat, Val) -> Match
%%   Mode :: concrete | symbolic
%%   TraceServer :: pid()
%%   Pat :: cerl()
%%   Val :: term()
%%   Match = {true, Map} | false
%%     Map :: [{semantic_var(), semantic_value()}]
%% Pattern Match an evaluated expression Val with a pattern Pat.
%% If it succeeds, it returns {true, Map}, yielding a mapping Map.
%% If it fails, it returns false.
%%----------------------------------------------------------------------

%% AtomicLiteral pattern
pattern_match(concrete, _Type, _TraceServer, {c_literal, _Anno, LitVal}, V) ->
  case LitVal =:= V of
    true ->
      {true, []};
    false ->
      false
  end;
  
%% VariableName pattern
pattern_match(concrete, _Type, _TraceServer, {c_var, _Anno, Name}, V) ->
  {true, [{Name, V}]};
  
%% Tuple pattern
pattern_match(concrete, Type,  TraceServer, {c_tuple, _Anno, Es}, V)
  when is_tuple(V) ->
    Vs = tuple_to_list(V),
    pattern_match_all(concrete, Type, TraceServer, Es, Vs);
pattern_match(concrete, _Type, _TraceServer, {c_tuple, _Anno, _Es}, _V) ->
  false;
  
%% List constructor pattern
%% TODO tail recursive
pattern_match(concrete, Type, TraceServer, {c_cons, _Anno, Hd, Tl}, [V|Vs]) ->
  case pattern_match(concrete, Type, TraceServer, Hd, V) of
    false ->
      false;
    {true, Mapping_Hd} ->
      case pattern_match(concrete, Type, TraceServer, Tl, Vs) of
        false ->
          false;
        {true, Mapping_Tl} ->
          %% Creating deep list of mappings
          {true, [Mapping_Hd | Mapping_Tl]}
      end
  end;
pattern_match(concrete, _Type, _TraceServer, {c_cons, _Anno, _Hd, _Tl}, _V) ->
  false;
  
%% Alias pattern
pattern_match(concrete, Type, TraceServer, {c_alias, _Anno, Var, Pat}, V) ->
  Match = pattern_match(concrete, Type, TraceServer, Pat, V),
  case Match of
    false ->
      false;
    {true, Mapping} ->
      VarName = Var#c_var.name,
      {true, [{VarName, V}|Mapping]}
  end.

%% Helper functions pattern_match_all/4 and pattern_match_all/5
%% that apply pattern_matching to a sequence of patterns and values
pattern_match_all(concrete, Type, TraceServer, Pats, EVals) ->
  pattern_match_all(concrete, Type, TraceServer, Pats, EVals, []).
  
%% Helper function pattern_match_all/6
pattern_match_all(concrete, _Type, _TraceServer, [] ,[], Mappings) ->
  {true, Mappings};
pattern_match_all(concrete, _Type, _TraceServer, _Pats, [], _Mappings) ->
  false;
pattern_match_all(concrete, _Type, _TraceServer, [], _EVals, _Mappings) ->
  false;
pattern_match_all(concrete, Type, TraceServer, [Pat|Pats], [EVal|EVals], Mappings) ->
  Match = pattern_match(concrete, Type, TraceServer, Pat, EVal),
  case Match of
    {true, Maps} ->
      %% Creating deep list of mappings
      pattern_match_all(concrete, Type, TraceServer, Pats, EVals, [Maps | Mappings]);
    false ->
      false
  end.

%%====================================================================
%% Helper functions
%%====================================================================


%% Creates a Closure when the MFA code is loaded or is a function bound in a letrec
create_closure(M, F, Arity, concrete, CodeServer, TraceServer) ->
  %% Module is already loaded since create_closure is called by eval_expr/6
  {ok, MDb} = get_module_db(M, CodeServer),
  Key = {M, F, Arity},
  {Def, _Exported} = retrieve_function(Key, MDb),
  Env = conc_lib:new_environment(),
  make_fun(M, F, Arity, concrete, CodeServer, TraceServer, Def#c_fun.vars, Def#c_fun.body, Env, true).
  
create_closure(M, F, Arity, concrete, CodeServer, TraceServer, Def, Env) ->
  make_fun(M, F, Arity, concrete, CodeServer, TraceServer, Def#c_fun.vars, Def#c_fun.body, Env, true).

make_fun(Mod, _Func, Arity, concrete, CodeServer, TraceServer, Vars, Body, OuterEnv, _TraceF) ->
  %% Manually creating anonymous func and not use a list Args for parameters
  %% since the problem is that high order functions don't always expect a /1 function
  case Arity of
    0 ->
      fun() ->
        eval_expr(Mod, concrete, CodeServer, TraceServer, Body, OuterEnv)
      end;
    1 ->
      fun(A) ->
        Args = [A],
        Env = bind_parameters(Args, Vars, OuterEnv),
        eval_expr(Mod, concrete, CodeServer, TraceServer, Body, Env)
      end;
    2 ->
      fun(A, B) ->
        Args = [A, B],
        Env = bind_parameters(Args, Vars, OuterEnv),
        eval_expr(Mod, concrete, CodeServer, TraceServer, Body, Env)
      end;
    3 ->
      fun(A, B, C) ->
        Args = [A, B, C],
        Env = bind_parameters(Args, Vars, OuterEnv),
        eval_expr(Mod, concrete, CodeServer, TraceServer, Body, Env)
      end;
    4 ->
      fun(A, B, C, D) ->
        Args = [A, B, C, D],
        Env = bind_parameters(Args, Vars, OuterEnv),
        eval_expr(Mod, concrete, CodeServer, TraceServer, Body, Env)
      end;
    5 ->
      fun(A, B, C, D, E) ->
        Args = [A, B, C, D, E],
        Env = bind_parameters(Args, Vars, OuterEnv),
        eval_expr(Mod, concrete, CodeServer, TraceServer, Body, Env)
      end;
    6 ->
      fun(A, B, C, D, E, F) ->
        Args = [A, B, C, D, E, F],
        Env = bind_parameters(Args, Vars, OuterEnv),
        eval_expr(Mod, concrete, CodeServer, TraceServer, Body, Env)
      end;
    7 ->
      fun(A, B, C, D, E, F, G) ->
        Args = [A, B, C, D, E, F, G],
        Env = bind_parameters(Args, Vars, OuterEnv),
        eval_expr(Mod, concrete, CodeServer, TraceServer, Body, Env)
      end;
    _ ->
      exception('error', {lambda_fun_argument_limit, Arity})
  end.
  
%% Creates a closure for make_fun when no information on MFA is available
make_fun(Mod, Func, Arity, concrete, CodeServer, TraceServer) ->
  case Arity of
    0 ->
      fun() ->
        eval({named, {Mod, Func}}, [], concrete, external, CodeServer, TraceServer)
      end;
    1 ->
      fun(A) ->
        Args = [A],
        eval({named, {Mod, Func}}, Args, concrete, external, CodeServer, TraceServer)
      end;
    2 ->
      fun(A, B) ->
        Args = [A, B],
        eval({named, {Mod, Func}}, Args, concrete, external, CodeServer, TraceServer)
      end;
    3 ->
      fun(A, B, C) ->
        Args = [A, B, C],
        eval({named, {Mod, Func}}, Args, concrete, external, CodeServer, TraceServer)
      end;
    4 ->
      fun(A, B, C, D) ->
        Args = [A, B, C, D],
        eval({named, {Mod, Func}}, Args, concrete, external, CodeServer, TraceServer)
      end;
    5 ->
      fun(A, B, C, D, E) ->
        Args = [A, B, C, D, E],
        eval({named, {Mod, Func}}, Args, concrete, external, CodeServer, TraceServer)
      end;
    6 ->
      fun(A, B, C, D, E, F) ->
        Args = [A, B, C, D, E, F],
        eval({named, {Mod, Func}}, Args, concrete, external, CodeServer, TraceServer)
      end;
    _ ->
      exception('error', {lambda_fun_argument_limit, Arity})
  end.

%% -------------------------------------------------------
%% register_and_apply(TraceServer, Parent, Args)
%%   TraceServer :: pid()
%%   Parent :: pid()
%%   Args :: [term()]
%% Initializations called when a new process is spawned
%% The process registers its parent to the TraceServer
%% and proceeds with interpreting the MFA
%% -------------------------------------------------------
register_and_apply(TraceServer, Parent, Args) ->
  fun() ->
    register_to_trace(TraceServer, Parent),
    Parent ! {self(), registered},
    erlang:apply(conc_eval, eval, Args)
  end.
 
%% Helper function register_to_trace/2
register_to_trace(TraceServer, Parent) ->
  gen_server:call(TraceServer, {register_parent, Parent}).
  
%% Send Trace Data to Trace Server
send_trace(TraceServer, Msg, true) ->
  gen_server:cast(TraceServer, {trace, self(), Msg});
send_trace(_TraceServer, _Msg, false) ->
  ok.
  
%% ----------------------------------------------------------
%% get_module_db(M, CodeServer) -> Info
%%   M :: atom()
%%   CodeServer :: pid()
%%   Info :: {ok, MDb} | preloaded
%%    MDb :: ets:tid()
%% Interacts with the CodeServer and asks for the ETS Table
%% where the code of the module M is stored.
%% ----------------------------------------------------------
get_module_db(M, CodeServer) ->
  case get(M) of
    undefined ->
      case gen_server:call(CodeServer, {load, M}) of
        {ok, MDb} ->
          %% Module Code loaded
          put(M, MDb),
          {ok, MDb};
        preloaded ->
          %% Preloaded Module
          preloaded;
        cover_compiled ->
          %% Cover Compiled Module
          exception(error, {cover_compiled, M});
        non_existing ->
          %% Invalid Module
          exception(error, {undef, M});
        {error, Error} ->
          %% Any Error during Code Loading
          exception(error, Error)
      end;
    MDb ->
      {ok, MDb}
  end.
  
%% ----------------------------------------------------------
%% retrieve_function({M, F, Arity}, MDb) -> Info
%%   M :: atom()
%%   F :: atom()
%%   Arity :: non_neg_integer()
%%   MDb :: ets:tid()
%%   Info :: {#c_def{}, boolean()}
%% Retrieves the code and exported type of an MFA
%% ----------------------------------------------------------
retrieve_function(FuncKey, ModDb) ->
  case get(FuncKey) of
    undefined ->
      case ets:lookup(ModDb, FuncKey) of
        [] ->
          exception(error, {undef, FuncKey});
        [{FuncKey, Val}] ->
          put(FuncKey, Val),
          Val
      end;
    Val ->
      Val
  end.
  
%% Ensures compatibility between calltype and exported type
check_exported(true, _CallType, _MFA) -> ok;
check_exported(false, local, _MFA)    -> ok;
check_exported(false, external, MFA) -> exception(error, {not_exported, MFA}).

%% Bind the parametres of a function to their actual values
bind_parameters([], [], Env) ->
  Env;
bind_parameters([Arg|Args], [Var|Vars], Env) ->
  NewEnv = conc_lib:add_binding(Var#c_var.name, Arg, Env),
  bind_parameters(Args, Vars, NewEnv).

%% calculated the calltype of an MFA from inside another function
find_call_type(M1, M2) ->
  case M1 =:= M2 of
    true  -> local;
    false -> external
  end.

%% Calculates if the number of patterns in a clause 
%% is compatible to the numbers of actual values
%% that are trying to be match to
is_patlist_compatible(Pats, Values) ->
  Degree = length(Pats),
  case {Degree, Values} of
    {1, {valuelist, _Vals, _N}} ->
      false;
    {1, _} ->
      true;
    {N, {valuelist, _Vals, N}} ->
      true;
    {_N, _} ->
      false
  end.
  
%% Y combinator for a function with arity 0
y(M) ->
  G = fun(F) -> M(fun() -> (F(F))() end) end,
  G(G).
  
%% validate the timeout value of a receive expression
check_timeout(infinity) ->
  true;
check_timeout(Timeout) when is_integer(Timeout) -> 
  Timeout >= 0;
check_timeout(Timeout) ->
  exception(error, {invalid_timeout, Timeout}).
  
