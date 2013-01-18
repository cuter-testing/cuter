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
  SymbA = conc_symb:abstract(A),
  Mapping = conc_symb:generate_mapping(A, SymbA),
  Args = [{named, {M, F}}, A, SymbA, external, CodeServer, TraceServer],
  I = fun() ->
    register_to_trace(TraceServer, Root, false),
    Val = apply(conc_eval, eval, Args),
    Root ! {R, Val}
  end,
  erlang:spawn(I),
  receive
    {R, Val} -> 
      {ok, {Mapping, Val}};
    {error, {Who, Error}} -> 
      {Cv, Sv} = unzip_reason(Error),
      {error, {Mapping, {Who, Cv, Sv}}}
  end.
  
%% ===============
%% eval
%% ===============

%% Concrete Evaluation of MFA


%% Handle functions that raise exceptions
%% so as to zip the concrete and symbolic reason

%% Handle throw/1
eval({named, {erlang, throw}}, CAs, SAs, _CallType, _CodeServer, _TraceServer) ->
  case length(CAs) of
    1 ->
      [Throw] = zip_vals(CAs, SAs),
      erlang:throw(Throw);
    N ->
      exception(error, {undef, {erlang, throw, N}})
  end;
  
%% Handle exit/1
eval({named, {erlang, exit}}, CAs, SAs, _CallType, _CodeServer, _TraceServer)
  when length(CAs) =:= 1 ->
    [Exit] = zip_vals(CAs, SAs),
    erlang:exit(Exit);
    
%% Handle error/1, error/2
eval({named, {erlang, error}}, CAs, SAs, _CallType, _CodeServer, _TraceServer) ->
  case length(CAs) of
  1 ->
    [Error] = zip_vals(CAs, SAs),
    erlang:error(Error);
  2 ->
    [CError, CArgs] = CAs,
    [SError, _SArgs] = SAs,
    [Error] = zip_vals([CError], [SError]),
    erlang:error(Error, CArgs);
  N ->
      exception(error, {undef, {erlang, error, N}})
  end;
  
%% Handle raise/3
eval({named, {erlang, raise}}, CAs, SAs, _CallType, _CodeServer, _TraceServer) ->
  case length(CAs) of
    3 ->
      [CClass, CReason, CStacktrace] = CAs,
      [_SClass, SReason, _] = SAs,
      [R] = zip_vals([CReason], [SReason]),
      %% TODO
      %% Create constraint Class=SClass
      io:format("[raise]: ~p = ~p~n", [CClass, _SClass]),
      erlang:raise(CClass, R, CStacktrace);
    N ->
        exception(error, {undef, {erlang, raise, N}})
  end;

%% Handle make_fun/3  
eval({named, {erlang, make_fun}}, CAs, SAs, _CallType, CodeServer, TraceServer) ->
  case CAs of
    [M, F, Arity] ->
      CR = make_fun(M, F, Arity, CodeServer, TraceServer),
      SR = conc_symb:mock_bif({erlang, make_fun, 3}, SAs),
      {CR, SR};
    _ ->
      exception(error, {undef, {erlang, make_fun, length(CAs)}})
  end;
  

%% Handle an MFA
eval({named, {M, F}}, CAs, SAs, CallType, CodeServer, TraceServer) ->
  Arity = length(CAs),
  case conc_lib:is_bif(M, F, Arity) of
    true ->
      CR = apply(M, F, CAs),
      SR = conc_symb:mock_bif({M, F, Arity}, SAs),
      {CR, SR};
    false ->
      case get_module_db(M, CodeServer) of
        preloaded ->
          CR = apply(M, F, CAs),
          SR = conc_symb:mock_bif({M, F, Arity}, SAs),
          {CR, SR};
        {ok, MDb} ->
          Key = {M, F, Arity},
          {Def, Exported} = retrieve_function(Key, MDb),
%          io:format("Def=~n~p~n", [Def]),
          check_exported(Exported, CallType, Key),
          Cenv = bind_parameters(CAs, Def#c_fun.vars, conc_lib:new_environment()),
          Senv = bind_parameters(SAs, Def#c_fun.vars, conc_lib:new_environment()),
          eval_expr(M, CodeServer, TraceServer, Def#c_fun.body, Cenv, Senv)
      end
  end;
  
%% Handle a Closure
eval({lambda, Closure}, CAs, SAs, _CallType, _CodeServer, _TraceServer) ->
  ZAs = zip_vals(CAs, SAs),
  apply(Closure, ZAs);
  
%% Handle a function bound in a letrec expression
eval({letrec_func, {M, _F, Def, E}}, CAs, SAs, _CallType, CodeServer, TraceServer) ->
  {Cenv, Senv} = E(),
  NCenv = bind_parameters(CAs, Def#c_fun.vars, Cenv),
  NSenv = bind_parameters(SAs, Def#c_fun.vars, Senv),
  eval_expr(M, CodeServer, TraceServer, Def#c_fun.body, NCenv, NSenv).

  
  
%%--------------------------------------------------------------------
%% exception(Class, Reason)
%%   Class :: error | exit | throw
%%   Reason :: term()
%% Raises the exception.
%%--------------------------------------------------------------------
exception(Class, Reason) ->
  erlang:Class(Reason).

%% ===============
%% eval_expr
%% ===============

%c_apply
eval_expr(M, CodeServer, TraceServer, {c_apply, _Anno, Op, Args}, Cenv, Senv) ->
  %% Will use OPsv for constraint OPsv=OPcv (maybe)
  {OPcv, _OPsv} = eval_expr(M, CodeServer, TraceServer, Op, Cenv, Senv),
  ZAs = lists:map(
    fun(A) -> %% Will create closures where appropriate
      {CA, SA} = eval_expr(M, CodeServer, TraceServer, A, Cenv, Senv),
      case CA of
        {func, {F, Arity}} ->
          Cl = create_closure(M, F, Arity, CodeServer, TraceServer, local),
          {Cl, Cl};
        {letrec_func, {Mod, F, Arity, Def, E}} ->
          {CE, SE} = E(),
          Cl = create_closure(Mod, F, Arity, CodeServer, TraceServer, {letrec_fun, {Def, CE, SE}}),
          {Cl, Cl};
        _ ->
          {CA, SA}
      end
    end,
    Args),
  {CAs, SAs} = lists:unzip(ZAs),
  case OPcv of %% Check eval_expr(..., #c_var{}, ...) output for reference
    {func, {Func, _Arity}} ->
      eval({named, {M, Func}}, CAs, SAs, local, CodeServer, TraceServer);
    {letrec_func, {Mod, Func, _Arity, Def, E}} ->
      eval({letrec_func, {Mod, Func, Def, E}}, CAs, SAs, local, CodeServer, TraceServer);
    Closure ->
      %% TODO
      %% Will make constraint OPsv=OPcv (in case closure is made by make_fun)
      io:format("[c_apply]: ~p = ~p~n", [OPcv, _OPsv]),
      eval({lambda, Closure}, CAs, SAs, local, CodeServer, TraceServer)
  end;
  
%c_binary
eval_expr(_M, _CodeServer, _TraceServer, {c_binary, _Anno, _Segments}, _Cenv, _Senv) ->
  exception(error, c_binary);
  
%c_bitstr
eval_expr(_M, _CodeServer, _TraceServer, {c_bitstr, _Anno, _Val, _Size, _Unit, _Type, _Flags}, _Cenv, _Senv) ->
  exception(error, c_bitstr);
  
%c_call
eval_expr(M, CodeServer, TraceServer, {c_call, _Anno, Mod, Name, Args}, Cenv, Senv) ->
  {Mcv, _Msv} = eval_expr(M, CodeServer, TraceServer, Mod, Cenv, Senv),
  {Fcv, _Fsv} = eval_expr(M, CodeServer, TraceServer, Name, Cenv, Senv),
  ZAs = lists:map(
    fun(A) -> %% Will create closures where appropriate
      {CA, SA} = eval_expr(M, CodeServer, TraceServer, A, Cenv, Senv),
      case CA of
        {func, {F, Arity}} ->
          Cl = create_closure(M, F, Arity, CodeServer, TraceServer, local),
          {Cl, Cl};
        {letrec_func, {Mod, F, Arity, Def, E}} ->
          {CE, SE} = E(),
          Cl = create_closure(Mod, F, Arity, CodeServer, TraceServer, {letrec_fun, {Def, CE, SE}}),
          {Cl, Cl};
        _ ->
          {CA, SA}
      end
    end,
    Args),
  {CAs, SAs} = lists:unzip(ZAs),
  %% TODO
  %% Will make constraints Mcv=Msv and Fcv=Fsv
  io:format("[c_call]: ~p = ~p~n", [Mcv, _Msv]),
  io:format("[c_call]: ~p = ~p~n", [Fcv, _Fsv]),
  eval({named, {Mcv, Fcv}}, CAs, SAs, find_call_type(M, Mcv), CodeServer, TraceServer);

%c_case
eval_expr(_M, _CodeServer, _TraceServer, {c_case, _Anno, _Arg, _Clauses}, _Cenv, _Senv) ->
  exception(error, c_case);

%c_catch
eval_expr(M, CodeServer, TraceServer, {c_catch, _Anno, Body}, Cenv, Senv) ->
  try
    eval_expr(M, CodeServer, TraceServer, Body, Cenv, Senv)
  catch
    throw:Throw ->
      {Cv, Sv} = unzip_reason(Throw),
      {Cv, Sv};
    exit:Exit ->
      {Cv, Sv} = unzip_reason(Exit),
      {{'EXIT', Cv}, {'EXIT', Sv}};
    error:Error ->
      %% CAUTION! Stacktrace info is not valid
      %% It refers to the interpreter process's stacktrace
      %% Used for internal debugging
      {Cv, Sv} = unzip_reason(Error),
      Stacktrace = erlang:get_stacktrace(),
      {{'EXIT', {Cv, Stacktrace}}, {'EXIT', {Sv, Stacktrace}}}
  end;

%c_cons
eval_expr(M, CodeServer, TraceServer, {c_cons, _Anno, Hd, Tl}, Cenv, Senv) ->
  {Hdcv, Hdsv} = eval_expr(M, CodeServer, TraceServer, Hd, Cenv, Senv),
  {Tlcv, Tlsv} = eval_expr(M, CodeServer, TraceServer, Tl, Cenv, Senv),
  {[Hdcv|Tlcv], [Hdsv|Tlsv]};

%c_fun
eval_expr(M, CodeServer, TraceServer, {c_fun, _Anno, Vars, Body}, Cenv, Senv) ->
  Arity = length(Vars),
  Lambda = make_fun(M, Arity, CodeServer, TraceServer, Vars, Body, Cenv, Senv),
  {Lambda, Lambda};

%c_let
eval_expr(M, CodeServer, TraceServer, {c_let, _Anno, Vars, Arg, Body}, Cenv, Senv) ->
  Degree = length(Vars),
  {C, S} = eval_expr(M, CodeServer, TraceServer, Arg, Cenv, Senv),
  case Degree of
    1 ->
      CAs = [C],
      SAs = [S];
    _ ->
      {valuelist, CAs, Degree} = C,
      {valuelist, SAs, Degree} = S
  end,
  NCenv = bind_parameters(CAs, Vars, Cenv),
  NSenv = bind_parameters(SAs, Vars, Senv),
  eval_expr(M, CodeServer, TraceServer, Body, NCenv, NSenv);

%c_letrec
eval_expr(M, CodeServer, TraceServer, {c_letrec, _Anno, Defs, Body}, Cenv, Senv) ->
  H = fun(F) -> fun() ->
    lists:foldl(
      fun({Func, Def}, {Ce, Se}) ->
        NCe = conc_lib:add_binding(Func#c_var.name, {letrec_func, {M, Def, F}}, Ce),
        NSe = conc_lib:add_binding(Func#c_var.name, {letrec_func, {M, Def, F}}, Se),
        {NCe, NSe}
      end,
      {Cenv, Senv}, Defs
    )
  end end,
  %% NewEnv is now a /0 function
  %% NewEnv() will create the necessary self-referenced environment
  {NCenv, NSenv} = (y(H))(),
  eval_expr(M, CodeServer, TraceServer, Body, NCenv, NSenv);

%c_literal
eval_expr(_M, _CodeServer, _TraceServer, {c_literal, _Anno, Val}, _Cenv, _Senv) ->
  {Val, Val};

%c_primop
eval_expr(_M, _CodeServer, _TraceServer, {c_primop, _Anno, _Name, _Args}, _Cenv, _Senv) ->
  exception(error, c_primop);

%c_receive
eval_expr(_M, _CodeServer, _TraceServer, {c_receive, _Anno, _Clauses, _Timeout, _Action}, _Cenv, _Senv) ->
  exception(error, c_receive);
  
%c_seq
eval_expr(M, CodeServer, TraceServer, {c_seq, _Anno, Arg, Body}, Cenv, Senv) ->
  _Val = eval_expr(M, CodeServer, TraceServer, Arg, Cenv, Senv),
  eval_expr(M, CodeServer, TraceServer, Body, Cenv, Senv);

%c_try
eval_expr(_M, _CodeServer, _TraceServer, {c_try, _Anno, _Arg, _Vars, _Body, _Evars, _Handler}, _Cenv, _Senv) ->
  exception(error, c_try);

%c_tuple
eval_expr(M, CodeServer, TraceServer, {c_tuple, _Anno, Es}, Cenv, Senv) ->
  ZEs = lists:map(
    fun(E) -> eval_expr(M, CodeServer, TraceServer, E, Cenv, Senv) end,
    Es),
  {CEs, SEs} = lists:unzip(ZEs),
  {list_to_tuple(CEs), list_to_tuple(SEs)};

%c_values
eval_expr(M, CodeServer, TraceServer, {c_values, _Anno, Es}, Cenv, Senv) ->
  Degree = length(Es),
  ZEs = lists:map(
    fun(E) -> eval_expr(M, CodeServer, TraceServer, E, Cenv, Senv) end,
    Es),
  {CEs, SEs} = lists:unzip(ZEs),
  {#valuelist{values=CEs, degree=Degree}, #valuelist{values=SEs, degree=Degree}};

%c_var
eval_expr(_M, _CodeServer, _TraceServer, {c_var, _Anno, Name}, Cenv, Senv)
  when is_tuple(Name) ->
    %% If Name is a function
    case conc_lib:get_value(Name, Cenv) of
      Closure when is_function(Closure) -> %% Closure
        {ok, Sv} = conc_lib:get_value(Name, Senv),
        {Closure, Sv};
      {ok, {letrec_func, {Mod, Def, E}}} ->  %% Fun bound in a letrec
        {Fun, Arity} = Name,
        R = {letrec_func, {Mod, Fun, Arity, Def, E}},
        {R, R};
      error -> %% either local in module or external
        {Fun, Arity} = Name,
        R = {func, {Fun, Arity}},
        {R, R}
    end;
eval_expr(_M, _CodeServer, _TraceServer, {c_var, _Anno, Name}, Cenv, Senv) ->
  %% If it's a variable then return its value
  {ok, Cval} = conc_lib:get_value(Name, Cenv),
  {ok, Sval} = conc_lib:get_value(Name, Senv),
  {Cval, Sval}.









%% ===============
%% create_closure
%% ===============

%% Creates a Closure of a local function
create_closure(M, F, Arity, CodeServer, TraceServer, local) ->
  %% Module is already loaded since create_closure is called by eval_expr
  {ok, MDb} = get_module_db(M, CodeServer),
  Key = {M, F, Arity},
  {Def, _Exported} = retrieve_function(Key, MDb),
  Cenv = conc_lib:new_environment(),
  Senv = conc_lib:new_environment(),
  make_fun(M, Arity, CodeServer, TraceServer, Def#c_fun.vars, Def#c_fun.body, Cenv, Senv);
  
%% Creates a Closure when the MFA is a function bound in a letrec
create_closure(M, _F, Arity, CodeServer, TraceServer, {letrec_fun, {Def, Cenv, Senv}}) ->
  make_fun(M, Arity, CodeServer, TraceServer, Def#c_fun.vars, Def#c_fun.body, Cenv, Senv).



%% ===============
%% make_fun
%% ===============
%% Manually creating anonymous func and not use a list Args for parameters
%% since the problem is that high order functions don't always expect a /1 function
make_fun(Mod, Arity, CodeServer, TraceServer, Vars, Body, Cenv, Senv) ->
  case Arity of
    0 ->
      fun() ->
        eval_expr(Mod, CodeServer, TraceServer, Body, Cenv, Senv)
      end;
    1 ->
      fun(A) ->
        Args = [A],
        {CAs, SAs} = unzip_vals(Args),
        NCenv = bind_parameters(CAs, Vars, Cenv),
        NSenv = bind_parameters(SAs, Vars, Senv),
        eval_expr(Mod, CodeServer, TraceServer, Body, NCenv, NSenv)
      end;
    2 ->
      fun(A, B) ->
        Args = [A, B],
        {CAs, SAs} = unzip_vals(Args),
        NCenv = bind_parameters(CAs, Vars, Cenv),
        NSenv = bind_parameters(SAs, Vars, Senv),
        eval_expr(Mod, CodeServer, TraceServer, Body, NCenv, NSenv)
      end;
    3 ->
      fun(A, B, C) ->
        Args = [A, B, C],
        {CAs, SAs} = unzip_vals(Args),
        NCenv = bind_parameters(CAs, Vars, Cenv),
        NSenv = bind_parameters(SAs, Vars, Senv),
        eval_expr(Mod, CodeServer, TraceServer, Body, NCenv, NSenv)
      end;
    4 ->
      fun(A, B, C, D) ->
        Args = [A, B, C, D],
        {CAs, SAs} = unzip_vals(Args),
        NCenv = bind_parameters(CAs, Vars, Cenv),
        NSenv = bind_parameters(SAs, Vars, Senv),
        eval_expr(Mod, CodeServer, TraceServer, Body, NCenv, NSenv)
      end;
    _ ->
      exception('error', {over_lambda_fun_argument_limit, Arity})
  end.

%% Creates a closure for make_fun when no information on MFA is available
make_fun(Mod, Func, Arity, CodeServer, TraceServer) ->
  case Arity of
    0 ->
      fun() ->
        eval({named, {Mod, Func}}, [], [], external, CodeServer, TraceServer)
      end;
    1 ->
      fun(A) ->
        Args = [A],
        {CAs, SAs} = unzip_vals(Args),
        eval({named, {Mod, Func}}, CAs, SAs, external, CodeServer, TraceServer)
      end;
    2 ->
      fun(A, B) ->
        Args = [A, B],
        {CAs, SAs} = unzip_vals(Args),
        eval({named, {Mod, Func}}, CAs, SAs, external, CodeServer, TraceServer)
      end;
    3 ->
      fun(A, B, C) ->
        Args = [A, B, C],
        {CAs, SAs} = unzip_vals(Args),
        eval({named, {Mod, Func}}, CAs, SAs, external, CodeServer, TraceServer)
      end;
    4 ->
      fun(A, B, C, D) ->
        Args = [A, B, C, D],
        {CAs, SAs} = unzip_vals(Args),
        eval({named, {Mod, Func}}, CAs, SAs, external, CodeServer, TraceServer)
      end;
    _ ->
      exception('error', {over_lambda_fun_argument_limit, Arity})
  end.
  
  
%% Zip and Unzip concrete-semantic values
%% Zipped Args are [{'_zip', CA, SA}]
zip_vals(CAs, SAs) ->
  F = fun(C, S) ->
    {'_zip', C, S}
  end,
  lists:zipwith(F, CAs, SAs).
  
unzip_vals(As) -> 
  F = fun(A) ->
    case A of
      {'_zip', CA, SA} ->
        {CA, SA};
      _ ->
        {A, A}
    end
  end,
  ZAs = lists:map(F, As),
  lists:unzip(ZAs).
  
unzip_reason({'_zip', Cv, Sv}) ->
  {Cv, Sv};
unzip_reason(R) when is_tuple(R) ->
  L = tuple_to_list(R),
  Zs = lists:map(
    fun(X) -> unzip_reason(X) end,
    L),
  {Cs, Ss} = lists:unzip(Zs),
  {list_to_tuple(Cs), list_to_tuple(Ss)};
unzip_reason(R) when is_list(R) ->
  Zs = lists:map(
    fun(X) -> unzip_reason(X) end,
    R),
  lists:unzip(Zs);
unzip_reason(R) ->
  {R, R}.
  
%% -------------------------------------------------------
%% register_and_apply(TraceServer, Parent, Args)
%%   TraceServer :: pid()
%%   Parent :: pid()
%%   Args :: [term()]
%% Initializations called when a new process is spawned
%% The process registers its parent to the TraceServer
%% and proceeds with interpreting the MFA
%% -------------------------------------------------------
register_and_apply(TraceServer, Parent, Args, Link) ->
  fun() ->
    register_to_trace(TraceServer, Parent, Link),
    Parent ! {self(), registered},
    erlang:apply(conc_eval, eval, Args)
  end.
  
register_to_trace(TraceServer, Parent, Link) ->
  gen_server:call(TraceServer, {register_parent, Parent, Link}).
  
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
%%
%% Optimization : For caching purposes, the MDb is stored
%% in the process dictionary for later lookups
%% ----------------------------------------------------------
get_module_db(M, CodeServer) ->
  case get({conc, M}) of
    undefined ->
      case gen_server:call(CodeServer, {load, M}) of
        %% Module Code loaded
        {ok, MDb} ->
          put({conc, M}, MDb),
          {ok, MDb};
        %% Preloaded Module
        preloaded ->
          preloaded;
        %% Cover Compiled Module
        cover_compiled ->
          exception(error, {cover_compiled, M});
        %% Invalid Module
        non_existing ->
          exception(error, {undef, M});
        %% Any Error during Code Loading
        {error, Error} ->
          exception(error, Error)
      end;
    MDb ->
      {ok, MDb}
  end.
  
%% ---------------------------------------------------------------
%% retrieve_function({M, F, Arity}, MDb) -> Info
%%   M :: atom()
%%   F :: atom()
%%   Arity :: non_neg_integer()
%%   MDb :: ets:tid()
%%   Info :: {#c_def{}, boolean()}
%% Retrieves the code and exported type of an MFA
%%
%% Optimization : For caching purposes, the function definition
%% is stored in the process dictionary for later lookups
%% ---------------------------------------------------------------
retrieve_function(FuncKey, ModDb) ->
  case get({conc, FuncKey}) of
    undefined ->
      case ets:lookup(ModDb, FuncKey) of
        [] ->
          exception(error, {undef, FuncKey});
        [{FuncKey, Val}] ->
          put({conc, FuncKey}, Val),
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
  
%% Y combinator for a function with arity 0
y(M) ->
  G = fun(F) -> M(fun() -> (F(F))() end) end,
  G(G).
