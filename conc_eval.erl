-module(conc_eval).
-compile([export_all]).

-include_lib("compiler/src/core_parse.hrl").
-include("conc_lib.hrl").

%% TODO Missing Specs and Comments! Will be added soon!

%%--------------------------------------------------------------------------
%% i(M, F, A, CodeServer, TraceServer) -> Pid
%%   M :: atom()
%%   F :: atom()
%%   A :: [term()]
%%   CodeServer :: pid()
%%   TraceServer :: pid()
%%   Pid :: pid()
%% Wrapper exported function that spawns an interpreter process
%% that returns the value of MFA to the Concolic Server
%%--------------------------------------------------------------------------
i(M, F, As, CodeServer, TraceServer) ->
  Root = self(),
  SymbAs = conc_symb:abstract(As),
  Mapping = conc_symb:generate_mapping(SymbAs, As),
  I = fun() ->
    {ok, _File} = conc_tserver:register_to_trace(TraceServer, Root, false),
    Args = [{named, {M, F}}, As, SymbAs, external, CodeServer, TraceServer],
    Val = apply(conc_eval, eval, Args),
    conc:send_return(Root, Mapping, Val)
  end,
  erlang:spawn(I).

  
%% ===============
%% eval
%% ===============

%% Concrete Evaluation of MFA

%% Handle spawns so that the spawned process
%% will be interpreted

%% Handle spawn/1, spawn/2, spawn/3, spawn/4,
%% spawn_link/1 spawn_link/2, spawn_link/3, spawn_link/4
eval({named, {erlang, F}}, CAs, SAs, _CallType, CodeServer, TraceServer)
  when F =:= spawn; F =:= spawn_link ->
    Arity = length(CAs),
    SAs_e = conc_symb:ensure_list(SAs, Arity),
    ChildPid =
      case CAs of
        [Fun] ->
          [_SFun] = SAs_e,
          %% TODO Constraint: SFun=Fun
          EvalArgs = [{lambda, Fun}, [], [], local, CodeServer, TraceServer],
          Child = register_and_apply(TraceServer, self(), EvalArgs, F =:= spawn_link),
          erlang:F(Child);
        [Node, Fun] ->
          [_SNode, _SFun] = SAs_e,
          %% TODO Constraints: SNode=Node, SFun=Fun
          {CServer, TServer} = conc_tserver:node_servers(TraceServer, Node),
          EvalArgs = [{lambda, Fun}, [], [], local, CServer, TServer],
          Child = register_and_apply(TServer, self(), EvalArgs, F =:= spawn_link),
          erlang:F(Node, Child);
        [Mod, Fun, Args] ->
          [_SMod, _SFun, SArgs] = SAs_e,
          %% TODO Constraints: SMod = Mod, SFun=Fun
          Call = find_call_type(erlang, Mod),
          EvalArgs = [{named, {Mod, Fun}}, Args, SArgs, Call, CodeServer, TraceServer],
          Child = register_and_apply(TraceServer, self(), EvalArgs, F =:= spawn_link),
          erlang:F(Child);
        [Node, Mod, Fun, Args] ->
          [_SNode, _SMod, _SFun, SArgs] = SAs_e,
          %% TODO Constraints: SNode=Node, SMod = Mod, SFun=Fun
          {CServer, TServer} = conc_tserver:node_servers(TraceServer, Node),
          Call = find_call_type(erlang, Mod),
          EvalArgs = [{named, {Mod, Fun}}, Args, SArgs, Call, CServer, TServer],
          Child = register_and_apply(TServer, self(), EvalArgs, F =:= spawn_link),
          erlang:F(Node, Child);
        _ ->
          exception(error, {undef, {erlang, spawn, Arity}})
      end,
    receive
      {ChildPid, registered} -> {ChildPid, ChildPid}
    end;

%% Handle spawn_monitor/1, spawn_monitor/3
eval({named, {erlang, spawn_monitor}}, CAs, SAs, _CallType, CodeServer, TraceServer) ->
  Arity = length(CAs),
  SAs_e = conc_symb:ensure_list(SAs, Arity),
  EvalArgs =
    case CAs of
      [Fun] ->
        [_SFun] = SAs_e,
        %% TODO Constraint: SFun=Fun
        [{lambda, Fun}, [], [], local, CodeServer, TraceServer];
      [Mod, Fun, Args] ->
        [_SMod, _SFun, SArgs] = SAs_e,
        %% TODO Constraints: SMod = Mod, SFun=Fun
        Call = find_call_type(erlang, Mod),
        [{named, {Mod, Fun}}, Args, SArgs, Call, CodeServer, TraceServer];
      _ ->
        exception(error, {undef, {erlang, spawn_monitor, Arity}})
    end,
  Child = register_and_apply(TraceServer, self(), EvalArgs, true),
  {ChildPid, ChildRef} = erlang:spawn_monitor(Child),
  receive
    {ChildPid, registered} -> {{ChildPid, ChildRef}, {ChildPid, ChildRef}}
  end;

%% Handle spawn_opt/2, spawn_opt/3, spawn_opt/4, spawn_opt/5
eval({named, {erlang, spawn_opt}}, CAs, SAs, _CallType, CodeServer, TraceServer) ->
  Arity = length(CAs),
  SAs_e = conc_symb:ensure_list(SAs, Arity),
  R =
    case CAs of
      [Fun, Opts] ->
        [_SFun, _SOpts] = SAs_e,
        %% TODO Constraints: SFun=Fun, SOpts=Opts
        EvalArgs = [{lambda, Fun}, [], [], local, CodeServer, TraceServer],
        Link = lists:member(link, Opts) orelse lists:member(monitor, Opts),
        Child = register_and_apply(TraceServer, self(), EvalArgs, Link),
        erlang:spawn_opt(Child, Opts);
      [Node, Fun, Opts] ->
        [_SNode, _SFun, _SOpts] = SAs_e,
        %% TODO Constraints: SNode=Node, SFun=Fun, SOpts=Opts
        {CServer, TServer} = conc_tserver:node_servers(TraceServer, Node),
        EvalArgs = [{lambda, Fun}, [], [], local, CServer, TServer],
        Link = lists:member(link, Opts) orelse lists:member(monitor, Opts),
        Child = register_and_apply(TServer, self(), EvalArgs, Link),
        erlang:spawn_opt(Node, Child, Opts);
      [Mod, Fun, Args, Opts] ->
        [_SMod, _SFun, SArgs, _SOpts] = SAs_e,
        %% TODO Constraints: SMod=Mode, SFun=Fun, SOpts=Opts
        Call = find_call_type(erlang, Mod),
        EvalArgs = [{named, {Mod, Fun}}, Args, SArgs, Call, CodeServer, TraceServer],
        Link = lists:member(link, Opts) orelse lists:member(monitor, Opts),
        Child = register_and_apply(TraceServer, self(), EvalArgs, Link),
        erlang:spawn_opt(Child, Opts);
      [Node, Mod, Fun, Args, Opts] ->
        [_SNode, _SMod, _SFun, SArgs, _SOpts] = SAs_e,
        %% TODO Constraints: SNode=Node, SMod=Mode, SFun=Fun, SOpts=Opts
        Call = find_call_type(erlang, Mod),
        {CServer, TServer} = conc_tserver:node_servers(TraceServer, Node),
        EvalArgs = [{named, {Mod, Fun}}, Args, SArgs, Call, CServer, TServer],
        Link = lists:member(link, Opts) orelse lists:member(monitor, Opts),
        Child = register_and_apply(TServer, self(), EvalArgs, Link),
        erlang:spawn_opt(Node, Child, Opts);
      _ ->
        exception(error, {undef, {erlang, spawn_opt, Arity}})
    end,
  ChildPid =
    case R of
      {Pid, _Ref} -> Pid;
      Pid -> Pid
    end,
  receive
    {ChildPid, registered} -> {R, R}
  end;

%% Handle message sending primitives
%% so as to zip the concrete and symbolic reason
  
%% Handle '!'/2
eval({named, {erlang, '!'}}, CAs, SAs, CallType, CodeServer, TraceServer)
  when length(CAs) =:= 2 ->
    eval({named, {erlang, send}}, CAs, SAs, CallType, CodeServer, TraceServer);

%% Handle send/2, send/3
eval({named, {erlang, send}}, CAs, SAs, _CallType, _CodeServer, TraceServer) ->
  Arity = length(CAs),
  SAs_e = conc_symb:ensure_list(SAs, Arity),
  case CAs of
    [CDest, CMsg] ->
      [_SDest, SMsg] = SAs_e,
      %% TODO Constraint: CDest=SDest
      Msg = encode_msg(TraceServer, CDest, CMsg, SMsg),
      erlang:send(CDest, Msg),
      {CMsg, SMsg};
    [CDest, CMsg, COpts] ->
      [_SDest, SMsg, _SOpts] = SAs_e,
      %% TODO Constraint: CDest=SDest, COpts=SOpts
      Msg = encode_msg(TraceServer, CDest, CMsg, SMsg),
      R = erlang:send(CDest, Msg, COpts),
      {R, R};
    _ ->
        exception(error, {undef, {erlang, send, Arity}})
  end;


%% Handle send_after/3
eval({named, {erlang, send_after}}, CAs, SAs, _CallType, _CodeServer, TraceServer) ->
  Arity = length(CAs),
  SAs_e = conc_symb:ensure_list(SAs, Arity),
  case CAs of
    [CTime, CDest, CMsg] ->
      [_STime, _SDest, SMsg] = SAs_e,
      %% TODO Constraint CTime=STime, CDest=SDest
      Msg = encode_msg(TraceServer, CDest, CMsg, SMsg),
      R = erlang:send_after(CTime, CDest, Msg),
      {R, R};
    _ ->
      exception(error, {undef, {erlang, send_after, Arity}})
  end;
 
%% Handle send_nosuspend/2, send_nosuspend/3
eval({named, {erlang, send_nosuspend}}, CAs, SAs, _CallType, _CodeServer, TraceServer) ->
  Arity = length(CAs),
  SAs_e = conc_symb:ensure_list(SAs, Arity),
  case CAs of
    [CDest, CMsg] ->
      [_SDest, SMsg] = SAs_e,
      %% TODO Constraint CDest=SDest
      Msg = encode_msg(TraceServer, CDest, CMsg, SMsg),
      R = erlang:send_nosuspend(CDest, Msg),
      {R, R};
    [CDest, CMsg, COpts] ->
      [_SDest, SMsg, _SOpts] = SAs_e,
      %% TODO Constraint CDest=SDest, COpts=SOpts
      Msg = encode_msg(TraceServer, CDest, CMsg, SMsg),
      R = erlang:send_nosuspend(CDest, Msg, COpts),
      {R, R};
    _ ->
      exception(error, {undef, {erlang, send_nosuspend, Arity}})
  end;


%% Handle functions that raise exceptions
%% so as to zip the concrete and symbolic reason

%% Handle throw/1
eval({named, {erlang, throw}}, CAs, SAs, _CallType, _CodeServer, _TraceServer) ->
  Arity = length(CAs),
  SAs_e = conc_symb:ensure_list(SAs, Arity),
  case CAs of
    [CThrow] ->
      [SThrow] = SAs_e,
      Throw = zip_one(CThrow, SThrow),
      erlang:throw(Throw);
    _ ->
      exception(error, {undef, {erlang, throw, Arity}})
  end;
  
%% Handle exit/1, exit2
eval({named, {erlang, exit}}, CAs, SAs, _CallType, _CodeServer, _TraceServer) ->
  Arity = length(CAs),
  SAs_e = conc_symb:ensure_list(SAs, Arity),
  case CAs of
    [CExit] ->
      [SExit] = SAs_e,
      Exit = 
        case CExit of
          normal ->
            %% TODO Constraint: SExit=normal
            normal;
          _ ->
            zip_one(CExit, SExit)
        end,
      erlang:exit(Exit);
    [CDest, CExit] ->
      [_SDest, SExit] = SAs_e,
      %% TODO Constraint CDest=SDest
      Exit = 
        case CExit of
          normal ->
            %% TODO Constraint: SExit=normal
            normal;
          kill ->
            %% TODO Constraint: SExit=kill
            kill;
          _ ->
            zip_one(CExit, SExit)
        end,
        R = erlang:exit(CDest, Exit),
        {R, R};
    _ ->
      exception(error, {undef, {erlang, exit, Arity}})
  end;

    
%% Handle error/1, error/2
eval({named, {erlang, error}}, CAs, SAs, _CallType, _CodeServer, _TraceServer) ->
  Arity = length(CAs),
  SAs_e = conc_symb:ensure_list(SAs, Arity),
  case CAs of
    [CError] ->
      [SError] = SAs_e,
      Error = zip_one(CError, SError),
      erlang:error(Error);
    [CError, CArgs] ->
      [SError, _SArgs] = SAs_e,
      %% TODO create constraint CArgs=SArgs
      Error = zip_one(CError, SError),
      erlang:error(Error, CArgs);
    _ ->
      exception(error, {undef, {erlang, error, Arity}})
  end;
  
%% Handle raise/3
eval({named, {erlang, raise}}, CAs, SAs, _CallType, _CodeServer, _TraceServer) ->
  Arity = length(CAs),
  SAs_e = conc_symb:ensure_list(SAs, Arity),
  case CAs of
    [CClass, CReason, CStacktrace] ->
      [_SClass, SReason, _] = SAs_e,
      %% TODO Create constraint Class=SClass
      R = zip_one(CReason, SReason),
      erlang:raise(CClass, R, CStacktrace);
    _ ->
        exception(error, {undef, {erlang, raise, Arity}})
  end;

%% Handle other important functions

%% Handle make_fun/3  
eval({named, {erlang, make_fun}}, CAs, SAs, _CallType, CodeServer, TraceServer) ->
  Arity = length(CAs),
  SAs_e = conc_symb:ensure_list(SAs, Arity),
  case CAs of
    [M, F, A] ->
      CR = make_fun(M, F, A, CodeServer, TraceServer),
      SR = conc_symb:mock_bif({erlang, make_fun, 3}, SAs_e),
      {CR, SR};
    _ ->
      exception(error, {undef, {erlang, make_fun, Arity}})
  end;
  
%% Handle apply/2, apply/3
eval({named, {erlang, apply}}, CAs, SAs, _CallType, CodeServer, TraceServer) ->
  Arity = length(CAs),
  SAs_e = conc_symb:ensure_list(SAs, Arity),
  EvalArgs =
    case CAs of
      [Fun, Args] ->
        [_SFun, SArgs] = SAs_e,
        %% TODO Constraint: Fun=SFun
        [{lambda, Fun}, Args, SArgs, local, CodeServer, TraceServer];
      [Mod, Fun, Args] ->
        [_SMod, _SFun, SArgs] = SAs_e,
        %% TODO Constraints: SMod = Mod, SFun=Fun
        Call = find_call_type(erlang, Mod),
        [{named, {Mod, Fun}}, Args, SArgs, Call, CodeServer, TraceServer];
      _ ->
        exception(error, {undef, {erlang, apply, Arity}})
    end,
  erlang:apply(conc_eval, eval, EvalArgs);
  

%% Handle an MFA
eval({named, {M, F}}, CAs, SAs, CallType, CodeServer, TraceServer) ->
  Arity = length(CAs),
  SAs_e = conc_symb:ensure_list(SAs, Arity),
%  io:format("~n~nCalling ~w:~w/~w~n",[M,F,Arity]),
%  io:format("CAs = ~w~n", [CAs]),
%  io:format("SAs = ~w~n", [SAs_e]),
  case conc_lib:is_bif(M, F, Arity) of
    true ->
      CR = apply(M, F, CAs),
      SR = conc_symb:mock_bif({M, F, Arity}, SAs_e),
      {CR, SR};
    false ->
      case get_module_db(M, CodeServer) of
        preloaded ->
          CR = apply(M, F, CAs),
          SR = conc_symb:mock_bif({M, F, Arity}, SAs_e),
          {CR, SR};
        {ok, MDb} ->
          Key = {M, F, Arity},
          {Def, Exported} = retrieve_function(Key, MDb),
%          io:format("Def=~n~p~n", [Def]),
          check_exported(Exported, CallType, Key),
          Cenv = conc_lib:bind_parameters(CAs, Def#c_fun.vars, conc_lib:new_environment()),
          Senv = conc_lib:bind_parameters(SAs_e, Def#c_fun.vars, conc_lib:new_environment()),
          eval_expr(M, CodeServer, TraceServer, Def#c_fun.body, Cenv, Senv)
      end
  end;
  
%% Handle a Closure
eval({lambda, Closure}, CAs, SAs, _CallType, _CodeServer, _TraceServer) ->
  SAs_e = conc_symb:ensure_list(SAs, length(CAs)),
  ZAs = zip_args(CAs, SAs_e),
  apply(Closure, ZAs);
  
%% Handle a function bound in a letrec expression
eval({letrec_func, {M, _F, Def, E}}, CAs, SAs, _CallType, CodeServer, TraceServer) ->
  {Cenv, Senv} = E(),
  SAs_e = conc_symb:ensure_list(SAs, length(CAs)),
  NCenv = conc_lib:bind_parameters(CAs, Def#c_fun.vars, Cenv),
  NSenv = conc_lib:bind_parameters(SAs_e, Def#c_fun.vars, Senv),
  eval_expr(M, CodeServer, TraceServer, Def#c_fun.body, NCenv, NSenv).
  
  
%%--------------------------------------------------------------------
%% exception(Class, Reason)
%%   Class :: error | exit | throw
%%   Reason :: term()
%% Raises the desired exception.
%%--------------------------------------------------------------------
exception(Class, Reason) ->
  erlang:Class(Reason).

%% ===============
%% eval_expr
%% ===============

%c_apply
eval_expr(M, CodeServer, TraceServer, {c_apply, _Anno, Op, Args}, Cenv, Senv) ->
  %% TODO Constraint: OPsv=OPcv
  {OPcv, _OPsv} = eval_expr(M, CodeServer, TraceServer, Op, Cenv, Senv),
  ZAs = lists:map(
    fun(A) -> %% Will create closures where appropriate
      {CA, SA} = eval_expr(M, CodeServer, TraceServer, A, Cenv, Senv),
      case CA of
        {func, {F, Arity}} -> %% local func (external func is already in make_fun/3 in core erlang)
          Cl = create_closure(M, F, Arity, CodeServer, TraceServer, local),
          {Cl, Cl};
        {letrec_func, {Mod, F, Arity, Def, E}} -> %% letrec func
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
      %% TODO Will make constraint OPsv=OPcv (in case closure is made by make_fun)
      eval({lambda, Closure}, CAs, SAs, local, CodeServer, TraceServer)
  end;
  
%c_binary
eval_expr(M, CodeServer, TraceServer, {c_binary, _Anno, Segments}, Cenv, Senv) ->
  Segms = lists:map(
    fun(S) -> eval_expr(M, CodeServer, TraceServer, S, Cenv, Senv) end,
    Segments),
  {Cs, Ss} = lists:unzip(Segms),
  append_segments(Cs, Ss);
  
%c_bitstr
eval_expr(M, CodeServer, TraceServer, {c_bitstr, _Anno, Val, Size, Unit, Type, Flags}, Cenv, Senv) ->
  {Cv, Sv} = eval_expr(M, CodeServer, TraceServer, Val, Cenv, Senv),
  {CSize, SSize} = eval_expr(M, CodeServer, TraceServer, Size, Cenv, Senv),
  {CUnit, SUnit} = eval_expr(M, CodeServer, TraceServer, Unit, Cenv, Senv),
  {CType, SType} = eval_expr(M, CodeServer, TraceServer, Type, Cenv, Senv),
  {CFlags, SFlags} = eval_expr(M, CodeServer, TraceServer, Flags, Cenv, Senv),
  Cbin = bin_lib:make_bitstring(Cv, CSize, CUnit, CType, CFlags),
  Sbin = conc_symb:make_bitstring(Sv, SSize, SUnit, SType, SFlags),
  {Cbin, Sbin};
  
%c_call
eval_expr(M, CodeServer, TraceServer, {c_call, _Anno, Mod, Name, Args}, Cenv, Senv) ->
  {Mcv, _Msv} = eval_expr(M, CodeServer, TraceServer, Mod, Cenv, Senv),
  {Fcv, _Fsv} = eval_expr(M, CodeServer, TraceServer, Name, Cenv, Senv),
  ZAs = lists:map(
    fun(A) -> %% Will create closures where appropriate
      {CA, SA} = eval_expr(M, CodeServer, TraceServer, A, Cenv, Senv),
      case CA of
        {func, {F, Arity}} -> %% local func (external func is already in make_fun/3 in core erlang)
          Cl = create_closure(M, F, Arity, CodeServer, TraceServer, local),
          {Cl, Cl};
        {letrec_func, {Mod, F, Arity, Def, E}} -> %% letrec func
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
  eval({named, {Mcv, Fcv}}, CAs, SAs, find_call_type(M, Mcv), CodeServer, TraceServer);

%c_case
eval_expr(M, CodeServer, TraceServer, {c_case, _Anno, Arg, Clauses}, Cenv, Senv) ->
  {Cv, Sv} = eval_expr(M, CodeServer, TraceServer, Arg, Cenv, Senv),
  {Body, NCenv, NSenv, _Cnt} = find_clause(M, 'case', CodeServer, TraceServer, Clauses, Cv, Sv, Cenv, Senv),
  eval_expr(M, CodeServer, TraceServer, Body, NCenv, NSenv);

%c_catch
eval_expr(M, CodeServer, TraceServer, {c_catch, _Anno, Body}, Cenv, Senv) ->
  try
    eval_expr(M, CodeServer, TraceServer, Body, Cenv, Senv)
  catch
    throw:Throw ->
      unzip_one(Throw);
    exit:Exit ->
      {Cv, Sv} = unzip_one(Exit),
      {{'EXIT', Cv}, {'EXIT', Sv}};
    error:Error ->
      %% CAUTION! Stacktrace info is not valid
      %% It refers to the interpreter process's stacktrace
      %% Used for internal debugging
      {Cv, Sv} = unzip_one(Error),
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
  NCenv = conc_lib:bind_parameters(CAs, Vars, Cenv),
  NSenv = conc_lib:bind_parameters(SAs, Vars, Senv),
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
eval_expr(M, CodeServer, TraceServer, {c_primop, _Anno, Name, Args}, Cenv, Senv) ->
  Primop = Name#c_literal.val,
  ZAs = lists:map(
    fun(A) -> eval_expr(M, CodeServer, TraceServer, A, Cenv, Senv) end,
    Args),
  {CAs, SAs} = lists:unzip(ZAs),
  %% TODO needs to records more primops
  %% and implement 'bs_context_to_binary', 'bs_init_writable'
  case Primop of
    'raise' ->
      [CClass, CReason] = CAs,
      [_SClass, SReason] = SAs,
      %% TODO
      %% Will create costraint CClass=SClass
      eval({named, {erlang, CClass}}, [CReason], [SReason], external, CodeServer, TraceServer);
    'match_fail' ->
      [Cv]= CAs,
      [Sv] = SAs,
      eval({named, {erlang, error}}, [{badmatch, Cv}], [{badmatch, Sv}], external, CodeServer, TraceServer);
    _ ->
      exception(error, {not_supported_primop, Primop})
  end;

%c_receive
eval_expr(M, CodeServer, TraceServer, {c_receive, _Anno, Clauses, Timeout, Action}, Cenv, Senv) ->
  {CTimeout, STimeout} = eval_expr(M, CodeServer, TraceServer, Timeout, Cenv, Senv),
  true = check_timeout(CTimeout, STimeout),
  Start = erlang:now(),  %% Start timeout timer
  {messages, Mailbox} = erlang:process_info(self(), messages),
  Message = find_message(M, CodeServer, TraceServer, Clauses, Mailbox, Cenv, Senv),
  case Message of
    {Msg, Body, NCenv, NSenv, _Cnt} ->  %% Matched a message already in the mailbox
      receive Msg -> ok end,  %% Just consume the message
      eval_expr(M, CodeServer, TraceServer, Body, NCenv, NSenv);
    false ->  %% No mailbox message matched, thus need to enter a receive loop
      CurrMsgs = length(Mailbox),
      find_message_loop(M, CodeServer, TraceServer, Clauses, Action, CTimeout, STimeout, Cenv, Senv, Start, CurrMsgs)
  end;
  
%c_seq
eval_expr(M, CodeServer, TraceServer, {c_seq, _Anno, Arg, Body}, Cenv, Senv) ->
  _Val = eval_expr(M, CodeServer, TraceServer, Arg, Cenv, Senv),
  eval_expr(M, CodeServer, TraceServer, Body, Cenv, Senv);

%c_try
eval_expr(M, CodeServer, TraceServer, {c_try, _Anno, Arg, Vars, Body, Evars, Handler}, Cenv, Senv) ->
  try
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
    NCenv = conc_lib:bind_parameters(CAs, Vars, Cenv),
    NSenv = conc_lib:bind_parameters(SAs, Vars, Senv),
    eval_expr(M, CodeServer, TraceServer, Body, NCenv, NSenv)
  catch
    Class:Reason ->
      {Cv, Sv} = unzip_one(Reason),
      {Cs, Ss} =
        case length(Evars) of
          3 -> {[Class, Cv, Class], [Class, Sv, Class]};
          2 -> {[Class, Cv], [Class, Sv]}
        end,
      ECenv = conc_lib:bind_parameters(Cs, Evars, Cenv),
      ESenv = conc_lib:bind_parameters(Ss, Evars, Senv),
      eval_expr(M, CodeServer, TraceServer, Handler, ECenv, ESenv)
  end;

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
%% find_message_loop
%% ===============
find_message_loop(M, CodeServer, TraceServer, Clauses, Action, infinity, STimeout, Cenv, Senv, Start, Msgs) ->
  %% TODO Constraint: STimeout=infinity but will have been made by chek_timeout
  run_message_loop(M, CodeServer, TraceServer, Clauses, Action, infinity, STimeout, Cenv, Senv, Start, Msgs);
  
find_message_loop(M, CodeServer, TraceServer, Clauses, Action, CTimeout, STimeout, Cenv, Senv, Start, Msgs) ->
  Now = erlang:now(),
  Passed = timer:now_diff(Now, Start) / 1000,
  case Passed >= CTimeout of
    true ->
      eval_expr(M, CodeServer, TraceServer, Action, Cenv, Senv);
    false ->
    run_message_loop(M, CodeServer, TraceServer, Clauses, Action, CTimeout, STimeout, Cenv, Senv, Start, Msgs)
  end.

%% Helper function run_message_loop/11
run_message_loop(M, CodeServer, TraceServer, Clauses, Action, CTimeout, STimeout, Cenv, Senv, Start, Msgs) ->
  erlang:yield(),
  {message_queue_len, CurrMsgs} = erlang:process_info(self(), message_queue_len),
  %% New messages will appended at the end of the mailbox
  case CurrMsgs > Msgs of
    false -> %% No new messages
      find_message_loop(M, CodeServer, TraceServer, Clauses, Action, CTimeout, STimeout, Cenv, Senv, Start, Msgs);
    true ->
      {messages, Mailbox} = erlang:process_info(self(), messages),
      NewMsgs = lists:nthtail(Msgs, Mailbox),
      Message = find_message(M, CodeServer, TraceServer, Clauses, NewMsgs, Cenv, Senv),
      case Message of
        false ->
          find_message_loop(M, CodeServer, TraceServer, Clauses, Action, CTimeout, STimeout, Cenv, Senv, Start, CurrMsgs);
        {Msg, Body, NCenv, NSenv, _Cnt} ->
%         io:format("r ~w  |  ",[Msg]), 
          receive Msg -> ok end,  %% Just consume the matched message
          eval_expr(M, CodeServer, TraceServer, Body, NCenv, NSenv)
      end
  end.
  
  

%% ===============
%% find_message
%% ===============
find_message(_M, _CodeServer, _TraceServer, _Clauses, [], _Cenv, _Senv) ->
  false;
find_message(M, CodeServer, TraceServer, Clauses, [Msg|Mailbox], Cenv, Senv) ->
%  {Cv, Sv} = {Msg, Msg},
  {Cv, Sv} = decode_msg(Msg),
%    case is_binary(Msg) of
%      true  -> unzip_msg(binary_to_term(Msg));
%      false -> unzip_msg(Msg)
%    end,
  case find_clause(M, 'receive', CodeServer, TraceServer, Clauses, Cv, Sv, Cenv, Senv) of
    false ->
      find_message(M, CodeServer, TraceServer, Clauses, Mailbox, Cenv, Senv);
    {Body, NCenv, NSenv, Cnt} ->
      {Msg, Body, NCenv, NSenv, Cnt}
  end.


%% ===============
%% find_clause
%% ===============
find_clause(M, Mode, CodeServer, TraceServer, Clauses, Cv, Sv, Cenv, Senv) ->
  find_clause(M, Mode, CodeServer, TraceServer, Clauses, Cv, Sv, Cenv, Senv, 1).

find_clause(_M, _Mode, _CodeServer, _TraceServer, [], _Cv, _Sv, _Cenv, _Senv, _Cnt) ->
  false;
find_clause(M, Mode, CodeServer, TraceServer, [Cl|Cls], Cv, Sv, Cenv, Senv, Cnt) ->
  Match = match_clause(M, Mode, CodeServer, TraceServer, Cl, Cv, Sv, Cenv, Senv, Cnt),
  case Match of
    false ->
      find_clause(M, Mode, CodeServer, TraceServer, Cls, Cv, Sv, Cenv, Senv, Cnt+1);
    {true, {Body, NCenv, NSenv, Cnt}} ->
      {Body, NCenv, NSenv, Cnt}
  end.
  
%% ===============
%% match_clause
%% ===============
match_clause(M, Mode, CodeServer, TraceServer, {c_clause, _Anno, Pats, Guard, Body}, Cv, Sv, Cenv, Senv, Cnt) ->
  case is_patlist_compatible(Pats, Cv) of
    false ->
      false;
    true ->
      Degree = length(Pats),
      case Degree of
        1 ->
          Cs = [Cv],
          Ss = [Sv];
        _ ->
          {valuelist, Cs, Degree} = Cv,
          {valuelist, Ss, Degree} = Sv
      end,
      %% BitInfo is needed for parameterized bit-syntax patterns
      BitInfo = {M, CodeServer, Cenv, Senv},
      Match = pattern_match_all(BitInfo, Mode, TraceServer, Pats, Cs, Ss),
      case Match of
        false ->
          false;
        {true, {CMs, SMs}} ->
          NCenv = conc_lib:add_mappings_to_environment(CMs, Cenv),
          NSenv = conc_lib:add_mappings_to_environment(SMs, Senv),
          try eval_expr(M, CodeServer, TraceServer, Guard, NCenv, NSenv) of
            {true, _SGv} ->
              %% TODO make constraint SGv=true
              {true, {Body, NCenv, NSenv, Cnt}};
            {false, _SGv} ->
              %% TODO make constraint SGv=false
              false
          catch
            error:_E -> false
          end
      end
  end.


%% ===============
%% pattern_match_all
%% ===============

pattern_match_all(BitInfo, Mode, TraceServer, Pats, Cvs, Svs) ->
  pattern_match_all(BitInfo, Mode, TraceServer, Pats, Cvs, Svs, [], []).
  
pattern_match_all(_BitInfo, _Mode, _TraceServer, [], [], [], CMaps, SMaps) ->
  {true, {CMaps, SMaps}};
pattern_match_all(BitInfo, Mode, TraceServer, [P|Ps], [Cv|Cvs], [Sv|Svs], CMaps, SMaps) ->
  Match = pattern_match(BitInfo, Mode, TraceServer, P, Cv, Sv, CMaps, SMaps),
  case Match of
    {true, {CMs, SMs}} ->
      pattern_match_all(BitInfo, Mode, TraceServer, Ps, Cvs, Svs, CMs, SMs);
    false ->
      false
  end.

%% ===============
%% pattern_match
%% ===============

%% AtomicLiteral pattern
pattern_match(_BitInfo, _Mode, _TraceServer, {c_literal, _Anno, LitVal}, Cv, _Sv, CMaps, SMaps) ->
  case LitVal =:= Cv of
    true ->
      %% TODO Constraint Sv == Litval
      {true, {CMaps, SMaps}};
    false ->
      %% TODO Constraint Sv != Litval
      false
  end;
  
%% VariableName pattern
pattern_match(_BitInfo, _Mode, _TraceServer, {c_var, _Anno, Name}, Cv, Sv, CMaps, SMaps) ->
  CMs = [{Name, Cv}|CMaps],
  SMs = [{Name, Sv}|SMaps],
  {true, {CMs, SMs}};
  
%% Tuple pattern
pattern_match(BitInfo, Mode, TraceServer, {c_tuple, _Anno, Es}, Cv, Sv, CMaps, SMaps)
  when is_tuple(Cv) ->
    Ne = length(Es),
    Cs = tuple_to_list(Cv),
    case length(Cs) of
      Ne ->
        %% TODO Constraint: Sv tuple with Ne elements
        Ss = conc_symb:tuple_to_list(Sv, Ne),
        pattern_match_all(BitInfo, Mode, TraceServer, Es, Cs, Ss, CMaps, SMaps);
      _ ->
        %% TODO Constraint: Sv not tuple with Ne elements
        false
    end;    
pattern_match(_BitInfo, _Mode, _TraceServer, {c_tuple, _Anno, _Es}, _Cv, _Sv, _CMaps, _SMaps) ->
  %% TODO Constraint: Sv not tuple
  false;
  
%% List constructor pattern
pattern_match(BitInfo, Mode, TraceServer, {c_cons, _Anno, Hd, Tl}, [Cv|Cvs], S, CMaps, SMaps) ->
  %% TODO Constraing: Sv is non empty list
  Sv = conc_symb:hd(S),
  Svs = conc_symb:tl(S),
  case pattern_match(BitInfo, Mode, TraceServer, Hd, Cv, Sv, CMaps, SMaps) of
    {true, {CMs, SMs}} ->
      pattern_match(BitInfo, Mode, TraceServer, Tl, Cvs, Svs, CMs, SMs);
    false ->
      false
  end;  
pattern_match(_BitInfo, _Mode, _TraceServer, {c_cons, _Anno, _Hd, _Tl}, _Cv, _Sv, _CMaps, _SMaps) ->
  %% TODO Constraint: Sv not list
  false;

%% Alias pattern
pattern_match(BitInfo, Mode, TraceServer, {c_alias, _Anno, Var, Pat}, Cv, Sv, CMaps, SMaps) ->
  Match = pattern_match(BitInfo, Mode, TraceServer, Pat, Cv, Sv, CMaps, SMaps),
  case Match of
    {true, {CMs, SMs}} ->
      VarName = Var#c_var.name,
      C = [{VarName, Cv}|CMs],
      S = [{VarName, Sv}|SMs],
      {true, {C, S}};
    false ->
      false
  end;
  
%% Binary pattern
pattern_match(BitInfo, Mode, TraceServer, {c_binary, _Anno, Segments}, Cv, Sv, CMaps, SMaps) ->
  bit_pattern_match(BitInfo, Mode, TraceServer, Segments, Cv, Sv, CMaps, SMaps).


%% ===============
%% bit_pattern_match
%% ===============
bit_pattern_match(_BitInfo, _Mode, _TraceServer, [], <<>>, _Sv, CMaps, SMaps) ->
  %% TODO Constraint: Sv = <<>>
  {true, {CMaps, SMaps}};

bit_pattern_match({M, CodeServer, Cenv, Senv}, Mode, TraceServer, [{c_bitstr, _Anno, Val, Size, Unit, Type, Flags} | Bs], Cv, Sv, CMaps, SMaps) ->
  case Val of
    {c_literal, _AnnoL, LitVal} ->
      {CSize, SSize} = eval_expr(M, CodeServer, TraceServer, Size, Cenv, Senv),
      {CUnit, SUnit} = eval_expr(M, CodeServer, TraceServer, Unit, Cenv, Senv),
      {CType, SType} = eval_expr(M, CodeServer, TraceServer, Type, Cenv, Senv),
      {CFlags, SFlags} = eval_expr(M, CodeServer, TraceServer, Flags, Cenv, Senv),
      try bin_lib:match_bitstring_const(LitVal, CSize, CUnit, CType, CFlags, Cv) of
        CRest ->
          SLit = conc_symb:make_bitstring(LitVal, SSize, SUnit, SType, SFlags),
          %% TODO Constraint: SLit matched Sv
          SRest = conc_symb:match_bitstring_const(SLit, Sv),
          bit_pattern_match({M, CodeServer, Cenv, Senv}, Mode, TraceServer, Bs, CRest, SRest, CMaps, SMaps)
      catch
        error:_E ->
          %% TODO Constraint: <<LitVal:CSize/CType-CFlags-unit:CUnit>> didn't match Sv
          false
      end;
    {c_var, _Anno, VarName} ->
      {CSize, SSize} = eval_expr(M, CodeServer, TraceServer, Size, Cenv, Senv),
      {CUnit, SUnit} = eval_expr(M, CodeServer, TraceServer, Unit, Cenv, Senv),
      {CType, SType} = eval_expr(M, CodeServer, TraceServer, Type, Cenv, Senv),
      {CFlags, SFlags} = eval_expr(M, CodeServer, TraceServer, Flags, Cenv, Senv),
      try bin_lib:match_bitstring_var(CSize, CUnit, CType, CFlags, Cv) of
        {CX, CRest} ->
          SEnc = {SSize, SUnit, SType, SFlags},
          {SX, SRest} =  conc_symb:match_bitstring_var(SEnc, Sv),
          {NewCMaps, NewSMaps} =
            case lists:keysearch(VarName, 1, CMaps) of
              {value, _} -> { lists:keyreplace(VarName, 1, CMaps, {VarName, CX}),
                              lists:keyreplace(VarName, 1, SMaps, {VarName, SX}) };
              false      -> { [{VarName, CX} | CMaps],
                              [{VarName, SX} | SMaps] }
            end,
          NewCenv = conc_lib:add_binding(VarName, CX, Cenv),
          NewSenv = conc_lib:add_binding(VarName, SX, Senv),
          bit_pattern_match({M, CodeServer, NewCenv, NewSenv}, Mode, TraceServer, Bs, CRest, SRest, NewCMaps, NewSMaps)
      catch
        error:_E ->
          %% TODO Constraint: (X is var) <<X:CSize/CType-CFlags-unit:CUnit>> didn't match Sv
          false
      end
  end.


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
make_fun(Mod, Arity, CServer, TServer, Vars, Body, Cenv, Senv) ->
  case Arity of
    0 ->
      fun() ->
        {CodeServer, TraceServer} = validate_servers(CServer, TServer),
        eval_expr(Mod, CodeServer, TraceServer, Body, Cenv, Senv)
      end;
    1 ->
      fun(A) ->
        Args = [A],
        {CAs, SAs} = unzip_args(Args),
        NCenv = conc_lib:bind_parameters(CAs, Vars, Cenv),
        NSenv = conc_lib:bind_parameters(SAs, Vars, Senv),
        {CodeServer, TraceServer} = validate_servers(CServer, TServer),
        eval_expr(Mod, CodeServer, TraceServer, Body, NCenv, NSenv)
      end;
    2 ->
      fun(A, B) ->
        Args = [A, B],
        {CAs, SAs} = unzip_args(Args),
        NCenv = conc_lib:bind_parameters(CAs, Vars, Cenv),
        NSenv = conc_lib:bind_parameters(SAs, Vars, Senv),
        {CodeServer, TraceServer} = validate_servers(CServer, TServer),
        eval_expr(Mod, CodeServer, TraceServer, Body, NCenv, NSenv)
      end;
    3 ->
      fun(A, B, C) ->
        Args = [A, B, C],
        {CAs, SAs} = unzip_args(Args),
        NCenv = conc_lib:bind_parameters(CAs, Vars, Cenv),
        NSenv = conc_lib:bind_parameters(SAs, Vars, Senv),
        {CodeServer, TraceServer} = validate_servers(CServer, TServer),
        eval_expr(Mod, CodeServer, TraceServer, Body, NCenv, NSenv)
      end;
    4 ->
      fun(A, B, C, D) ->
        Args = [A, B, C, D],
        {CAs, SAs} = unzip_args(Args),
        NCenv = conc_lib:bind_parameters(CAs, Vars, Cenv),
        NSenv = conc_lib:bind_parameters(SAs, Vars, Senv),
        {CodeServer, TraceServer} = validate_servers(CServer, TServer),
        eval_expr(Mod, CodeServer, TraceServer, Body, NCenv, NSenv)
      end;
    5 ->
      fun(A, B, C, D, E) ->
        Args = [A, B, C, D, E],
        {CAs, SAs} = unzip_args(Args),
        NCenv = conc_lib:bind_parameters(CAs, Vars, Cenv),
        NSenv = conc_lib:bind_parameters(SAs, Vars, Senv),
        {CodeServer, TraceServer} = validate_servers(CServer, TServer),
        eval_expr(Mod, CodeServer, TraceServer, Body, NCenv, NSenv)
      end;
    6 ->
      fun(A, B, C, D, E, F) ->
        Args = [A, B, C, D, E, F],
        {CAs, SAs} = unzip_args(Args),
        NCenv = conc_lib:bind_parameters(CAs, Vars, Cenv),
        NSenv = conc_lib:bind_parameters(SAs, Vars, Senv),
        {CodeServer, TraceServer} = validate_servers(CServer, TServer),
        eval_expr(Mod, CodeServer, TraceServer, Body, NCenv, NSenv)
      end;
    7 ->
      fun(A, B, C, D, E, F, G) ->
        Args = [A, B, C, D, E, F, G],
        {CAs, SAs} = unzip_args(Args),
        NCenv = conc_lib:bind_parameters(CAs, Vars, Cenv),
        NSenv = conc_lib:bind_parameters(SAs, Vars, Senv),
        {CodeServer, TraceServer} = validate_servers(CServer, TServer),
        eval_expr(Mod, CodeServer, TraceServer, Body, NCenv, NSenv)
      end;
    8 ->
      fun(A, B, C, D, E, F, G, H) ->
        Args = [A, B, C, D, E, F, G, H],
        {CAs, SAs} = unzip_args(Args),
        NCenv = conc_lib:bind_parameters(CAs, Vars, Cenv),
        NSenv = conc_lib:bind_parameters(SAs, Vars, Senv),
        {CodeServer, TraceServer} = validate_servers(CServer, TServer),
        eval_expr(Mod, CodeServer, TraceServer, Body, NCenv, NSenv)
      end;
    9 ->
      fun(A, B, C, D, E, F, G, H, I) ->
        Args = [A, B, C, D, E, F, G, H, I],
        {CAs, SAs} = unzip_args(Args),
        NCenv = conc_lib:bind_parameters(CAs, Vars, Cenv),
        NSenv = conc_lib:bind_parameters(SAs, Vars, Senv),
        {CodeServer, TraceServer} = validate_servers(CServer, TServer),
        eval_expr(Mod, CodeServer, TraceServer, Body, NCenv, NSenv)
      end;
    10 ->
      fun(A, B, C, D, E, F, G, H, I, J) ->
        Args = [A, B, C, D, E, F, G, H, I, J],
        {CAs, SAs} = unzip_args(Args),
        NCenv = conc_lib:bind_parameters(CAs, Vars, Cenv),
        NSenv = conc_lib:bind_parameters(SAs, Vars, Senv),
        {CodeServer, TraceServer} = validate_servers(CServer, TServer),
        eval_expr(Mod, CodeServer, TraceServer, Body, NCenv, NSenv)
      end;
    _ ->
      exception('error', {over_lambda_fun_argument_limit, Arity})
  end.

%% Creates a closure for make_fun when no information on MFA is available
make_fun(Mod, Func, Arity, CServer, TServer) ->
  case Arity of
    0 ->
      fun() ->
        {CodeServer, TraceServer} = validate_servers(CServer, TServer),
        eval({named, {Mod, Func}}, [], [], external, CodeServer, TraceServer)
      end;
    1 ->
      fun(A) ->
        Args = [A],
        {CAs, SAs} = unzip_args(Args),
        {CodeServer, TraceServer} = validate_servers(CServer, TServer),
        eval({named, {Mod, Func}}, CAs, SAs, external, CodeServer, TraceServer)
      end;
    2 ->
      fun(A, B) ->
        Args = [A, B],
        {CAs, SAs} = unzip_args(Args),
        {CodeServer, TraceServer} = validate_servers(CServer, TServer),
        eval({named, {Mod, Func}}, CAs, SAs, external, CodeServer, TraceServer)
      end;
    3 ->
      fun(A, B, C) ->
        Args = [A, B, C],
        {CAs, SAs} = unzip_args(Args),
        {CodeServer, TraceServer} = validate_servers(CServer, TServer),
        eval({named, {Mod, Func}}, CAs, SAs, external, CodeServer, TraceServer)
      end;
    4 ->
      fun(A, B, C, D) ->
        Args = [A, B, C, D],
        {CAs, SAs} = unzip_args(Args),
        {CodeServer, TraceServer} = validate_servers(CServer, TServer),
        eval({named, {Mod, Func}}, CAs, SAs, external, CodeServer, TraceServer)
      end;
    5 ->
      fun(A, B, C, D, E) ->
        Args = [A, B, C, D, E],
        {CAs, SAs} = unzip_args(Args),
        {CodeServer, TraceServer} = validate_servers(CServer, TServer),
        eval({named, {Mod, Func}}, CAs, SAs, external, CodeServer, TraceServer)
      end;
    6 ->
      fun(A, B, C, D, E, F) ->
        Args = [A, B, C, D, E, F],
        {CAs, SAs} = unzip_args(Args),
        {CodeServer, TraceServer} = validate_servers(CServer, TServer),
        eval({named, {Mod, Func}}, CAs, SAs, external, CodeServer, TraceServer)
      end;
    7 ->
      fun(A, B, C, D, E, F, G) ->
        Args = [A, B, C, D, E, F, G],
        {CAs, SAs} = unzip_args(Args),
        {CodeServer, TraceServer} = validate_servers(CServer, TServer),
        eval({named, {Mod, Func}}, CAs, SAs, external, CodeServer, TraceServer)
      end;
    8 ->
      fun(A, B, C, D, E, F, G, H) ->
        Args = [A, B, C, D, E, F, G, H],
        {CAs, SAs} = unzip_args(Args),
        {CodeServer, TraceServer} = validate_servers(CServer, TServer),
        eval({named, {Mod, Func}}, CAs, SAs, external, CodeServer, TraceServer)
      end;
    9 ->
      fun(A, B, C, D, E, F, G, H, I) ->
        Args = [A, B, C, D, E, F, G, H, I],
        {CAs, SAs} = unzip_args(Args),
        {CodeServer, TraceServer} = validate_servers(CServer, TServer),
        eval({named, {Mod, Func}}, CAs, SAs, external, CodeServer, TraceServer)
      end;
    10 ->
      fun(A, B, C, D, E, F, G, H, I, J) ->
        Args = [A, B, C, D, E, F, G, H, I, J],
        {CAs, SAs} = unzip_args(Args),
        {CodeServer, TraceServer} = validate_servers(CServer, TServer),
        eval({named, {Mod, Func}}, CAs, SAs, external, CodeServer, TraceServer)
      end;
    _ ->
      exception('error', {over_lambda_fun_argument_limit, Arity})
  end.

validate_servers(CodeServer, TraceServer) ->
  Node = node(),
  case Node =:= node(TraceServer) of
    true  -> {CodeServer, TraceServer};
    false -> conc_tserver:node_servers(TraceServer, Node)
  end.
  
%% Encode and Decode Msgs
  
%% Encode Msg
encode_msg(TraceServer, Dest, CMsg, SMsg) ->
  case conc_tserver:is_monitored(TraceServer, Dest) of
    true  ->
      Msg = {'_conc', zip_one(CMsg, SMsg)},
      term_to_binary(Msg, [{compressed, 1}]);
    false ->
      CMsg
  end.

%% Decode Msg
decode_msg(Msg) when is_binary(Msg) ->
  try binary_to_term(Msg) of
    {'_conc', ActMsg} -> unzip_msg(ActMsg)
  catch
    error:badarg -> unzip_msg(Msg)
  end;
decode_msg(Msg) ->
  unzip_msg(Msg).
  
  
%% Zip and Unzip concrete-semantic values
%% Zipped values are [{'_zip', CVal, SVal}]

%% zip_one
zip_one(Cv, Sv) ->
  {'_zip', Cv, Sv}.
  
%% unzip_one
unzip_one({'_zip', Cv, Sv}) ->
  {Cv, Sv};
unzip_one(V) ->
  {V, V}.

%% zip_args
zip_args(CAs, SAs) when is_list(CAs), is_list(SAs) ->
  lists:zipwith(fun zip_one/2, CAs, SAs).
  
%% unzip_args
unzip_args(As) when is_list(As) ->
  L = lists:map(fun unzip_one/1, As),
  lists:unzip(L).
  
%% unzip_error // for exception reasons
unzip_error({nocatch, {'_zip', Cv, Sv}}) ->
  {Cv, Sv};
unzip_error(V) ->
  unzip_one(V).

%% unzip_msg
%% Trapping exit and a monitored process died
unzip_msg({'DOWN', MonitorRef, Type, Object, Info}) ->
  {Cv, Sv} = unzip_error(Info),
  CMsg = {'DOWN', MonitorRef, Type, Object, Cv},
  SMsg = {'DOWN', MonitorRef, Type, Object, Sv},
  {CMsg, SMsg};
%% Trapping exit and got an exit signal
unzip_msg({'EXIT', From, {Reason, Stack}}) ->
  {Cv, Sv} = unzip_error(Reason),
  CMsg = {'EXIT', From, {Cv, Stack}},
  SMsg = {'EXIT', From, {Sv, Stack}},
  {CMsg, SMsg};
unzip_msg({'EXIT', From, Reason}) ->
  {Cv, Sv} = unzip_error(Reason),
  CMsg = {'EXIT', From, Cv},
  SMsg = {'EXIT', From, Sv},
  {CMsg, SMsg};
%% Any other message
unzip_msg(V) ->
  unzip_one(V).
  
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
    {ok, _File} = conc_tserver:register_to_trace(TraceServer, Parent, Link),
    Parent ! {self(), registered},
    erlang:apply(?MODULE, eval, Args)
  end.
  
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
      case conc_cserver:load(CodeServer, M) of
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
check_exported(false, external, MFA)  -> exception(error, {not_exported, MFA}).

%% calculated the calltype of an MFA from inside another function
find_call_type(M1, M2) when M1 =:= M2 -> local;
find_call_type(_M1, _M2) -> external.
  
%% Y combinator for a function with arity 0
y(M) ->
  G = fun(F) -> M(fun() -> (F(F))() end) end,
  G(G).
  
%% Calculates if the number of patterns in a clause 
%% is compatible to the numbers of actual values
%% that are trying to be match to
is_patlist_compatible(Pats, Values) ->
  Degree = length(Pats),
  case {Degree, Values} of
    {1, {valuelist, _Vals, _N}} ->
      false;
    {1, _Val} ->
      true;
    {N, {valuelist, _Vals, N}} ->
      true;
    {_N, _Val} ->
      false
  end.
  
%% validate the timeout value of a receive expression
check_timeout(infinity, _Sv) ->
  %% TODO Constraint: Sv=infinity
  true;
check_timeout(Timeout, _Sv) when is_integer(Timeout) -> 
  %% TODO Constraint: Sv is non_neg_int()
  Timeout >= 0;
check_timeout(Timeout, _Sv) ->
  exception(error, {invalid_timeout, Timeout}).
  
%% Concatenate a list of bistrings
append_segments(Cs, Ss) ->
  append_segments(lists:reverse(Cs), <<>>, lists:reverse(Ss), conc_symb:empty_binary()).
  
append_segments([], CAcc, [], SAcc) ->
  {CAcc, SAcc};
append_segments([Cv|Cvs], CAcc, [Sv|Svs], SAcc) ->
  Cbin = <<Cv/bitstring, CAcc/bitstring>>,
  Sbin = conc_symb:append_binary(Sv, SAcc),
  append_segments(Cvs, Cbin, Svs, Sbin).
  

