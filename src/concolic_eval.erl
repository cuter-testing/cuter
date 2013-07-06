%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(concolic_eval).

-export([i/5, eval/7, unzip_error/1]).

-export_type([result/0, valuelist/0]).

-include_lib("compiler/src/core_parse.hrl").

-define(FUNCTION_PREFIX, '__func').
-define(CONCOLIC_PREFIX_MSG, '__concm').
-define(CONCOLIC_PREFIX_PDICT, '__concp').

-type calltype() :: 'local' | 'external'.
-type class()    :: 'error' | 'exit' | 'throw'.
-type eval()     :: {'named', {atom(), atom()}}
                  | {'lambda', function()}
                  | {'letrec_func', {atom(), atom(), cerl:c_fun(), function()}}.
-type exported() :: boolean().
-type result()   :: {term(), term()}.
%% Used to represent list of values for Core Erlang interpretation
-record(valuelist, {values :: [term()], degree :: non_neg_integer()}).
-opaque valuelist() :: #valuelist{}.

%% --------------------------------------------------------
%% Wrapper exported function that spawns an interpreter 
%% process which returns the value of an MFA call 
%% to the Concolic Server
%% --------------------------------------------------------
-spec i(atom(), atom(), [term()], pid(), pid()) -> pid().

i(M, F, As, CodeServer, TraceServer) ->
  Root = self(),
  I = 
    fun() ->
      {SymbAs, Mapping} = concolic_symbolic:abstract(As),
      {ok, Fd} = concolic_tserver:register_to_trace(TraceServer, Root),
      concolic_encdec:log(Fd, 'params', SymbAs),
      concolic:send_mapping(Root, Mapping),
      NMF = {named, {M, F}},
      Val = eval(NMF, As, SymbAs, external, CodeServer, TraceServer, Fd),
      concolic:send_return(Root, Val)
    end,
  erlang:spawn(I).

%% --------------------------------------------------------
%% eval
%%
%% Concrete/Symbolic Evaluation and Logging of an MFA call
%% --------------------------------------------------------
-spec eval(eval(), [term()], [term()], calltype(), pid(), pid(), file:io_device()) -> result().

%% Handle spawns so that the spawned process
%% will be interpreted

%% Handle spawn/1, spawn/2, spawn/3, spawn/4,
%% spawn_link/1 spawn_link/2, spawn_link/3, spawn_link/4
eval({named, {erlang, F}}, CAs, SAs, _CallType, CodeServer, TraceServer, Fd)
  when F =:= spawn; F =:= spawn_link ->
    Arity = length(CAs),
    SAs_e = concolic_symbolic:ensure_list(SAs, Arity, CAs, Fd),
    ChildPid =
      case CAs of
        [Fun] ->
          [_SFun] = SAs_e,
          %% TODO Constraint: SFun=Fun
          EvalArgs = [{lambda, Fun}, [], [], local, CodeServer, TraceServer],
          Child = register_and_apply(TraceServer, self(), EvalArgs),
          erlang:F(Child);
        [Node, Fun] ->
          [_SNode, _SFun] = SAs_e,
          %% TODO Constraints: SNode=Node, SFun=Fun
          {CServer, TServer} = concolic_tserver:node_servers(TraceServer, Node),
          EvalArgs = [{lambda, Fun}, [], [], local, CServer, TServer],
          Child = register_and_apply(TServer, self(), EvalArgs),
          erlang:F(Node, Child);
        [Mod, Fun, Args] ->
          [_SMod, _SFun, SArgs] = SAs_e,
          %% TODO Constraints: SMod = Mod, SFun=Fun
          Call = find_call_type(erlang, Mod),
          EvalArgs = [{named, {Mod, Fun}}, Args, SArgs, Call, CodeServer, TraceServer],
          Child = register_and_apply(TraceServer, self(), EvalArgs),
          erlang:F(Child);
        [Node, Mod, Fun, Args] ->
          [_SNode, _SMod, _SFun, SArgs] = SAs_e,
          %% TODO Constraints: SNode=Node, SMod = Mod, SFun=Fun
          {CServer, TServer} = concolic_tserver:node_servers(TraceServer, Node),
          Call = find_call_type(erlang, Mod),
          EvalArgs = [{named, {Mod, Fun}}, Args, SArgs, Call, CServer, TServer],
          Child = register_and_apply(TServer, self(), EvalArgs),
          erlang:F(Node, Child);
        _ ->
          exception('error', {'undef', {erlang, spawn, Arity}})
      end,
    receive
      {ChildPid, registered} -> {ChildPid, ChildPid}
    end;

%% Handle spawn_monitor/1, spawn_monitor/3
eval({named, {erlang, spawn_monitor}}, CAs, SAs, _CallType, CodeServer, TraceServer, Fd) ->
  Arity = length(CAs),
  SAs_e = concolic_symbolic:ensure_list(SAs, Arity, CAs, Fd),
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
        exception('error', {'undef', {erlang, spawn_monitor, Arity}})
    end,
  Child = register_and_apply(TraceServer, self(), EvalArgs),
  {ChildPid, _ChildRef} = CC = erlang:spawn_monitor(Child),
  receive
    {ChildPid, registered} -> {CC, CC}
  end;

%% Handle spawn_opt/2, spawn_opt/3, spawn_opt/4, spawn_opt/5
eval({named, {erlang, spawn_opt}}, CAs, SAs, _CallType, CodeServer, TraceServer, Fd) ->
  Arity = length(CAs),
  SAs_e = concolic_symbolic:ensure_list(SAs, Arity, CAs, Fd),
  R =
    case CAs of
      [Fun, Opts] ->
        [_SFun, _SOpts] = SAs_e,
        %% TODO Constraints: SFun=Fun, SOpts=Opts
        EvalArgs = [{lambda, Fun}, [], [], local, CodeServer, TraceServer],
        Child = register_and_apply(TraceServer, self(), EvalArgs),
        erlang:spawn_opt(Child, Opts);
      [Node, Fun, Opts] ->
        [_SNode, _SFun, _SOpts] = SAs_e,
        %% TODO Constraints: SNode=Node, SFun=Fun, SOpts=Opts
        {CServer, TServer} = concolic_tserver:node_servers(TraceServer, Node),
        EvalArgs = [{lambda, Fun}, [], [], local, CServer, TServer],
        Child = register_and_apply(TServer, self(), EvalArgs),
        erlang:spawn_opt(Node, Child, Opts);
      [Mod, Fun, Args, Opts] ->
        [_SMod, _SFun, SArgs, _SOpts] = SAs_e,
        %% TODO Constraints: SMod=Mode, SFun=Fun, SOpts=Opts
        Call = find_call_type(erlang, Mod),
        EvalArgs = [{named, {Mod, Fun}}, Args, SArgs, Call, CodeServer, TraceServer],
        Child = register_and_apply(TraceServer, self(), EvalArgs),
        erlang:spawn_opt(Child, Opts);
      [Node, Mod, Fun, Args, Opts] ->
        [_SNode, _SMod, _SFun, SArgs, _SOpts] = SAs_e,
        %% TODO Constraints: SNode=Node, SMod=Mode, SFun=Fun, SOpts=Opts
        Call = find_call_type(erlang, Mod),
        {CServer, TServer} = concolic_tserver:node_servers(TraceServer, Node),
        EvalArgs = [{named, {Mod, Fun}}, Args, SArgs, Call, CServer, TServer],
        Child = register_and_apply(TServer, self(), EvalArgs),
        erlang:spawn_opt(Node, Child, Opts);
      _ ->
        exception('error', {'undef', {erlang, spawn_opt, Arity}})
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
eval({named, {erlang, '!'}}, [_, _] = CAs, SAs, CallType, CodeServer, TraceServer, Fd) ->
  eval({named, {erlang, send}}, CAs, SAs, CallType, CodeServer, TraceServer, Fd);

%% Handle send/2, send/3
eval({named, {erlang, send}}, CAs, SAs, _CallType, _CodeServer, TraceServer, Fd) ->
  Arity = length(CAs),
  SAs_e = concolic_symbolic:ensure_list(SAs, Arity, CAs, Fd),
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
      exception('error', {'undef', {erlang, send, Arity}})
  end;

%% Handle send_after/3
eval({named, {erlang, send_after}}, CAs, SAs, _CallType, _CodeServer, TraceServer, Fd) ->
  Arity = length(CAs),
  SAs_e = concolic_symbolic:ensure_list(SAs, Arity, CAs, Fd),
  case CAs of
    [CTime, CDest, CMsg] ->
      [_STime, _SDest, SMsg] = SAs_e,
      %% TODO Constraint CTime=STime, CDest=SDest
      Msg = encode_msg(TraceServer, CDest, CMsg, SMsg),
      R = erlang:send_after(CTime, CDest, Msg),
      {R, R};
    _ ->
      exception('error', {'undef', {erlang, send_after, Arity}})
  end;
 
%% Handle send_nosuspend/2, send_nosuspend/3
eval({named, {erlang, send_nosuspend}}, CAs, SAs, _CallType, _CodeServer, TraceServer, Fd) ->
  Arity = length(CAs),
  SAs_e = concolic_symbolic:ensure_list(SAs, Arity, CAs, Fd),
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
      exception('error', {'undef', {erlang, send_nosuspend, Arity}})
  end;

%% Handle functions that raise exceptions
%% so as to zip the concrete and symbolic reason

%% Handle throw/1
eval({named, {erlang, throw}}, CAs, SAs, _CallType, _CodeServer, _TraceServer, Fd) ->
  Arity = length(CAs),
  SAs_e = concolic_symbolic:ensure_list(SAs, Arity, CAs, Fd),
  case CAs of
    [CThrow] ->
      [SThrow] = SAs_e,
      Throw = zip_one(CThrow, SThrow),
      erlang:throw(Throw);
    _ ->
      exception('error', {'undef', {erlang, throw, Arity}})
  end;
  
%% Handle exit/1, exit2
eval({named, {erlang, exit}}, CAs, SAs, _CallType, _CodeServer, _TraceServer, Fd) ->
  Arity = length(CAs),
  SAs_e = concolic_symbolic:ensure_list(SAs, Arity, CAs, Fd),
  case CAs of
    [CExit] ->
      [SExit] = SAs_e,
      Exit = 
        case CExit of
          %% TODO Constraint: SExit=normal
          normal -> normal;
          _ -> zip_one(CExit, SExit)
        end,
      erlang:exit(Exit);
    [CDest, CExit] ->
      [_SDest, SExit] = SAs_e,
      %% TODO Constraint CDest=SDest
      Exit = 
        case CExit of
          %% TODO Constraint: SExit=normal
          normal -> normal;
          %% TODO Constraint: SExit=kill
          kill -> kill;
          _ -> zip_one(CExit, SExit)
        end,
        R = erlang:exit(CDest, Exit),
        {R, R};
    _ ->
      exception('error', {'undef', {erlang, exit, Arity}})
  end;
    
%% Handle error/1, error/2
eval({named, {erlang, error}}, CAs, SAs, _CallType, _CodeServer, _TraceServer, Fd) ->
  Arity = length(CAs),
  SAs_e = concolic_symbolic:ensure_list(SAs, Arity, CAs, Fd),
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
      exception('error', {'undef', {erlang, error, Arity}})
  end;
  
%% Handle raise/3
eval({named, {erlang, raise}}, CAs, SAs, _CallType, _CodeServer, _TraceServer, Fd) ->
  Arity = length(CAs),
  SAs_e = concolic_symbolic:ensure_list(SAs, Arity, CAs, Fd),
  case CAs of
    [CClass, CReason, CStacktrace] ->
      [_SClass, SReason, _] = SAs_e,
      %% TODO Create constraint Class=SClass
      R = zip_one(CReason, SReason),
      erlang:raise(CClass, R, CStacktrace);
    _ ->
      exception('error', {'undef', {erlang, raise, Arity}})
  end;

%% Handle other important functions

%% Handle make_fun/3  
eval({named, {erlang, make_fun}}, CAs, SAs, _CallType, CodeServer, TraceServer, Fd) ->
  Arity = length(CAs),
  SAs_e = concolic_symbolic:ensure_list(SAs, Arity, CAs, Fd),
  case CAs of
    [M, F, A] ->
      CR = make_fun(M, F, A, CodeServer, TraceServer, Fd),
      SR = concolic_symbolic:mock_bif({erlang, make_fun, 3}, {CAs, SAs_e}, CR, Fd),
      {CR, SR};
    _ ->
      exception('error', {'undef', {erlang, make_fun, Arity}})
  end;
  
%% Handle apply/2, apply/3
eval({named, {erlang, apply}}, CAs, SAs, _CallType, CodeServer, TraceServer, Fd) ->
  Arity = length(CAs),
  SAs_e = concolic_symbolic:ensure_list(SAs, Arity, CAs, Fd),
  case CAs of
    [Fun, Args] ->
      [_SFun, SArgs] = SAs_e,
      %% TODO Constraint: Fun=SFun
      eval({lambda, Fun}, Args, SArgs, local, CodeServer, TraceServer, Fd);
    [M, F, Args] ->
      [_SMod, _SFun, SArgs] = SAs_e,
      %% TODO Constraints: SMod = M, SFun=F
      Call = find_call_type(erlang, M),
      eval({named, {M, F}}, Args, SArgs, Call, CodeServer, TraceServer, Fd);
    _ ->
      exception('error', {'undef', {erlang, apply, Arity}})
  end;

%% Handle an MFA
eval({named, {M, F}}, CAs_b, SAs_b, CallType, CodeServer, TraceServer, Fd) ->
  {CAs, SAs} = adjust_arguments(M, F, CAs_b, SAs_b, Fd),
  Arity = length(CAs),
  SAs_e = concolic_symbolic:ensure_list(SAs, Arity, CAs, Fd),
  MFA = {M, F, Arity},
  case concolic_lib:is_bif(MFA) of  %% Check if MFA is a BIF
    true ->
      evaluate_bif(MFA, CAs, SAs_e, Fd);
    false ->
      case get_module_db(M, CodeServer) of
        preloaded ->
          evaluate_bif(MFA, CAs, SAs_e, Fd);
        {ok, MDb} ->
          {Def, Exported} = retrieve_function(MFA, MDb),  %% Get the MFA Code
          %%  io:format("Def=~n~p~n", [Def]),
          check_exported(Exported, CallType, MFA),
          NCenv = concolic_lib:new_environment(),
          NSenv = concolic_lib:new_environment(),
          Cenv = concolic_lib:bind_parameters(CAs, Def#c_fun.vars, NCenv),
          Senv = concolic_lib:bind_parameters(SAs_e, Def#c_fun.vars, NSenv),
          eval_expr(M, CodeServer, TraceServer, Def#c_fun.body, Cenv, Senv, Fd)
      end
  end;
  
%% Handle a Closure
eval({lambda, Closure}, CAs, SAs, _CallType, _CodeServer, _TraceServer, Fd) ->
  SAs_e = concolic_symbolic:ensure_list(SAs, length(CAs), CAs, Fd),
  ZAs = zip_args(CAs, SAs_e),
  apply(Closure, ZAs);
  
%% Handle a function bound in a letrec expression
eval({letrec_func, {M, _F, Def, E}}, CAs, SAs, _CallType, CodeServer, TraceServer, Fd) ->
  {Cenv, Senv} = E(),
  SAs_e = concolic_symbolic:ensure_list(SAs, length(CAs), CAs, Fd),
  NCenv = concolic_lib:bind_parameters(CAs, Def#c_fun.vars, Cenv),
  NSenv = concolic_lib:bind_parameters(SAs_e, Def#c_fun.vars, Senv),
  eval_expr(M, CodeServer, TraceServer, Def#c_fun.body, NCenv, NSenv, Fd).
  
%% --------------------------------------------------------
%% @doc Raises the desired exception.
%% --------------------------------------------------------
-spec exception(class(), term()) -> no_return().

exception(Class, Reason) ->
  erlang:Class(Reason).

%% --------------------------------------------------------
%% eval_expr
%%
%% Evaluates a Core Erlang expression
%% --------------------------------------------------------
-spec eval_expr(atom(), pid(), pid(), cerl:cerl(), concolic_lib:environment(), concolic_lib:environment(), file:io_device()) ->
  {concolic_lib:semantic_value(), concolic_lib:semantic_value()}.

%% c_apply
eval_expr(M, CodeServer, TraceServer, {c_apply, _Anno, Op, Args}, Cenv, Senv, Fd) ->
  {OPcv, _OPsv} = eval_expr(M, CodeServer, TraceServer, Op, Cenv, Senv, Fd),
  Fun = 
    fun(A) -> %% Will create closures where appropriate
      {CA, SA} = eval_expr(M, CodeServer, TraceServer, A, Cenv, Senv, Fd),
      case CA of
        {?FUNCTION_PREFIX, {F, Arity}} -> %% local func (external func is already in make_fun/3 in core erlang)
          Cl = create_closure(M, F, Arity, CodeServer, TraceServer, local, Fd),
          {Cl, Cl};
        {letrec_func, {Mod, F, Arity, Def, E}} -> %% letrec func
          {CE, SE} = E(),
          Cl = create_closure(Mod, F, Arity, CodeServer, TraceServer, {letrec_fun, {Def, CE, SE}}, Fd),
          {Cl, Cl};
        _ ->
          {CA, SA}
      end
    end,
  ZAs = [Fun(A) || A <- Args],
  {CAs, SAs} = lists:unzip(ZAs),
  case OPcv of %% Check eval_expr(..., #c_var{}, ...) output for reference
    {?FUNCTION_PREFIX, {Func, _Arity}} ->
      eval({named, {M, Func}}, CAs, SAs, local, CodeServer, TraceServer, Fd);
    {letrec_func, {Mod, Func, _Arity, Def, E}} ->
      eval({letrec_func, {Mod, Func, Def, E}}, CAs, SAs, local, CodeServer, TraceServer, Fd);
    Closure ->
      %% TODO Will make constraint OPsv=OPcv (in case closure is made by make_fun)
      eval({lambda, Closure}, CAs, SAs, local, CodeServer, TraceServer, Fd)
  end;
  
%% c_binary
eval_expr(M, CodeServer, TraceServer, {c_binary, _Anno, Segments}, Cenv, Senv, Fd) ->
  Segms = [eval_expr(M, CodeServer, TraceServer, S, Cenv, Senv, Fd) || S <- Segments],
  {Cs, Ss} = lists:unzip(Segms),
  append_segments(Cs, Ss, Fd);
  
%% c_bitstr
eval_expr(M, CodeServer, TraceServer, {c_bitstr, _Anno, Val, Size, Unit, Type, Flags}, Cenv, Senv, Fd) ->
  {Cv, Sv} = eval_expr(M, CodeServer, TraceServer, Val, Cenv, Senv, Fd),
  {CSize, SSize} = eval_expr(M, CodeServer, TraceServer, Size, Cenv, Senv, Fd),
  {CUnit, SUnit} = eval_expr(M, CodeServer, TraceServer, Unit, Cenv, Senv, Fd),
  {CType, SType} = eval_expr(M, CodeServer, TraceServer, Type, Cenv, Senv, Fd),
  {CFlags, SFlags} = eval_expr(M, CodeServer, TraceServer, Flags, Cenv, Senv, Fd),
  Cbin = bin_lib:make_bitstring(Cv, CSize, CUnit, CType, CFlags),
  Sbin = concolic_symbolic:make_bitstring(Sv, {SSize, SUnit, SType, SFlags}, {'some', Cbin}, Fd),
  {Cbin, Sbin};
  
%% c_call
eval_expr(M, CodeServer, TraceServer, {c_call, _Anno, Mod, Name, Args}, Cenv, Senv, Fd) ->
  {Mcv, _Msv} = eval_expr(M, CodeServer, TraceServer, Mod, Cenv, Senv, Fd),
  {Fcv, _Fsv} = eval_expr(M, CodeServer, TraceServer, Name, Cenv, Senv, Fd),
  Fun = 
    fun(A) -> %% Will create closures where appropriate
      {CA, SA} = eval_expr(M, CodeServer, TraceServer, A, Cenv, Senv, Fd),
      case CA of
        {?FUNCTION_PREFIX, {F, Arity}} -> %% local func (external func is already in make_fun/3 in core erlang)
          Cl = create_closure(M, F, Arity, CodeServer, TraceServer, local, Fd),
          {Cl, Cl};
        {letrec_func, {Mod, F, Arity, Def, E}} -> %% letrec func
          {CE, SE} = E(),
          Cl = create_closure(Mod, F, Arity, CodeServer, TraceServer, {letrec_fun, {Def, CE, SE}}, Fd),
          {Cl, Cl};
        _ ->
          {CA, SA}
      end
    end,
  ZAs = [Fun(A) || A <- Args],
  {CAs, SAs} = lists:unzip(ZAs),
  %% TODO Will make constraints Mcv=Msv and Fcv=Fsv
  eval({named, {Mcv, Fcv}}, CAs, SAs, find_call_type(M, Mcv), CodeServer, TraceServer, Fd);

%% c_case
eval_expr(M, CodeServer, TraceServer, {c_case, _Anno, Arg, Clauses}, Cenv, Senv, Fd) ->
  {Cv, Sv} = eval_expr(M, CodeServer, TraceServer, Arg, Cenv, Senv, Fd),
  {Body, NCenv, NSenv, _Cnt} = find_clause(M, 'case', CodeServer, TraceServer, Clauses, Cv, Sv, Cenv, Senv, Fd),
  eval_expr(M, CodeServer, TraceServer, Body, NCenv, NSenv, Fd);

%% c_catch
eval_expr(M, CodeServer, TraceServer, {c_catch, _Anno, Body}, Cenv, Senv, Fd) ->
  try
    eval_expr(M, CodeServer, TraceServer, Body, Cenv, Senv, Fd)
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

%% c_cons
eval_expr(M, CodeServer, TraceServer, {c_cons, _Anno, Hd, Tl}, Cenv, Senv, Fd) ->
  {Hdcv, Hdsv} = eval_expr(M, CodeServer, TraceServer, Hd, Cenv, Senv, Fd),
  {Tlcv, Tlsv} = eval_expr(M, CodeServer, TraceServer, Tl, Cenv, Senv, Fd),
  {[Hdcv|Tlcv], [Hdsv|Tlsv]};

%% c_fun
eval_expr(M, CodeServer, TraceServer, {c_fun, _Anno, Vars, Body}, Cenv, Senv, Fd) ->
  Arity = length(Vars),
  Lambda = make_fun(M, Arity, CodeServer, TraceServer, Vars, Body, Cenv, Senv, Fd),
  {Lambda, Lambda};

%% c_let
eval_expr(M, CodeServer, TraceServer, {c_let, _Anno, Vars, Arg, Body}, Cenv, Senv, Fd) ->
  Degree = length(Vars),
  {C, S} = eval_expr(M, CodeServer, TraceServer, Arg, Cenv, Senv, Fd),
  case Degree of
    1 ->
      CAs = [C],
      SAs = [S];
    _ ->
      {valuelist, CAs, Degree} = C,
      {valuelist, SAs, Degree} = S
  end,
  NCenv = concolic_lib:bind_parameters(CAs, Vars, Cenv),
  NSenv = concolic_lib:bind_parameters(SAs, Vars, Senv),
  eval_expr(M, CodeServer, TraceServer, Body, NCenv, NSenv, Fd);

%% c_letrec
eval_expr(M, CodeServer, TraceServer, {c_letrec, _Anno, Defs, Body}, Cenv, Senv, Fd) ->
  H = fun(F) -> fun() ->
    lists:foldl(
      fun({Func, Def}, {Ce, Se}) ->
        LetRec = {letrec_func, {M, Def, F}},
        NCe = concolic_lib:add_binding(Func#c_var.name, LetRec, Ce),
        NSe = concolic_lib:add_binding(Func#c_var.name, LetRec, Se),
        {NCe, NSe}
      end,
      {Cenv, Senv}, Defs
    )
  end end,
  %% NewEnv is now a /0 function
  %% NewEnv() will create the necessary self-referenced environment
  {NCenv, NSenv} = (y(H))(),
  eval_expr(M, CodeServer, TraceServer, Body, NCenv, NSenv, Fd);

%% c_literal
eval_expr(_M, _CodeServer, _TraceServer, {c_literal, _Anno, Val}, _Cenv, _Senv, _Fd) ->
  {Val, Val};

%% c_primop
eval_expr(M, CodeServer, TraceServer, {c_primop, _Anno, Name, Args}, Cenv, Senv, Fd) ->
  Primop = Name#c_literal.val,
  ZAs = [eval_expr(M, CodeServer, TraceServer, A, Cenv, Senv, Fd) || A <- Args],
  {CAs, SAs} = lists:unzip(ZAs),
  %% TODO needs to records more primops
  %% and implement 'bs_context_to_binary', 'bs_init_writable'
  case Primop of
    'raise' ->
      [CClass, CReason] = CAs,
      [_SClass, SReason] = SAs,
      %% TODO Will create costraint CClass=SClass
      eval({named, {erlang, CClass}}, [CReason], [SReason], external, CodeServer, TraceServer, Fd);
    'match_fail' ->
      [Cv]= CAs,
      [Sv] = SAs,
      eval({named, {erlang, error}}, [{badmatch, Cv}], [{badmatch, Sv}], external, CodeServer, TraceServer, Fd);
    _ ->
      exception('error', {'not_supported_primop', Primop})
  end;

%% c_receive
eval_expr(M, CodeServer, TraceServer, {c_receive, _Anno, Clauses, Timeout, Action}, Cenv, Senv, Fd) ->
  {CTimeout, STimeout} = eval_expr(M, CodeServer, TraceServer, Timeout, Cenv, Senv, Fd),
  true = check_timeout(CTimeout, STimeout, Fd),
  Start = erlang:now(),  %% Start timeout timer
  {messages, Mailbox} = erlang:process_info(self(), messages),
  Message = find_message(M, CodeServer, TraceServer, Clauses, Mailbox, Cenv, Senv, Fd),
  case Message of
    {Msg, Body, NCenv, NSenv, _Cnt} ->  %% Matched a message already in the mailbox
      receive Msg -> ok end,  %% Just consume the message
      eval_expr(M, CodeServer, TraceServer, Body, NCenv, NSenv, Fd);
    false ->  %% No mailbox message matched, thus need to enter a receive loop
      CurrMsgs = length(Mailbox),
      find_message_loop(M, CodeServer, TraceServer, Clauses, Action, CTimeout, STimeout, Cenv, Senv, Start, CurrMsgs, Fd)
  end;
  
%% c_seq
eval_expr(M, CodeServer, TraceServer, {c_seq, _Anno, Arg, Body}, Cenv, Senv, Fd) ->
  _Val = eval_expr(M, CodeServer, TraceServer, Arg, Cenv, Senv, Fd),
  eval_expr(M, CodeServer, TraceServer, Body, Cenv, Senv, Fd);

%% c_try
eval_expr(M, CodeServer, TraceServer, {c_try, _Anno, Arg, Vars, Body, Evars, Handler}, Cenv, Senv, Fd) ->
  try
    Degree = length(Vars),
    {C, S} = eval_expr(M, CodeServer, TraceServer, Arg, Cenv, Senv, Fd),
    case Degree of
      1 ->
        CAs = [C],
        SAs = [S];
      _ ->
        {valuelist, CAs, Degree} = C,
        {valuelist, SAs, Degree} = S
    end,
    NCenv = concolic_lib:bind_parameters(CAs, Vars, Cenv),
    NSenv = concolic_lib:bind_parameters(SAs, Vars, Senv),
    eval_expr(M, CodeServer, TraceServer, Body, NCenv, NSenv, Fd)
  catch
    Class:Reason ->
      {Cv, Sv} = unzip_one(Reason),
      {Cs, Ss} =
        case length(Evars) of
          3 -> {[Class, Cv, Class], [Class, Sv, Class]};
          2 -> {[Class, Cv], [Class, Sv]}
        end,
      ECenv = concolic_lib:bind_parameters(Cs, Evars, Cenv),
      ESenv = concolic_lib:bind_parameters(Ss, Evars, Senv),
      eval_expr(M, CodeServer, TraceServer, Handler, ECenv, ESenv, Fd)
  end;

%% c_tuple
eval_expr(M, CodeServer, TraceServer, {c_tuple, _Anno, Es}, Cenv, Senv, Fd) ->
  ZEs = [eval_expr(M, CodeServer, TraceServer, E, Cenv, Senv, Fd) || E <- Es],
  {CEs, SEs} = lists:unzip(ZEs),
  {list_to_tuple(CEs), list_to_tuple(SEs)};

%% c_values
eval_expr(M, CodeServer, TraceServer, {c_values, _Anno, Es}, Cenv, Senv, Fd) ->
  Degree = length(Es),
  ZEs = [eval_expr(M, CodeServer, TraceServer, E, Cenv, Senv, Fd) || E <- Es],
  {CEs, SEs} = lists:unzip(ZEs),
  {#valuelist{values=CEs, degree=Degree}, #valuelist{values=SEs, degree=Degree}};

%% c_var
eval_expr(_M, _CodeServer, _TraceServer, {c_var, _Anno, Name}, Cenv, Senv, _Fd)
  when is_tuple(Name) ->
    %% If Name is a function
    case concolic_lib:get_value(Name, Cenv) of
      {ok, Closure} when is_function(Closure) -> %% Closure
        {ok, Sv} = concolic_lib:get_value(Name, Senv),
        {Closure, Sv};
      {ok, {letrec_func, {Mod, Def, E}}} ->  %% Fun bound in a letrec
        {Fun, Arity} = Name,
        R = {letrec_func, {Mod, Fun, Arity, Def, E}},
        {R, R};
      error -> %% either local in module or external
        {_Fun, _Arity} = Name,
        R = {?FUNCTION_PREFIX, Name},
        {R, R}
    end;
eval_expr(_M, _CodeServer, _TraceServer, {c_var, _Anno, Name}, Cenv, Senv, _Fd) ->
  %% If it's a variable then return its value
  {ok, Cval} = concolic_lib:get_value(Name, Cenv),
  {ok, Sval} = concolic_lib:get_value(Name, Senv),
  {Cval, Sval}.


%% --------------------------------------------------------
%% find_message_loop
%%
%% Enters a loop waiting for a message that will match.
%% Wraps calls to run_message_loop to check for timeout.
%% --------------------------------------------------------
find_message_loop(M, CodeServer, TraceServer, Clauses, Action, infinity, STimeout, Cenv, Senv, Start, Msgs, Fd) ->
  %% TODO Constraint: STimeout=infinity but will have been made by check_timeout
  run_message_loop(M, CodeServer, TraceServer, Clauses, Action, infinity, STimeout, Cenv, Senv, Start, Msgs, Fd);
find_message_loop(M, CodeServer, TraceServer, Clauses, Action, CTimeout, STimeout, Cenv, Senv, Start, Msgs, Fd) ->
  Now = erlang:now(),
  Passed = timer:now_diff(Now, Start) / 1000,
  case Passed >= CTimeout of
    true ->
      eval_expr(M, CodeServer, TraceServer, Action, Cenv, Senv, Fd);
    false ->
      run_message_loop(M, CodeServer, TraceServer, Clauses, Action, CTimeout, STimeout, Cenv, Senv, Start, Msgs, Fd)
  end.

%% --------------------------------------------------------
%% run_message_looop
%%
%% Implements the actual waiting receive loop
%% --------------------------------------------------------
run_message_loop(M, CodeServer, TraceServer, Clauses, Action, CTimeout, STimeout, Cenv, Senv, Start, Msgs, Fd) ->
  erlang:yield(),
  {message_queue_len, CurrMsgs} = erlang:process_info(self(), message_queue_len),
  %% New messages will appended at the end of the mailbox
  case CurrMsgs > Msgs of
    false -> %% No new messages
      find_message_loop(M, CodeServer, TraceServer, Clauses, Action, CTimeout, STimeout, Cenv, Senv, Start, Msgs, Fd);
    true ->
      {messages, Mailbox} = erlang:process_info(self(), messages),
      NewMsgs = lists:nthtail(Msgs, Mailbox),
      Message = find_message(M, CodeServer, TraceServer, Clauses, NewMsgs, Cenv, Senv, Fd),
      case Message of
        false ->
          find_message_loop(M, CodeServer, TraceServer, Clauses, Action, CTimeout, STimeout, Cenv, Senv, Start, CurrMsgs, Fd);
        {Msg, Body, NCenv, NSenv, _Cnt} ->
          receive Msg -> ok end,  %% Just consume the matched message
          eval_expr(M, CodeServer, TraceServer, Body, NCenv, NSenv, Fd)
      end
  end.
  
%% --------------------------------------------------------
%% find_message
%%
%% Wraps calls to find_clause when trying to match 
%% a message against a series of patterns
%% --------------------------------------------------------
find_message(_M, _CodeServer, _TraceServer, _Clauses, [], _Cenv, _Senv, _Fd) ->
  false;
find_message(M, CodeServer, TraceServer, Clauses, [Msg|Mailbox], Cenv, Senv, Fd) ->
  {Cv, Sv} = decode_msg(Msg),
  case find_clause(M, 'receive', CodeServer, TraceServer, Clauses, Cv, Sv, Cenv, Senv, Fd) of
    false ->
      find_message(M, CodeServer, TraceServer, Clauses, Mailbox, Cenv, Senv, Fd);
    {Body, NCenv, NSenv, Cnt} ->
      %% I can log the received Msg here
      {Msg, Body, NCenv, NSenv, Cnt}
  end.

%% --------------------------------------------------------
%% find_clause
%%
%% Finds the clause that matches a pair of concrete &
%% symbolic values.
%% Can return false only when used to match a message.
%% Otherwise always finds a matching clause since the 
%% compiler adds a catch all clause at the end of every
%% case statement.
%% --------------------------------------------------------
find_clause(M, Mode, CodeServer, TraceServer, Clauses, Cv, Sv, Cenv, Senv, Fd) ->
  find_clause(M, Mode, CodeServer, TraceServer, Clauses, Cv, Sv, Cenv, Senv, Fd, 1).

find_clause(_M, _Mode, _CodeServer, _TraceServer, [], _Cv, _Sv, _Cenv, _Senv, _Fd, _Cnt) ->
  false;
find_clause(M, Mode, CodeServer, TraceServer, [Cl|Cls], Cv, Sv, Cenv, Senv, Fd, Cnt) ->
  Match = match_clause(M, Mode, CodeServer, TraceServer, Cl, Cv, Sv, Cenv, Senv, Fd, Cnt),
  case Match of
    false ->
      find_clause(M, Mode, CodeServer, TraceServer, Cls, Cv, Sv, Cenv, Senv, Fd, Cnt+1);
    {true, {_Body, _NCenv, _NSenv, Cnt} = MatchClause} ->
      MatchClause
  end.

%% --------------------------------------------------------
%% match_clause
%%
%% Match a pair of concrete & symbolic values against
%% a specific clause (i.e. with patterns and guard)
%% --------------------------------------------------------
match_clause(M, Mode, CodeServer, TraceServer, {c_clause, _Anno, Pats, Guard, Body}, Cv, Sv, Cenv, Senv, Fd, Cnt) ->
  case is_patlist_compatible(Pats, Cv) of
    false ->
      false;
    true ->
      Degree = length(Pats),
      case Degree of
        1 ->
          Cs = [Cv],
          S = [Sv];
        _ ->
          {valuelist, Cs, Degree} = Cv,
          {valuelist, S, Degree} = Sv
      end,
      %% BitInfo is needed for parameterized bit-syntax patterns
      BitInfo = {M, CodeServer, Cenv, Senv},
      Ss = concolic_symbolic:ensure_list(S, length(Cs), Cs, Fd),
      Match = pattern_match_all(BitInfo, Mode, TraceServer, Pats, Cs, Ss, Fd),
      case Match of
        false ->
          false;
        {true, {CMs, SMs}} ->
          NCenv = concolic_lib:add_mappings_to_environment(CMs, Cenv),
          NSenv = concolic_lib:add_mappings_to_environment(SMs, Senv),
          %% Make silent guards
          try eval_expr(M, CodeServer, TraceServer, Guard, NCenv, NSenv, Fd) of
            {true, SGv} ->
              %% CONSTRAINT: SGv is a True guard
              log(Mode, Fd, 'guard', {SGv, true}),
              {true, {Body, NCenv, NSenv, Cnt}};
            {false, SGv} ->
              %% CONSTRAINT: SGv is a False guard
              log(Mode, Fd, 'guard', {SGv, false}),
              false
          catch
            error:_E -> false
          end
      end
  end.

%% --------------------------------------------------------
%% pattern_match_all
%%
%% Pattern Match a series of values against a series of
%% patterns (short-circuited match)
%% --------------------------------------------------------

pattern_match_all(BitInfo, Mode, TraceServer, Pats, Cvs, Svs, Fd) ->
  pattern_match_all(BitInfo, Mode, TraceServer, Pats, Cvs, Svs, [], [], Fd).

pattern_match_all(_BitInfo, _Mode, _TraceServer, [], [], [], CMaps, SMaps, _Fd) ->
  {true, {CMaps, SMaps}};
pattern_match_all(BitInfo, Mode, TraceServer, [P|Ps], [Cv|Cvs], [Sv|Svs], CMaps, SMaps, Fd) ->
  Match = pattern_match(BitInfo, Mode, TraceServer, P, Cv, Sv, CMaps, SMaps, Fd),
  case Match of
    {true, {CMs, SMs}} ->
      pattern_match_all(BitInfo, Mode, TraceServer, Ps, Cvs, Svs, CMs, SMs, Fd);
    false ->
      false
  end.

%% --------------------------------------------------------
%% pattern_match
%%
%% Pattern match a pair of concrete & symbolic values
%% against a single pattern
%% --------------------------------------------------------

%% AtomicLiteral pattern
pattern_match(_BitInfo, Mode, _TraceServer, {c_literal, _Anno, LitVal}, Cv, Sv, CMaps, SMaps, Fd) ->
  case LitVal =:= Cv of
    true ->
      %% CONSTRAINT: Sv =:= Litval
      log(Mode, Fd, 'eq', {LitVal, Sv}),
      {true, {CMaps, SMaps}};
    false ->
      %% CONSTRAINT: Sv =/= Litval
      log(Mode, Fd, 'neq', {LitVal, Sv}),
      false
  end;

%% VariableName pattern
pattern_match(_BitInfo, _Mode, _TraceServer, {c_var, _Anno, Name}, Cv, Sv, CMaps, SMaps, _Fd) ->
  CMs = [{Name, Cv}|CMaps],
  SMs = [{Name, Sv}|SMaps],
  {true, {CMs, SMs}};

%% Tuple pattern
pattern_match(BitInfo, Mode, TraceServer, {c_tuple, _Anno, Es}, Cv, Sv, CMaps, SMaps, Fd)
  when is_tuple(Cv) ->
    Ne = length(Es),
    case tuple_size(Cv) of
      Ne ->
        Cs = tuple_to_list(Cv),
        %% CONSTRAINT: Sv is a tuple of Ne elements
        log(Mode, Fd, 'tuple_size', {'eq', Sv, Ne}),
        Ss = concolic_symbolic:tuple_to_list(Sv, Ne, Cs, Fd),
        pattern_match_all(BitInfo, Mode, TraceServer, Es, Cs, Ss, CMaps, SMaps, Fd);
      _ ->
        %% CONSTRAINT: Sv is a tuple of not Ne elements
        log(Mode, Fd, 'tuple_size', {'neq', Sv, Ne}),
        false
    end;
pattern_match(_BitInfo, Mode, _TraceServer, {c_tuple, _Anno, Es}, _Cv, Sv, _CMaps, _SMaps, Fd) ->
  Ne = length(Es),
  %% CONSTRAINT: Sv is not a tuple
  log(Mode, Fd, 'not_tuple', {Sv, Ne}),
  false;

%% List constructor pattern
pattern_match(BitInfo, Mode, TraceServer, {c_cons, _Anno, Hd, Tl}, [Cv|Cvs], S, CMaps, SMaps, Fd) ->
  %% CONSTRAINT: S is a non empty list
  log(Mode, Fd, 'non_empty_list', S),
  Sv = concolic_symbolic:hd(S, Cv, Fd),
  Svs = concolic_symbolic:tl(S, Cvs, Fd),
  case pattern_match(BitInfo, Mode, TraceServer, Hd, Cv, Sv, CMaps, SMaps, Fd) of
    {true, {CMs, SMs}} ->
      pattern_match(BitInfo, Mode, TraceServer, Tl, Cvs, Svs, CMs, SMs, Fd);
    false ->
      false
  end;
pattern_match(_BitInfo, Mode, _TraceServer, {c_cons, _Anno, _Hd, _Tl}, [], Sv, _CMaps, _SMaps, Fd) ->
  %% CONSTRAINT: Sv is an empty list
  log(Mode, Fd, 'empty_list', Sv),
  false;
pattern_match(_BitInfo, Mode, _TraceServer, {c_cons, _Anno, _Hd, _Tl}, _Cv, Sv, _CMaps, _SMaps, Fd) ->
  %% CONSTRAINT: Sv is not a list
  log(Mode, Fd, 'not_list', Sv),
  false;

%% Alias pattern
pattern_match(BitInfo, Mode, TraceServer, {c_alias, _Anno, Var, Pat}, Cv, Sv, CMaps, SMaps, Fd) ->
  Match = pattern_match(BitInfo, Mode, TraceServer, Pat, Cv, Sv, CMaps, SMaps, Fd),
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
pattern_match(BitInfo, Mode, TraceServer, {c_binary, _Anno, Segments}, Cv, Sv, CMaps, SMaps, Fd) ->
  bit_pattern_match(BitInfo, Mode, TraceServer, Segments, Cv, Sv, CMaps, SMaps, Fd).


%% ===============
%% bit_pattern_match
%% TODO Needs some code cleanup
%% ===============
bit_pattern_match(_BitInfo, Mode, _TraceServer, [], Cv, Sv, CMaps, SMaps, Fd) ->
  case Cv =:= <<>> of
    true ->
      %% TODO Constraint: Sv =:= <<>>
      log(Mode, Fd, 'eq', {<<>>, Sv}),
      {true, {CMaps, SMaps}};
    false ->
      %% TODO Constraint: Sv =/= <<>>
      log(Mode, Fd, 'neq', {<<>>, Sv}),
      false
  end;

bit_pattern_match({M, CodeServer, Cenv, Senv} = BitInfo, Mode, TraceServer, [{c_bitstr, _Anno, Val, Size, Unit, Type, Flags} | Bs], Cv, Sv, CMaps, SMaps, Fd) ->
  case Val of
    {c_literal, _AnnoL, LitVal} ->
      {CSize, SSize} = eval_expr(M, CodeServer, TraceServer, Size, Cenv, Senv, Fd),
      {CUnit, SUnit} = eval_expr(M, CodeServer, TraceServer, Unit, Cenv, Senv, Fd),
      {CType, SType} = eval_expr(M, CodeServer, TraceServer, Type, Cenv, Senv, Fd),
      {CFlags, SFlags} = eval_expr(M, CodeServer, TraceServer, Flags, Cenv, Senv, Fd),
      SEnc = {SSize, SUnit, SType, SFlags},
      try bin_lib:match_bitstring_const(LitVal, CSize, CUnit, CType, CFlags, Cv) of
        CRest ->
          {SX, SRest} = concolic_symbolic:match_bitstring_const(LitVal, SEnc, Sv, CRest, Fd),
          %% TODO Constraint: Match
          log(Mode, Fd, 'match', {SX, SRest, Sv}),
          bit_pattern_match(BitInfo, Mode, TraceServer, Bs, CRest, SRest, CMaps, SMaps, Fd)
      catch
        error:_E ->
          %% TODO Constraint: Not Match
          log(Mode, Fd, 'not_match', {LitVal, SEnc, Sv}),
          false
      end;
    {c_var, _Anno, VarName} ->
      {CSize, SSize} = eval_expr(M, CodeServer, TraceServer, Size, Cenv, Senv, Fd),
      {CUnit, SUnit} = eval_expr(M, CodeServer, TraceServer, Unit, Cenv, Senv, Fd),
      {CType, SType} = eval_expr(M, CodeServer, TraceServer, Type, Cenv, Senv, Fd),
      {CFlags, SFlags} = eval_expr(M, CodeServer, TraceServer, Flags, Cenv, Senv, Fd),
      SEnc = {SSize, SUnit, SType, SFlags},
      try bin_lib:match_bitstring_var(CSize, CUnit, CType, CFlags, Cv) of
        {CX, CRest} ->
          {SX, SRest} = concolic_symbolic:match_bitstring_var(SEnc, Sv, CX, CRest, Fd),
          %% TODO Constraint: Match
          log(Mode, Fd, 'match', {SX, SRest, Sv}),
          {NewCMaps, NewSMaps} =
            case lists:keymember(VarName, 1, CMaps) of
              true ->
                {lists:keyreplace(VarName, 1, CMaps, {VarName, CX}),
                 lists:keyreplace(VarName, 1, SMaps, {VarName, SX})};
              false ->
                {[{VarName, CX} | CMaps],
                 [{VarName, SX} | SMaps]}
            end,
          NewCenv = concolic_lib:add_binding(VarName, CX, Cenv),
          NewSenv = concolic_lib:add_binding(VarName, SX, Senv),
          bit_pattern_match({M, CodeServer, NewCenv, NewSenv}, Mode, TraceServer, Bs, CRest, SRest, NewCMaps, NewSMaps, Fd)
      catch
        error:_E ->
          %% TODO Constraint: Not Match
          log(Mode, Fd, 'not_match_v', {SEnc, Sv}),
          false
      end
  end.

%% --------------------------------------------------------
%% Wrap calls to make_fun/9 when creating a closure to be
%% passed as an argument to a function call
%% --------------------------------------------------------

%% Create a Closure of a local function
create_closure(M, F, Arity, CodeServer, TraceServer, local, Fd) ->
  %% Module is already loaded since create_closure is called by eval_expr
  {ok, MDb} = get_module_db(M, CodeServer),
  Key = {M, F, Arity},
  {Def, _Exported} = retrieve_function(Key, MDb),
  Cenv = concolic_lib:new_environment(),
  Senv = concolic_lib:new_environment(),
  make_fun(M, Arity, CodeServer, TraceServer, Def#c_fun.vars, Def#c_fun.body, Cenv, Senv, Fd);
  
%% Create a Closure when the MFA is a function bound in a letrec
create_closure(M, _F, Arity, CodeServer, TraceServer, {letrec_fun, {Def, Cenv, Senv}}, Fd) ->
  make_fun(M, Arity, CodeServer, TraceServer, Def#c_fun.vars, Def#c_fun.body, Cenv, Senv, Fd).

%% --------------------------------------------------------
%% Create closures.
%% We need to create a closure of the proper arity thus
%% we cannot pass the arguments in a list.
%% Instead, the closure is created manually depending on
%% its arity, thus limiting the support to a maximum
%% allowed arity (currently 12).
%% --------------------------------------------------------

%% Creates a closure from Core Erlang code. 
%% The interpreted code is wrapped in a call to eval_expr.
make_fun(Mod, Arity, CServer, TServer, Vars, Body, Cenv, Senv, FileDescr) ->
  Creator = self(),
  case Arity of
    0 ->
      fun() ->
        make_fun_h1(Mod, [], CServer, TServer, Vars, Body, Cenv, Senv, Creator, FileDescr)
      end;
    1 ->
      fun(A) ->
        Args = [A],
        make_fun_h1(Mod, Args, CServer, TServer, Vars, Body, Cenv, Senv, Creator, FileDescr)
      end;
    2 ->
      fun(A, B) ->
        Args = [A, B],
        make_fun_h1(Mod, Args, CServer, TServer, Vars, Body, Cenv, Senv, Creator, FileDescr)
      end;
    3 ->
      fun(A, B, C) ->
        Args = [A, B, C],
        make_fun_h1(Mod, Args, CServer, TServer, Vars, Body, Cenv, Senv, Creator, FileDescr)
      end;
    4 ->
      fun(A, B, C, D) ->
        Args = [A, B, C, D],
        make_fun_h1(Mod, Args, CServer, TServer, Vars, Body, Cenv, Senv, Creator, FileDescr)
      end;
    5 ->
      fun(A, B, C, D, E) ->
        Args = [A, B, C, D, E],
        make_fun_h1(Mod, Args, CServer, TServer, Vars, Body, Cenv, Senv, Creator, FileDescr)
      end;
    6 ->
      fun(A, B, C, D, E, F) ->
        Args = [A, B, C, D, E, F],
        make_fun_h1(Mod, Args, CServer, TServer, Vars, Body, Cenv, Senv, Creator, FileDescr)
      end;
    7 ->
      fun(A, B, C, D, E, F, G) ->
        Args = [A, B, C, D, E, F, G],
        make_fun_h1(Mod, Args, CServer, TServer, Vars, Body, Cenv, Senv, Creator, FileDescr)
      end;
    8 ->
      fun(A, B, C, D, E, F, G, H) ->
        Args = [A, B, C, D, E, F, G, H],
        make_fun_h1(Mod, Args, CServer, TServer, Vars, Body, Cenv, Senv, Creator, FileDescr)
      end;
    9 ->
      fun(A, B, C, D, E, F, G, H, I) ->
        Args = [A, B, C, D, E, F, G, H, I],
        make_fun_h1(Mod, Args, CServer, TServer, Vars, Body, Cenv, Senv, Creator, FileDescr)
      end;
    10 ->
      fun(A, B, C, D, E, F, G, H, I, J) ->
        Args = [A, B, C, D, E, F, G, H, I, J],
        make_fun_h1(Mod, Args, CServer, TServer, Vars, Body, Cenv, Senv, Creator, FileDescr)
      end;
    11 ->
      fun(A, B, C, D, E, F, G, H, I, J, K) ->
        Args = [A, B, C, D, E, F, G, H, I, J, K],
        make_fun_h1(Mod, Args, CServer, TServer, Vars, Body, Cenv, Senv, Creator, FileDescr)
      end;
    12 ->
      fun(A, B, C, D, E, F, G, H, I, J, K, L) ->
        Args = [A, B, C, D, E, F, G, H, I, J, K, L],
        make_fun_h1(Mod, Args, CServer, TServer, Vars, Body, Cenv, Senv, Creator, FileDescr)
      end;
    13 ->
      fun(A, B, C, D, E, F, G, H, I, J, K, L, M) ->
        Args = [A, B, C, D, E, F, G, H, I, J, K, L, M],
        make_fun_h1(Mod, Args, CServer, TServer, Vars, Body, Cenv, Senv, Creator, FileDescr)
      end;
    14 ->
      fun(A, B, C, D, E, F, G, H, I, J, K, L, M, N) ->
        Args = [A, B, C, D, E, F, G, H, I, J, K, L, M, N],
        make_fun_h1(Mod, Args, CServer, TServer, Vars, Body, Cenv, Senv, Creator, FileDescr)
      end;
    15 ->
      fun(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) ->
        Args = [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O],
        make_fun_h1(Mod, Args, CServer, TServer, Vars, Body, Cenv, Senv, Creator, FileDescr)
      end;
    _ ->
      exception('error', {'over_lambda_fun_argument_limit', Arity})
  end.

make_fun_h1(Mod, Args, CServer, TServer, Vars, Body, Cenv, Senv, Creator, FileDescr) ->
  {NCenv, NSenv} = register_new_environments(Args, Vars, Cenv, Senv),
  {CodeServer, TraceServer} = validate_servers(CServer, TServer),
  Fd = validate_file_descriptor(TraceServer, Creator, FileDescr),
  eval_expr(Mod, CodeServer, TraceServer, Body, NCenv, NSenv, Fd).

register_new_environments([], _Vars, Cenv, Senv) ->
  {Cenv, Senv};
register_new_environments(Args, Vars, Cenv, Senv) ->
  {CAs, SAs} = unzip_args(Args),
  NCenv = concolic_lib:bind_parameters(CAs, Vars, Cenv),
  NSenv = concolic_lib:bind_parameters(SAs, Vars, Senv),
  {NCenv, NSenv}.

%% Creates a closure from an MFA (emulates the behaviour
%% of erlang:make_fun/3) 
make_fun(Mod, Func, Arity, CServer, TServer, FileDescr) ->
  Creator = self(),
  case Arity of
    0 ->
      fun() ->
        make_fun_h2(Mod, Func, [], CServer, TServer, Creator, FileDescr)
      end;
    1 ->
      fun(A) ->
        Args = [A],
        make_fun_h2(Mod, Func, Args, CServer, TServer, Creator, FileDescr)
      end;
    2 ->
      fun(A, B) ->
        Args = [A, B],
        make_fun_h2(Mod, Func, Args, CServer, TServer, Creator, FileDescr)
      end;
    3 ->
      fun(A, B, C) ->
        Args = [A, B, C],
        make_fun_h2(Mod, Func, Args, CServer, TServer, Creator, FileDescr)
      end;
    4 ->
      fun(A, B, C, D) ->
        Args = [A, B, C, D],
        make_fun_h2(Mod, Func, Args, CServer, TServer, Creator, FileDescr)
      end;
    5 ->
      fun(A, B, C, D, E) ->
        Args = [A, B, C, D, E],
        make_fun_h2(Mod, Func, Args, CServer, TServer, Creator, FileDescr)
      end;
    6 ->
      fun(A, B, C, D, E, F) ->
        Args = [A, B, C, D, E, F],
        make_fun_h2(Mod, Func, Args, CServer, TServer, Creator, FileDescr)
      end;
    7 ->
      fun(A, B, C, D, E, F, G) ->
        Args = [A, B, C, D, E, F, G],
        make_fun_h2(Mod, Func, Args, CServer, TServer, Creator, FileDescr)
      end;
    8 ->
      fun(A, B, C, D, E, F, G, H) ->
        Args = [A, B, C, D, E, F, G, H],
        make_fun_h2(Mod, Func, Args, CServer, TServer, Creator, FileDescr)
      end;
    9 ->
      fun(A, B, C, D, E, F, G, H, I) ->
        Args = [A, B, C, D, E, F, G, H, I],
        make_fun_h2(Mod, Func, Args, CServer, TServer, Creator, FileDescr)
      end;
    10 ->
      fun(A, B, C, D, E, F, G, H, I, J) ->
        Args = [A, B, C, D, E, F, G, H, I, J],
        make_fun_h2(Mod, Func, Args, CServer, TServer, Creator, FileDescr)
      end;
    11 ->
      fun(A, B, C, D, E, F, G, H, I, J, K) ->
        Args = [A, B, C, D, E, F, G, H, I, J, K],
        make_fun_h2(Mod, Func, Args, CServer, TServer, Creator, FileDescr)
      end;
    12 ->
      fun(A, B, C, D, E, F, G, H, I, J, K, L) ->
        Args = [A, B, C, D, E, F, G, H, I, J, K, L],
        make_fun_h2(Mod, Func, Args, CServer, TServer, Creator, FileDescr)
      end;
    13 ->
      fun(A, B, C, D, E, F, G, H, I, J, K, L, M) ->
        Args = [A, B, C, D, E, F, G, H, I, J, K, L, M],
        make_fun_h2(Mod, Func, Args, CServer, TServer, Creator, FileDescr)
      end;
    14 ->
      fun(A, B, C, D, E, F, G, H, I, J, K, L, M, N) ->
        Args = [A, B, C, D, E, F, G, H, I, J, K, L, M, N],
        make_fun_h2(Mod, Func, Args, CServer, TServer, Creator, FileDescr)
      end;
    15 ->
      fun(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) ->
        Args = [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O],
        make_fun_h2(Mod, Func, Args, CServer, TServer, Creator, FileDescr)
      end;
    _ ->
      exception('error', {'over_lambda_fun_argument_limit', Arity})
  end.

make_fun_h2(Mod, Func, Args, CServer, TServer, Creator, FileDescr) ->
  {CAs, SAs} = unzip_args(Args), %% If Args =:= [] then unzip_args([]) will return {[], []}
  {CodeServer, TraceServer} = validate_servers(CServer, TServer),
  Fd = validate_file_descriptor(TraceServer, Creator, FileDescr),
  eval({named, {Mod, Func}}, CAs, SAs, external, CodeServer, TraceServer, Fd).

%% --------------------------------------------------------
%% Check if the given servers correspond to the current 
%% node. If not, get the pids of the proper servers.
%% Needed when a closure is created in node A and is 
%% executed in node B.
%% --------------------------------------------------------
validate_servers(CodeServer, TraceServer) ->
  Node = node(),
  case Node =:= node(TraceServer) of
    true  -> {CodeServer, TraceServer};
    false -> concolic_tserver:node_servers(TraceServer, Node)
  end.

%% --------------------------------------------------------
%% Check if the given file descriptor correponds to this 
%% process. If not get the proper file descriptor.
%% Needed when a closure is created in process A and is
%% executed in process B.
%% --------------------------------------------------------
validate_file_descriptor(TraceServer, Pid, Fd) ->
  case Pid =:= self() of
    true  -> Fd;
    false -> concolic_tserver:file_descriptor(TraceServer)
  end.
  
%% --------------------------------------------------------
%% Evaluates a BIF call
%% --------------------------------------------------------
-spec evaluate_bif(mfa(), [term()], [term()], file:io_device()) -> result().

evaluate_bif({M, F, _A} = MFA, CAs, SAs, Fd) ->
  CR = apply(M, F, CAs),
  SR = concolic_symbolic:mock_bif(MFA, {CAs, SAs}, CR, Fd),
  {CR, SR}.

%% --------------------------------------------------------
%% Encode and Decode Msgs
%% --------------------------------------------------------

%% Encode a Message
encode_msg(TraceServer, Dest, CMsg, SMsg) ->
  case concolic_tserver:is_monitored(TraceServer, Dest) of
    true  -> {?CONCOLIC_PREFIX_MSG, zip_one(CMsg, SMsg)};
    false -> CMsg
  end.

%% Decode a Message
decode_msg({?CONCOLIC_PREFIX_MSG, Msg}) -> unzip_msg(Msg);
decode_msg(Msg) -> unzip_msg(Msg).

%% --------------------------------------------------------
%% Zip and Unzip concrete-semantic values
%%
%% A zipped value is a {'__zip', CVal, SVal}.
%% Zipped values are used when sending messages, raising 
%% exceptions and  passing arguments to closures.
%% --------------------------------------------------------

%% zip_one
zip_one(Cv, Sv) -> {'__zip', Cv, Sv}.

%% unzip_one
unzip_one({'__zip', Cv, Sv}) -> {Cv, Sv};
unzip_one(V) -> {V, V}.

%% zip_args
zip_args(CAs, SAs) when is_list(CAs), is_list(SAs) ->
  lists:zipwith(fun zip_one/2, CAs, SAs).

%% unzip_args
unzip_args(As) when is_list(As) ->
   lists:unzip([unzip_one(A) || A <- As]).

%% unzip_error // for exception reasons
-spec unzip_error(term()) -> {term(), term()}.
unzip_error({nocatch, {'__zip', Cv, Sv}}) ->
  {Cv, Sv};
unzip_error({{'__zip', Cv, Sv}, Stack}) when is_list(Stack) ->
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
  
%% --------------------------------------------------------
%% Initializations called when a new process is spawned.
%% *  Register the parent process to the TraceServer
%% *  Open a new file to store the process's trace data
%% *  Then proceed with interpreting the MFA call
%% --------------------------------------------------------
register_and_apply(TraceServer, Parent, Args) ->
  fun() ->
    {ok, Fd} = concolic_tserver:register_to_trace(TraceServer, Parent),
    Parent ! {self(), registered},
    erlang:apply(?MODULE, eval, Args ++ [Fd])
  end.

%% --------------------------------------------------------
%% Interact with the CodeServer and ask for the ETS Table
%% where the code of the module M is stored.
%%
%% Optimization : For caching purposes, the MDb is stored
%% in the process dictionary for subsequent lookups
%% --------------------------------------------------------
-spec get_module_db(atom(), pid()) -> {'ok', ets:tab()} | 'preloaded'.

get_module_db(M, CodeServer) ->
  What = {?CONCOLIC_PREFIX_PDICT, M},
  case get(What) of
    undefined ->
      case concolic_cserver:load(CodeServer, M) of
        %% Module Code loaded
        {ok, MDb} = Ok -> 
          put(What, MDb),
          Ok;
        %% Preloaded Module
        preloaded ->
          preloaded;
        %% Cover Compiled Module
        cover_compiled ->
          exception('error', {'cover_compiled', M});
        %% Invalid Module
        non_existing ->
          exception('error', {'undef', M});
        %% Any Error during Code Loading
        {error, Error} ->
          exception('error', Error)
      end;
    MDb ->
      {ok, MDb}
  end.
  
%% --------------------------------------------------------
%% Retrieves the code and exported type of an MFA
%%
%% Optimization : For caching purposes, the function 
%% definition is stored in the process dictionary for 
%% subsequent lookups
%% --------------------------------------------------------
-spec retrieve_function(mfa(), ets:tab()) -> {cerl:c_fun(), boolean()}.

retrieve_function(FuncKey, ModDb) ->
  What = {?CONCOLIC_PREFIX_PDICT, FuncKey},
  case get(What) of
    undefined ->
      case ets:lookup(ModDb, FuncKey) of
        [] ->
          exception('error', {'undef', FuncKey});
        [{FuncKey, Val}] ->
          put(What, Val),
          Val
      end;
    Val ->
      Val
  end.
  
%% --------------------------------------------------------
%% Ensures compatibility between the type of the call
%% and the exported status of the MFA
%% --------------------------------------------------------
-spec check_exported(exported(), calltype(), mfa()) -> 'ok'.

check_exported(true, _CallType, _MFA) -> ok;
check_exported(false, local, _MFA)    -> ok;
check_exported(false, external, MFA)  -> exception('error', {'not_exported', MFA}).

%% --------------------------------------------------------
%% Resolve the type of an MFA call
%% --------------------------------------------------------
find_call_type(_M, _M)   -> local;
find_call_type(_M1, _M2) -> external.

%% --------------------------------------------------------
%% Y combinator for a function with arity 0
%% --------------------------------------------------------
y(M) ->
  G = fun(F) -> M(fun() -> (F(F))() end) end,
  G(G).

%% --------------------------------------------------------
%% Calculates if the number of patterns in a clause is 
%% compatible to the numbers of actual values that are 
%% to be matched against
%% --------------------------------------------------------
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
  
%% --------------------------------------------------------
%% Validate the timeout value of a receive expression
%% --------------------------------------------------------
check_timeout(infinity, _Sv, _Fd) ->
  %% TODO Constraint: Sv=infinity
  true;
check_timeout(Timeout, _Sv, _Fd) when is_integer(Timeout) -> 
  %% TODO Constraint: Sv is non_neg_int()
  Timeout >= 0;
check_timeout(Timeout, _Sv, _Fd) ->
  exception('error', {'invalid_timeout', Timeout}).
  
%% --------------------------------------------------------
%% Concatenate a list of bistrings
%% --------------------------------------------------------
append_segments(Cs, Ss, Fd) ->
  Cv = lists:foldl(
    fun(Seg, Bin) -> <<Seg/bitstring, Bin/bitstring>> end,
    <<>>, lists:reverse(Cs)
  ),
  Sv = concolic_symbolic:append_segments(Ss, Fd),
  {Cv, Sv}.

%% --------------------------------------------------------
%% Adjust the arguments of a function
%% --------------------------------------------------------

%% Transfrom all the calls to slave:start/{1,2,3} and slave:start_link/{1,2,3}
%% to the slave:start/3 calls and slave:start_link/3 calls respectively
%% A new slave node will be given the path to the ebin of the tool
adjust_arguments(slave, F, CAs, SAs, Fd) when F =:= 'start'; F =:= 'start_link' ->
  Arity = length(CAs),
  SAs_e = concolic_symbolic:ensure_list(SAs, Arity, CAs, Fd),
  Ebin = " -pa " ++ ?EBIN,
  case CAs of
    [Host] ->
      N = atom_to_list(node()),
      Name = hd(string:tokens(N, "@")),
      [SHost] = SAs_e,
      {[Host, Name, Ebin], [SHost, Name, Ebin]};
    [Host, Name] ->
      [SHost, SName] = SAs_e,
      {[Host, Name, Ebin], [SHost, SName, Ebin]};
    [Host, Name, Args] ->
      %% Also add the path to the symbolic Args ???
      {[Host, Name, Args ++ Ebin], SAs_e};
    _ ->
      {CAs, SAs}
  end;
%% All other functions will have their arguments unaltered
adjust_arguments(_M, _F, CAs, SAs, _Fd) -> {CAs, SAs}.

%% --------------------------------------------------------
%% Logging function that wraps all the calls
%% to the proper concolic_encdec logging functions
%% --------------------------------------------------------
log(T, D, C, I) -> concolic_encdec:log(T, D, C, I).


