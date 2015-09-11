%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_eval).

-export([i/4, eval/7, unzip_error/1]).

-include("include/cuter_macros.hrl").
-include_lib("compiler/src/core_parse.hrl").

-export_type([result/0, valuelist/0, value/0]).

-type calltype() :: local | external.
-type result()   :: {any(), any()}.
-type class()    :: error | exit | throw.
-type eval()     :: {named, module(), atom()}
                  | {lambda, function()}
                  | {letrec_func, {atom(), atom(), cerl:c_fun(), function()}}.
-type value()    :: any().

%% Used to represent list of values for Core Erlang interpretation
-record(valuelist, {
  values :: [value()],
  degree :: non_neg_integer()
}).
-type valuelist() :: #valuelist{}.


%% -------------------------------------------------------------------
%% Wrapper exported function that spawns an interpreter process 
%% which returns the value of an MFA call to the interpreter server
%% -------------------------------------------------------------------
-spec i(module(), atom(), [any()], servers()) -> pid().
i(M, F, As, Servers) ->
  Root = self(),
  I =
    fun() ->
      {SymbAs, Mapping} = cuter_symbolic:abstract(As),
      {ok, Fd} = cuter_monitor:subscribe(Servers#svs.monitor, Root),
      cuter_log:log_symb_params(Fd, SymbAs),
      %% Log the spec of the MFA
      MFA = {M, F, length(As)},
      log_mfa_spec(Fd, Servers#svs.code, MFA),
      cuter_iserver:send_mapping(Root, Mapping),
      NMF = {named, M, F},
      Tag = cuter_cerl:empty_tag(),
      Ret = eval(NMF, As, SymbAs, external, Servers, Tag, Fd),
      cuter_log:close_file(Fd),
      cuter_iserver:int_return(Root, Ret)
    end,
  erlang:spawn(I).

%% Parse and log the spec of the mfa.
-spec log_mfa_spec(file:io_device(), pid(), mfa()) -> ok.
-ifdef(USE_SPECS).
log_mfa_spec(Fd, CodeServer, MFA) ->
  case cuter_codeserver:retrieve_spec(CodeServer, MFA) of
    {ok, Spec} -> cuter_log:log_spec(Fd, Spec);
    error -> ok
  end.
-else.
log_mfa_spec(_, _, _) -> ok.
-endif.

%% -------------------------------------------------------------------
%% eval
%%
%% Concrete/Symbolic Evaluation and Logging of an MFA call
%% -------------------------------------------------------------------
-spec eval(eval(), [any()], [any()], calltype(), servers(), cuter_cerl:tag(), file:io_device()) -> result().

%% Handle spawns so that the spawned process will be interpreted
%% and not directly executed

%% spawn/{1,2,3,4} & spawn_link/{1,2,3,4}
eval({named, erlang, F}, CAs, SAs, _CallType, Servers, Tag, Fd) when F =:= spawn; F =:= spawn_link ->
  Arity = length(CAs),
  SAs_e = cuter_symbolic:ensure_list(SAs, Arity, Fd),
  Rf = erlang:make_ref(),
  ChildP =
    case CAs of
      [Fun] ->
        [_SFun] = SAs_e,
        %% Constraint: SFun=Fun
        EvalAs = [{lambda, Fun}, [], [], local, Servers, Tag],
        Child = subscribe_and_apply(Servers#svs.monitor, self(), EvalAs, Rf),
        erlang:F(Child);
      [Node, Fun] ->
        [_SNode, _SFun] = SAs_e,
        %% Constraints: SNode=Node, SFun=Fun
        NSvs = cuter_monitor:node_servers(Servers#svs.monitor, Node),
        EvalAs = [{lambda, Fun}, [], [], local, NSvs, Tag],
        Child = subscribe_and_apply(NSvs#svs.monitor, self(), EvalAs, Rf),
        erlang:F(Node, Child);
      [Mod, Fun, Args] ->
        [_SMod, _SFun, SArgs] = SAs_e,
        %% Constraints: SMod = Mod, SFun=Fun
        Call = find_call_type(erlang, Mod),
        EvalAs = [{named, Mod, Fun}, Args, SArgs, Call, Servers, Tag],
        Child = subscribe_and_apply(Servers#svs.monitor, self(), EvalAs, Rf),
        erlang:F(Child);
      [Node, Mod, Fun, Args] ->
        [_SNode, _SMod, _SFun, SArgs] = SAs_e,
        %% Constraints: SNode=Node, SMod = Mod, SFun=Fun
        NSvs = cuter_monitor:node_servers(Servers#svs.monitor, Node),
        Call = find_call_type(erlang, Mod),
        EvalAs = [{named, Mod, Fun}, Args, SArgs, Call, NSvs, Tag],
        Child = subscribe_and_apply(NSvs#svs.monitor, self(), EvalAs, Rf),
        erlang:F(Node, Child);
      _ ->
        exception(error, {undef, {erlang, spawn, Arity}})
    end,
  receive
    {ChildP, registered} ->
      cuter_log:log_spawn(Fd, ChildP, Rf),
      {ChildP, ChildP}
  end;

%% spawn_monitor/{1,3}
eval({named, erlang, spawn_monitor}, CAs, SAs, _CallType, Servers, Tag, Fd) ->
  Arity = length(CAs),
  SAs_e = cuter_symbolic:ensure_list(SAs, Arity, Fd),
  Rf = erlang:make_ref(),
  EvalAs =
    case CAs of
      [Fun] ->
        [_SFun] = SAs_e,
        %% Constraint: SFun=Fun
        [{lambda, Fun}, [], [], local, Servers, Tag];
      [Mod, Fun, Args] ->
        [_SMod, _SFun, SArgs] = SAs_e,
        %% Constraints: SMod = Mod, SFun=Fun
        Call = find_call_type(erlang, Mod),
        [{named, Mod, Fun}, Args, SArgs, Call, Servers, Tag];
      _ ->
        exception(error, {undef, {erlang, spawn_monitor, Arity}})
    end,
  Child = subscribe_and_apply(Servers#svs.monitor, self(), EvalAs, Rf),
  {ChildP, _ChildRef} = CC = erlang:spawn_monitor(Child),
  receive
    {ChildP, registered} ->
      cuter_log:log_spawn(Fd, ChildP, Rf),
      {CC, CC}
  end;

%% spawn_opt/{1,3,4,5}
eval({named, erlang, spawn_opt}, CAs, SAs, _CallType, Servers, Tag, Fd) ->
  Arity = length(CAs),
  SAs_e = cuter_symbolic:ensure_list(SAs, Arity, Fd),
  Rf = erlang:make_ref(),
  R =
    case CAs of
      [Fun, Opts] ->
        [_SFun, _SOpts] = SAs_e,
        %% Constraints: SFun=Fun, SOpts=Opts
        EvalAs = [{lambda, Fun}, [], [], local, Servers, Tag],
        Child = subscribe_and_apply(Servers#svs.monitor, self(), EvalAs, Rf),
        erlang:spawn_opt(Child, Opts);
      [Node, Fun, Opts] ->
        [_SNode, _SFun, _SOpts] = SAs_e,
        %% Constraints: SNode=Node, SFun=Fun, SOpts=Opts
        NSVs = cuter_monitor:node_servers(Servers#svs.monitor, Node),
        EvalAs = [{lambda, Fun}, [], [], local, NSVs, Tag],
        Child = subscribe_and_apply(NSVs#svs.monitor, self(), EvalAs, Rf),
        erlang:spawn_opt(Node, Child, Opts);
      [Mod, Fun, Args, Opts] ->
        [_SMod, _SFun, SArgs, _SOpts] = SAs_e,
        %% Constraints: SMod=Mode, SFun=Fun, SOpts=Opts
        Call = find_call_type(erlang, Mod),
        EvalAs = [{named, Mod, Fun}, Args, SArgs, Call, Servers, Tag],
        Child = subscribe_and_apply(Servers#svs.monitor, self(), EvalAs, Rf),
        erlang:spawn_opt(Child, Opts);
      [Node, Mod, Fun, Args, Opts] ->
        [_SNode, _SMod, _SFun, SArgs, _SOpts] = SAs_e,
        %% Constraints: SNode=Node, SMod=Mode, SFun=Fun, SOpts=Opts
        Call = find_call_type(erlang, Mod),
        NSvs = cuter_monitor:node_servers(Servers#svs.monitor, Node),
        EvalAs = [{named, Mod, Fun}, Args, SArgs, Call, NSvs, Tag],
        Child = subscribe_and_apply(NSvs#svs.monitor, self(), EvalAs, Rf),
        erlang:spawn_opt(Node, Child, Opts);
      _ ->
        exception(error, {undef, {erlang, spawn_opt, Arity}})
    end,
  ChildP =
    case R of
      {Pid, _Ref} -> Pid;
      Pid -> Pid
    end,
  receive
    {ChildP, registered} ->
      cuter_log:log_spawn(Fd, ChildP, Rf),
      {R, R}
  end;

%% Handle message sending primitives
%% so as to zip the concrete and symbolic message

%% Redirect erlang:'!'/2 to erlang:send/2
eval({named, erlang, '!'}, [_, _] = CAs, SAs, CallType, Servers, Tag, Fd) ->
  eval({named, erlang, send}, CAs, SAs, CallType, Servers, Tag, Fd);

%% send/{2,3}
eval({named, erlang, send}, CAs, SAs, _CallType, Servers, _Tag, Fd) ->
  Arity = length(CAs),
  SAs_e = cuter_symbolic:ensure_list(SAs, Arity, Fd),
  case CAs of
    [CDest_u, CMsg] ->
      [_SDest, SMsg] = SAs_e,
      %% Constraint: CDest=SDest
      CDest = adjust_message_destination(CDest_u),
      Msg = encode_msg(Servers#svs.monitor, CDest, CMsg, SMsg, Fd),
      erlang:send(CDest, Msg),
      {CMsg, SMsg};
    [CDest_u, CMsg, COpts] ->
      [_SDest, SMsg, _SOpts] = SAs_e,
      %% Constraint: CDest=SDest, COpts=SOpts
      CDest = adjust_message_destination(CDest_u),
      Msg = encode_msg(Servers#svs.monitor, CDest, CMsg, SMsg, Fd),
      R = erlang:send(CDest, Msg, COpts),
      {R, R};
    _ ->
      exception(error, {undef, {erlang, send, Arity}})
  end;

%% send_after/3
eval({named, erlang, send_after}, CAs, SAs, _CallType, Servers, _Tag, Fd) ->
  Arity = length(CAs),
  SAs_e = cuter_symbolic:ensure_list(SAs, Arity, Fd),
  case CAs of
    [CTime, CDest_u, CMsg] ->
      [_STime, _SDest, SMsg] = SAs_e,
      %% Constraint CTime=STime, CDest=SDest
      CDest = adjust_message_destination(CDest_u),
      Msg = encode_msg(Servers#svs.monitor, CDest, CMsg, SMsg, Fd),
      R = erlang:send_after(CTime, CDest, Msg),
      {R, R};
    _ ->
      exception(error, {undef, {erlang, send_after, Arity}})
  end;

%% send_nosuspend/{2,3}
eval({named, erlang, send_nosuspend}, CAs, SAs, _CallType, Servers, _Tag, Fd) ->
  Arity = length(CAs),
  SAs_e = cuter_symbolic:ensure_list(SAs, Arity, Fd),
  case CAs of
    [CDest_u, CMsg] ->
      [_SDest, SMsg] = SAs_e,
      %% Constraint CDest=SDest
      CDest = adjust_message_destination(CDest_u),
      Msg = encode_msg(Servers#svs.monitor, CDest, CMsg, SMsg, Fd),
      R = erlang:send_nosuspend(CDest, Msg),
      {R, R};
    [CDest_u, CMsg, COpts] ->
      [_SDest, SMsg, _SOpts] = SAs_e,
      %% Constraint CDest=SDest, COpts=SOpts
      CDest = adjust_message_destination(CDest_u),
      Msg = encode_msg(Servers#svs.monitor, CDest, CMsg, SMsg, Fd),
      R = erlang:send_nosuspend(CDest, Msg, COpts),
      {R, R};
    _ ->
      exception(error, {undef, {erlang, send_nosuspend, Arity}})
  end;

%% Handle functions that raise exceptions
%% so as to zip the concrete and symbolic reason

%% throw/1
eval({named, erlang, throw}, CAs, SAs, _CallType, _Servers, _Tag, Fd) ->
  Arity = length(CAs),
  SAs_e = cuter_symbolic:ensure_list(SAs, Arity, Fd),
  case CAs of
    [CThrow] ->
      [SThrow] = SAs_e,
      Throw = zip_one(CThrow, SThrow),
      erlang:throw(Throw);
    _ ->
      exception(error, {undef, {erlang, throw, Arity}})
  end;

%% exit/{1,2}
eval({named, erlang, exit}, CAs, SAs, _CallType, _Servers, _Tag, Fd) ->
  Arity = length(CAs),
  SAs_e = cuter_symbolic:ensure_list(SAs, Arity, Fd),
  case CAs of
    [CExit] ->
      [SExit] = SAs_e,
      Exit = 
        case CExit of
          %% Constraint: SExit=normal
          normal -> normal;
          _ -> zip_one(CExit, SExit)
        end,
      erlang:exit(Exit);
    [CDest, CExit] ->
      [_SDest, SExit] = SAs_e,
      %% Constraint CDest=SDest
      Exit = 
        case CExit of
          %% Constraint: SExit=normal
          normal -> normal;
          %% Constraint: SExit=kill
          kill -> kill;
          _ -> zip_one(CExit, SExit)
        end,
      R = erlang:exit(CDest, Exit),
      {R, R};
    _ ->
      exception(error, {undef, {erlang, exit, Arity}})
  end;

%% error/{1,2}
eval({named, erlang, error}, CAs, SAs, _CallType, _Servers, _Tag, Fd) ->
  Arity = length(CAs),
  SAs_e = cuter_symbolic:ensure_list(SAs, Arity, Fd),
  case CAs of
    [CError] ->
      [SError] = SAs_e,
      Error = zip_one(CError, SError),
      erlang:error(Error);
    [CError, CArgs] ->
      [SError, _SArgs] = SAs_e,
      %% create constraint CArgs=SArgs
      Error = zip_one(CError, SError),
      erlang:error(Error, CArgs);
    _ ->
      exception(error, {undef, {erlang, error, Arity}})
  end;

%% raise/3
eval({named, erlang, raise}, CAs, SAs, _CallType, _Servers, _Tag, Fd) ->
  Arity = length(CAs),
  SAs_e = cuter_symbolic:ensure_list(SAs, Arity, Fd),
  case CAs of
    [CClass, CReason, CStacktrace] ->
      [_SClass, SReason, _] = SAs_e,
      %% Create constraint Class=SClass
      R = zip_one(CReason, SReason),
      erlang:raise(CClass, R, CStacktrace);
    _ ->
      exception(error, {undef, {erlang, raise, Arity}})
  end;

%% Handle other important functions

%% make_fun/3
eval({named, erlang, make_fun}, CAs, SAs, _CallType, Servers, Tag, Fd) ->
  Arity = length(CAs),
  _ = cuter_symbolic:ensure_list(SAs, Arity, Fd),
  case CAs of
    [M, F, A] ->
      CR = make_fun(M, F, A, Servers, Tag, Fd),
      %% We assume that we cannot symbolically abstract the result of make_fun/3
      {CR, CR};
    _ ->
      exception(error, {undef, {erlang, make_fun, Arity}})
  end;

%% apply/{2,3}
eval({named, erlang, apply}, CAs, SAs, _CallType, Servers, Tag, Fd) ->
  Arity = length(CAs),
  SAs_e = cuter_symbolic:ensure_list(SAs, Arity, Fd),
  case CAs of
    [Fun, Args] ->
      [_SFun, SArgs] = SAs_e,
      %% Constraint: Fun=SFun
      eval({lambda, Fun}, Args, SArgs, local, Servers, Tag, Fd);
    [M, F, Args] ->
      [_SMod, _SFun, SArgs] = SAs_e,
      %% Constraints: SMod = M, SFun=F
      Call = find_call_type(erlang, M),
      eval({named, M, F}, Args, SArgs, Call, Servers, Tag, Fd);
    _ ->
      exception(error, {undef, {erlang, apply, Arity}})
  end;

%% Generic case

%% Handle an MFA
eval({named, M, F}, CAs_b, SAs_b, CallType, Servers, Tag, Fd) ->
  {CAs, SAs} = adjust_arguments(M, F, CAs_b, SAs_b, Fd),
  Arity = length(CAs),
  SAs_e = cuter_symbolic:ensure_list(SAs, Arity, Fd),
  MFA = {M, F, Arity},
  case access_mfa_code(MFA, Servers) of
    error -> evaluate_bif(MFA, CAs, SAs_e, Servers, Tag, Fd);
    {NM, {Def, Exported}} ->
      check_exported(Exported, CallType, MFA),
      NCenv = cuter_env:new_environment(),
      NSenv = cuter_env:new_environment(),
      Cenv = cuter_env:bind_parameters(CAs, Def#c_fun.vars, NCenv),
      Senv = cuter_env:bind_parameters(SAs_e, Def#c_fun.vars, NSenv),
      eval_expr(Def#c_fun.body, NM, Cenv, Senv, Servers, Fd)
  end;

%% Handle a Closure
eval({lambda, Closure}, CAs, SAs, _CallType, _Servers, _Tag, Fd) ->
  SAs_e = cuter_symbolic:ensure_list(SAs, length(CAs), Fd),
  ZAs = zip_args(CAs, SAs_e),
  apply(Closure, ZAs);

%% Handle a function bound in a letrec expression
eval({letrec_func, {M, _F, Def, E}}, CAs, SAs, _CallType, Servers, _Tag, Fd) ->
  {Cenv, Senv} = E(),
  SAs_e = cuter_symbolic:ensure_list(SAs, length(CAs), Fd),
  NCenv = cuter_env:bind_parameters(CAs, Def#c_fun.vars, Cenv),
  NSenv = cuter_env:bind_parameters(SAs_e, Def#c_fun.vars, Senv),
  eval_expr(Def#c_fun.body, M, NCenv, NSenv, Servers, Fd).


%% --------------------------------------------------------
%% eval_expr
%%
%% Evaluates a Core Erlang expression
%% --------------------------------------------------------
-spec eval_expr(cerl:cerl(), module(), cuter_env:environment(), cuter_env:environment(), servers(), file:io_device()) -> result().

%% c_apply
eval_expr({c_apply, Anno, Op, Args}, M, Cenv, Senv, Servers, Fd) ->
  {Op_c, _Op_s} = eval_expr(Op, M, Cenv, Senv, Servers, Fd),
  Fun = 
    fun(A) ->
      {A_c, A_s} = eval_expr(A, M, Cenv, Senv, Servers, Fd),
      %% Will create closures where appropriate
      case A_c of
        {?FUNCTION_PREFIX, {F, Arity}} ->
          %% local func (external func is already in make_fun/3 in core erlang)
          Clz = create_closure(M, F, Arity, local, Servers, Fd),
          {Clz, Clz};
        {letrec_func, {Mod, F, Arity, Def, E}} ->
          %% letrec func
          {Ce, Se} = E(),
          Clz = create_closure(Mod, F, Arity, {letrec_func, {Def, Ce, Se}}, Servers, Fd),
          {Clz, Clz};
        _ ->
          {A_c, A_s}
      end
    end,
  ZAs = [Fun(A) || A <- Args],
  {CAs, SAs} = lists:unzip(ZAs),
  Tags = cuter_cerl:get_tags(Anno),
  case Op_c of %% See eval_expr(#c_var{}, ...) output for reference
    {?FUNCTION_PREFIX, {Func, _Arity}} ->
      eval({named, M, Func}, CAs, SAs, local, Servers, Tags#tags.this, Fd);
    {letrec_func, {Mod, Func, _Arity, Def, E}} ->
      eval({letrec_func, {Mod, Func, Def, E}}, CAs, SAs, local, Servers, Tags#tags.this, Fd);
    Closure ->
      %% Constraint OP_s = OP_c (in case closure is made by make_fun)
      eval({lambda, Closure}, CAs, SAs, local, Servers, Tags#tags.this, Fd)
  end;

%% c_binary
%% TODO Use the tags of segments.
eval_expr({c_binary, _Anno, Segments}, M, Cenv, Senv, Servers, Fd) ->
  Segs = [eval_expr(S, M, Cenv, Senv, Servers, Fd) || S <- Segments],
  {Cs, Ss} = lists:unzip(Segs),
  append_segments(Cs, Ss, Fd);

%% c_bitstr
eval_expr({c_bitstr, _Anno, Val, Size, Unit, Type, Flags}, M, Cenv, Senv, Servers, Fd) ->
  {Cv, Sv} = eval_expr(Val, M, Cenv, Senv, Servers, Fd),
  {Size_c, Size_s} = eval_expr(Size, M, Cenv, Senv, Servers, Fd),
  {Unit_c, Unit_s} = eval_expr(Unit, M, Cenv, Senv, Servers, Fd),
  {Type_c, Type_s} = eval_expr(Type, M, Cenv, Senv, Servers, Fd),
  {Flags_c, Flags_s} = eval_expr(Flags, M, Cenv, Senv, Servers, Fd),
  Bin_c = cuter_binlib:make_bitstring(Cv, Size_c, Unit_c, Type_c, Flags_c),
  Bin_s = cuter_symbolic:make_bitstring(Sv, {Size_s, Unit_s, Type_s, Flags_s}, Bin_c, Size_c, Fd),
  {Bin_c, Bin_s};

%% c_call
eval_expr({c_call, Anno, Mod, Name, Args}, M, Cenv, Senv, Servers, Fd) ->
  {Mod_c, _Mod_s} = eval_expr(Mod, M, Cenv, Senv, Servers, Fd),
  {Fv_c, _Fv_s} = eval_expr(Name, M, Cenv, Senv, Servers, Fd),
  Fun = 
    fun(A) ->
      {A_c, A_s} = eval_expr(A, M, Cenv, Senv, Servers, Fd),
      %% Will create closures where appropriate
      case A_c of
        {?FUNCTION_PREFIX, {F, Arity}} ->
          %% local func (external func is already in make_fun/3 in core erlang)
          Clz = create_closure(M, F, Arity, local, Servers, Fd),
          {Clz, Clz};
        {letrec_func, {Mod, F, Arity, Def, E}} ->
          %% letrec func
          {Ce, Se} = E(),
          Clz = create_closure(Mod, F, Arity, {letrec_func, {Def, Ce, Se}}, Servers, Fd),
          {Clz, Clz};
        _ ->
          {A_c, A_s}
      end
    end,
  ZAs = [Fun(A) || A <- Args],
  {CAs, SAs} = lists:unzip(ZAs),
  %% Constraints Mod_c = Mod_s and Fv_c = Fv_s
  Tags = cuter_cerl:get_tags(Anno),
  eval({named, Mod_c, Fv_c}, CAs, SAs, find_call_type(M, Mod_c), Servers, Tags#tags.this, Fd);

%% c_case
eval_expr({c_case, _Anno, Arg, Clauses}, M, Cenv, Senv, Servers, Fd) ->
  {Arg_c, Arg_s} = eval_expr(Arg, M, Cenv, Senv, Servers, Fd),
  {Body, Ce, Se, _Cnt} = find_clause(Clauses, M, 'case', Arg_c, Arg_s, Cenv, Senv, Servers, Fd),
  cuter_log:reduce_constraint_counter(),  %% Should also add this call to c_receive
  eval_expr(Body, M, Ce, Se, Servers, Fd);

%% c_catch
%% Commented code: allow the exceptions to propagate
eval_expr({c_catch, _Anno, Body}, M, Cenv, Senv, Servers, Fd) ->
  try
    eval_expr(Body, M, Cenv, Senv, Servers, Fd)
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
%%eval_expr({c_catch, _Anno, Body}, M, Cenv, Senv, Servers, Fd) ->
%%  eval_expr(Body, M, Cenv, Senv, Servers, Fd);

%% c_cons
eval_expr({c_cons, _Anno, Hd, Tl}, M, Cenv, Senv, Servers, Fd) ->
  {Hd_c, Hd_s} = eval_expr(Hd, M, Cenv, Senv, Servers, Fd),
  {Tl_c, Tl_s} = eval_expr(Tl, M, Cenv, Senv, Servers, Fd),
  Cv = [Hd_c | Tl_c],
  Sv = cuter_symbolic:cons(Hd_s, Tl_s, Cv, Fd),
  {Cv, Sv};

%% c_fun
eval_expr({c_fun, _Anno, Vars, Body}, M, Cenv, Senv, Servers, Fd) ->
  Arity = length(Vars),
  Lambda = make_fun(Vars, Body, M, Arity, Cenv, Senv, Servers, Fd),
  {Lambda, Lambda};

%% c_let
eval_expr({c_let, _Anno, Vars, Arg, Body}, M, Cenv, Senv, Servers, Fd) ->
  Deg = length(Vars),
  {Arg_c, Arg_s} = eval_expr(Arg, M, Cenv, Senv, Servers, Fd),
  case Deg of
    1 ->
      CAs = [Arg_c],
      SAs = [Arg_s];
    _ ->
      {valuelist, CAs, Deg} = Arg_c,
      {valuelist, SAs, Deg} = Arg_s
  end,
  Ce = cuter_env:bind_parameters(CAs, Vars, Cenv),
  Se = cuter_env:bind_parameters(SAs, Vars, Senv),
  eval_expr(Body, M, Ce, Se, Servers, Fd);

%% c_letrec
eval_expr({c_letrec, _Anno, Defs, Body}, M, Cenv, Senv, Servers, Fd) ->
  H = fun(F) -> fun() ->
    lists:foldl(
      fun({Func, Def}, {E_c, E_s}) ->
        %% F is a /0 function that will create 
        %% the necessary self-referenced environment
        LetRec = {letrec_func, {M, Def, F}},
        Ce = cuter_env:add_binding(Func#c_var.name, LetRec, E_c),
        Se = cuter_env:add_binding(Func#c_var.name, LetRec, E_s),
        {Ce, Se}
      end,
      {Cenv, Senv},
      Defs
    )
  end end,
  {NCe, NSe} = (y(H))(),
  eval_expr(Body, M, NCe, NSe, Servers, Fd);

%% c_literal
eval_expr({c_literal, _Anno, V}, _M, _Cenv, _Senv, _Servers, _Fd) ->
  {V, V};

%% c_primop
eval_expr({c_primop, _Anno, Name, Args}, M, Cenv, Senv, Servers, Fd) ->
  PrimOp = Name#c_literal.val,
  ZAs = [eval_expr(A, M, Cenv, Senv, Servers, Fd) || A <- Args],
  {CAs, SAs} = lists:unzip(ZAs),
  %% TODO need to record and implement more primops
  %% like 'bs_context_to_binary', 'bs_init_writable'
  Tag = cuter_cerl:empty_tag(),
  case PrimOp of
    raise ->
      [Class_c, Reason_c] = CAs,
      [_Class_s, Reason_s] = SAs,
      %% CONSTRAINT: Class_c = Class_s
      eval({named, erlang, Class_c}, [Reason_c], [Reason_s], external, Servers, Tag, Fd);
    match_fail ->
      [Cv] = CAs,
      [Sv] = SAs,
      eval({named, erlang, error}, [{badmatch, Cv}], [{badmatch, Sv}], external, Servers, Tag, Fd);
    _ ->
      exception(error, {primop_not_supported, PrimOp})
  end;

%% c_receive
eval_expr({c_receive, _Anno, Clauses, Timeout, Action}, M, Cenv, Senv, Servers, Fd) ->
  {CTimeout, STimeout} = eval_expr(Timeout, M, Cenv, Senv, Servers, Fd),
  true = check_timeout(CTimeout, STimeout, Fd),
  Start = os:timestamp(),  %% Start time of timeout timer
  {messages, Mailbox} = erlang:process_info(self(), messages),
  Message = find_message(Mailbox, Clauses, M, Cenv, Senv, Servers, Fd),
  case Message of
    {Msg, Body, NCenv, NSenv, _Cnt} ->  %% Matched a message already in the mailbox
      receive Msg -> ok end,  %% Just consume the message
      eval_expr(Body, M, NCenv, NSenv, Servers, Fd);
    false ->  %% No mailbox message matched, thus need to enter a receive loop
      CurrMsgs = length(Mailbox),
      find_message_loop(Clauses, Action, CTimeout, STimeout, Start, CurrMsgs, M, Cenv, Senv, Servers, Fd)
  end;

%% c_seq
eval_expr({c_seq, _Anno, Arg, Body}, M, Cenv, Senv, Servers, Fd) ->
  _ = eval_expr(Arg, M, Cenv, Senv, Servers, Fd),
  eval_expr(Body, M, Cenv, Senv, Servers, Fd);

%% c_try
%% Commented code: allow the exceptions to propagate
eval_expr({c_try, _Anno, Arg, Vars, Body, Evars, Handler}, M, Cenv, Senv, Servers, Fd) ->
  try
    Deg = length(Vars),
    {A_c, A_s} = eval_expr(Arg, M, Cenv, Senv, Servers, Fd),
    case Deg of
      1 ->
        CAs = [A_c],
        SAs = [A_s];
      _ ->
        {valuelist, CAs, Deg} = A_c,
        {valuelist, SAs, Deg} = A_s
    end,
    Ce = cuter_env:bind_parameters(CAs, Vars, Cenv),
    Se = cuter_env:bind_parameters(SAs, Vars, Senv),
    eval_expr(Body, M, Ce, Se, Servers, Fd)
  catch
    Class:Reason ->
      {Cv, Sv} = unzip_one(Reason),
      {Cs, Ss} =
        case length(Evars) of
          3 -> {[Class, Cv, Class], [Class, Sv, Class]};
          2 -> {[Class, Cv], [Class, Sv]}
        end,
      ECe = cuter_env:bind_parameters(Cs, Evars, Cenv),
      ESe = cuter_env:bind_parameters(Ss, Evars, Senv),
      eval_expr(Handler, M, ECe, ESe, Servers, Fd)
  end;
%%eval_expr({c_try, _Anno, Arg, Vars, Body, _Evars, _Handler}, M, Cenv, Senv, Servers, Fd) ->
%%  Deg = length(Vars),
%%  {A_c, A_s} = eval_expr(Arg, M, Cenv, Senv, Servers, Fd),
%%  case Deg of
%%    1 ->
%%      CAs = [A_c],
%%      SAs = [A_s];
%%    _ ->
%%      {valuelist, CAs, Deg} = A_c,
%%      {valuelist, SAs, Deg} = A_s
%%  end,
%%  Ce = cuter_env:bind_parameters(CAs, Vars, Cenv),
%%  Se = cuter_env:bind_parameters(SAs, Vars, Senv),
%%  eval_expr(Body, M, Ce, Se, Servers, Fd);

%% c_tuple
eval_expr({c_tuple, _Anno, Es}, M, Cenv, Senv, Servers, Fd) ->
  Zes = [eval_expr(E, M, Cenv, Senv, Servers, Fd) || E <- Es],
  {Es_c, Es_s} = lists:unzip(Zes),
  Cv = list_to_tuple(Es_c),
  Sv = cuter_symbolic:make_tuple(Es_s, Cv, Fd),
  {Cv, Sv};

%% c_values
eval_expr({c_values, _Anno, Es}, M, Cenv, Senv, Servers, Fd) ->
  Deg = length(Es),
  Zes = [eval_expr(E, M, Cenv, Senv, Servers, Fd) || E <- Es],
  {Es_c, Es_s} = lists:unzip(Zes),
  {#valuelist{values=Es_c, degree=Deg}, #valuelist{values=Es_s, degree=Deg}};

%% c_var
eval_expr({c_var, _Anno, Name}, _M, Cenv, Senv, _Servers, _Fd) when is_tuple(Name) ->
  %% If Name is a function
  case cuter_env:get_value(Name, Cenv) of
    {ok, {letrec_func, {Mod, Def, E}}} ->
      %% fun bound in a letrec
      {Fun, Arity} = Name,
      R = {letrec_func, {Mod, Fun, Arity, Def, E}},
      {R, R};
    {ok, Closure} when is_function(Closure) ->
      %% closure
      {ok, Sv} = cuter_env:get_value(Name, Senv),
      {Closure, Sv};
    error ->
      %% either local in module or external
      R = {?FUNCTION_PREFIX, Name},
      {R, R}
  end;
eval_expr({c_var, _Anno, Name}, _M, Cenv, Senv, _Servers, _Fd) ->
  %% If it's a variable then return its value
  {ok, Cv} = cuter_env:get_value(Name, Cenv),
  {ok, Sv} = cuter_env:get_value(Name, Senv),
  {Cv, Sv};

eval_expr(Cerl, _M, _Cenv, _Senv, _Servers, _Fd) ->
  exception(error, {unknown_cerl, Cerl}).


%% --------------------------------------------------------
%% Evaluates a BIF call
%% --------------------------------------------------------
-spec evaluate_bif(mfa(), [any()], [any()], servers(), cuter_cerl:tag(), file:io_device()) -> result().
evaluate_bif(MFA, CAs, SAs, Servers, Tag, Fd) ->
  Cv = evaluate_bif_concrete(MFA, CAs, Servers#svs.code, Tag),
  Sv = cuter_symbolic:evaluate_mfa(MFA, SAs, Cv, Servers#svs.code, Tag, Fd),
  {Cv, Sv}.

%% Concrete evaluation of a BIF.
evaluate_bif_concrete({M, F, _A}=MFA, As, CodeServer, Tag) ->
  case cuter_symbolic:is_supported_mfa(MFA) of
    false -> apply(M, F, As);
    true ->
      try
        apply(M, F, As)
      catch
        error:E ->
          visit_tag(CodeServer, Tag),
          erlang:error({E, MFA, As})
      end
  end.

%% --------------------------------------------------------
%% Try to retrieve the code of an MFA
%% --------------------------------------------------------
-spec access_mfa_code(mfa(), servers()) -> {cerl:c_fun(), boolean()} | error.
access_mfa_code({Mod, Fun, Arity}, Servers) ->
  case cuter_mock:simulate_behaviour(Mod, Fun, Arity) of
    bif -> error;
    {ok, {M, _F, Arity} = MFA} ->
      case get_module_cache(M, Servers#svs.code) of
        {error, _} -> error;
        {ok, Cache} -> {M, retrieve_function_code(MFA, Cache)}
      end
  end.

%% --------------------------------------------------------
%% Interact with the CodeServer and ask for the ETS Table
%% where the code of the module M is stored.
%%
%% Optimization : For caching purposes, the MDb is stored
%% in the process dictionary for subsequent lookups
%% --------------------------------------------------------
-spec get_module_cache(atom(), pid()) -> {ok, cuter_codeserver:module_cache()} | {error, (preloaded | cover_compiled)}.
get_module_cache(M, CodeServer) ->
  What = {?CONCOLIC_PREFIX_PDICT, M},
  case get(What) of
    undefined ->
      case cuter_codeserver:load(CodeServer, M) of
        %% Module Code loaded
        {ok, Cache} = Ok -> put(What, Cache), Ok;
        %% Preloaded Module
        {error, preloaded} = E -> E;
        %% Cover Compiled Module
        {error, cover_compiled} = E -> E;
        %% Invalid Module
        {error, non_existing} -> exception(error, {undef, M});
        %% Any Error during Code Loading
        {error, Error} -> exception(error, Error)
      end;
    MDb -> {ok, MDb}
  end.

%% --------------------------------------------------------
%% Retrieves the code and exported type of an MFA
%%
%% Optimization : For caching purposes, the function 
%% definition is stored in the process dictionary for 
%% subsequent lookups
%% --------------------------------------------------------
-spec retrieve_function_code(mfa(), cuter_codeserver:module_cache()) -> {cerl:c_fun(), boolean()}.
retrieve_function_code(MFA, Cache) ->
  What = {?CONCOLIC_PREFIX_PDICT, MFA},
  case get(What) of
    undefined ->
      case cuter_codeserver:lookup_in_module_cache(MFA, Cache) of
        error -> exception(error, {undef, MFA});
        {ok, Val} -> put(What, Val), Val
      end;
    Val -> Val
  end.

%% --------------------------------------------------------
%% Raises the desired exception.
%% --------------------------------------------------------
-spec exception(class(), term()) -> no_return().
exception(Class, Reason) ->
  erlang:Class(Reason).

%% --------------------------------------------------------
%% find_message_loop
%%
%% Enters a loop waiting for a message that will match.
%% Wraps calls to run_message_loop to check for timeout.
%% --------------------------------------------------------
find_message_loop(Clauses, Action, infinity, STimetout, Start, Msgs, M, Cenv, Senv, Servers, Fd) ->
  run_message_loop(Clauses, Action, infinity, STimetout, Start, Msgs, M, Cenv, Senv, Servers, Fd);
find_message_loop(Clauses, Action, CTimeout, STimeout, Start, Msgs, M, Cenv, Senv, Servers, Fd) ->
  Now = os:timestamp(),
  Passed = timer:now_diff(Now, Start) / 1000,
  case Passed >= CTimeout of
    true ->
      eval_expr(Action, M, Cenv, Senv, Servers, Fd);
    false ->
      run_message_loop(Clauses, Action, CTimeout, STimeout, Start, Msgs, M, Cenv, Senv, Servers, Fd)
  end.

%% --------------------------------------------------------
%% run_message_looop
%%
%% Implements the actual waiting receive loop
%% --------------------------------------------------------
run_message_loop(Clauses, Action, CTimeout, STimeout, Start, Msgs, M, Cenv, Senv, Servers, Fd) ->
  erlang:yield(),
  {message_queue_len, CurrMsgs} = erlang:process_info(self(), message_queue_len),
  %% New messages will appended at the end of the mailbox
  case CurrMsgs > Msgs of
    false -> %% No new messages
      find_message_loop(Clauses, Action, CTimeout, STimeout, Start, Msgs, M, Cenv, Senv, Servers, Fd);
    true ->
      {messages, Mailbox} = erlang:process_info(self(), messages),
      NewMsgs = lists:nthtail(Msgs, Mailbox),
      Message = find_message(NewMsgs, Clauses, M, Cenv, Senv, Servers, Fd),
      case Message of
        false ->
          find_message_loop(Clauses, Action, CTimeout, STimeout, Start, CurrMsgs, M, Cenv, Senv, Servers, Fd);
        {Msg, Body, NCenv, NSenv, _Cnt} ->
          receive Msg -> ok end,  %% Just consume the matched message
          eval_expr(Body, M, NCenv, NSenv, Servers, Fd)
      end
  end.

%% --------------------------------------------------------
%% find_message
%%
%% Wraps calls to find_clause when trying to match 
%% a message against a series of patterns
%% --------------------------------------------------------
find_message([], _Clauses, _M, _Cenv, _Senv, _Servers, _Fd) ->
  false;

find_message([Msg|Mailbox], Clauses, M, Cenv, Senv, Servers, Fd) ->
  {LoggerFun, {Cv, Sv}} = decode_msg(Msg, Fd),
  case find_clause(Clauses, M, 'receive', Cv, Sv, Cenv, Senv, Servers, Fd) of
    false ->
      find_message(Mailbox, Clauses, M, Cenv, Senv, Servers, Fd);
    {Body, NCenv, NSenv, Cnt} ->
      log_successful_msg_match(LoggerFun),
      %% I can log the received Msg here
      {Msg, Body, NCenv, NSenv, Cnt}
  end.

%% Log the successful matching of a message
log_successful_msg_match(withoutLogger) -> ok;
log_successful_msg_match({withLogger, Fun}) -> Fun().

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
find_clause(Clauses, M, Mode, Cv, Sv, Cenv, Senv, Servers, Fd) ->
  find_clause(Clauses, M, Mode, Cv, Sv, Cenv, Senv, Servers, Fd, 1).

find_clause([], _M, _Mode, _Cv, _Sv, _Cenv, _Senv, _Servers, _Fd, _Cnt) ->
  false;
find_clause([Cl|Cls], M, Mode, Cv, Sv, Cenv, Senv, Servers, Fd, Cnt) ->
  Match = match_clause(Cl, M, Mode, Cv, Sv, Cenv, Senv, Servers,  Fd, Cnt),
  case Match of
    false ->
      find_clause(Cls, M, Mode, Cv, Sv, Cenv, Senv, Servers, Fd, Cnt+1);
    {true, {_Body, _NCenv, _NSenv, Cnt} = Matched} ->
      Matched
  end.

%% --------------------------------------------------------
%% match_clause
%%
%% Match a pair of concrete & symbolic values against
%% a specific clause (i.e. with patterns and guard)
%% --------------------------------------------------------
match_clause({c_clause, Anno, Pats, Guard, Body}, M, Mode, Cv, Sv, Cenv, Senv, Servers, Fd, Cnt) ->
  case is_patlist_compatible(Pats, Cv) of
    false -> false;
    true ->
      Deg = length(Pats),
      case Deg of
        1 ->
          Cs = [Cv],
          Ss = [Sv];
        _ ->
          {valuelist, Cs, Degree} = Cv,
          {valuelist, Ss, Degree} = Sv
      end,
      %% BitInfo is needed for parameterized bit-syntax patterns
      BitInfo = {M, Cenv, Senv},
      Ss_e = cuter_symbolic:ensure_list(Ss, length(Cs), Fd),
      Match = pattern_match_all(Pats, BitInfo, Mode, Cs, Ss_e, Servers, Fd),
      case Match of
        false -> false;
        {true, {CMs, SMs}} ->
          Ce = cuter_env:add_mappings_to_environment(CMs, Cenv),
          Se = cuter_env:add_mappings_to_environment(SMs, Senv),
          %% Make silent guards
          Tags = cuter_cerl:get_tags(Anno),
          try eval_expr(Guard, M, Ce, Se, Servers, Fd) of
            {true, SGv} ->
              %% CONSTRAINT: SGv is a True guard
              visit_tag(Servers#svs.code, Tags#tags.this),
              cuter_log:log_guard(Fd, true, SGv, Tags#tags.next),
              {true, {Body, Ce, Se, Cnt}};
            {false, SGv} ->
              %% CONSTRAINT: SGv is a False guard
              visit_tag(Servers#svs.code, Tags#tags.next),
              cuter_log:log_guard(Fd, false, SGv, Tags#tags.this),
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

pattern_match_all(Pats, BitInfo, Mode, Cs, Ss, Servers, Fd) ->
  pattern_match_all(Pats, BitInfo, Mode, Cs, Ss, [], [], Servers, Fd).

pattern_match_all([], _BitInfo, _Mode, [], [], CMaps, SMaps, _Servers, _Fd) ->
  {true, {CMaps, SMaps}};
pattern_match_all([P|Ps], BitInfo, Mode, [Cv|Cvs], [Sv|Svs], CMaps, SMaps, Servers, Fd) ->
  Match = pattern_match(P, BitInfo, Mode, Cv, Sv, CMaps, SMaps, Servers, Fd),
  case Match of
    false -> false;
    {true, {CMs, SMs}} ->
      pattern_match_all(Ps, BitInfo, Mode, Cvs, Svs, CMs, SMs, Servers, Fd)
  end.

%% --------------------------------------------------------
%% pattern_match
%%
%% Pattern match a pair of concrete & symbolic values
%% against a single pattern
%% --------------------------------------------------------

%% AtomicLiteral pattern
pattern_match({c_literal, Anno, LitVal}, _Bitinfo, _Mode, Cv, Sv, CMaps, SMaps, Servers, Fd) ->
  Tags = cuter_cerl:get_tags(Anno),
  case LitVal =:= Cv of
    true ->
      %% CONSTRAINT: Sv =:= Litval
      visit_tag(Servers#svs.code, Tags#tags.this),
      log_literal_match_success(Fd, LitVal, Sv, Tags#tags.next),
      {true, {CMaps, SMaps}};
    false ->
      %% CONSTRAINT: Sv =/= Litval
      visit_tag(Servers#svs.code, Tags#tags.next),
      log_literal_match_failure(Fd, LitVal, Sv, Tags#tags.this),
      false
  end;

%% VariableName pattern
pattern_match({c_var, _Anno, Name}, _BitInfo, _Mode, Cv, Sv, CMaps, SMaps, _Servers, _Fd) ->
  CMs = [{Name, Cv}|CMaps],
  SMs = [{Name, Sv}|SMaps],
  {true, {CMs, SMs}};

%% Tuple pattern
pattern_match({c_tuple, Anno, Es}, BitInfo, Mode, Cv, Sv, CMaps, SMaps, Servers, Fd) when is_tuple(Cv) ->
  Ne = length(Es),
  Tags = cuter_cerl:get_tags(Anno),
  case tuple_size(Cv) of
    Ne ->
      Cv_l = tuple_to_list(Cv),
      %% CONSTRAINT: Sv is a tuple of Ne elements
      visit_tag(Servers#svs.code, Tags#tags.this),
      cuter_log:log_tuple(Fd, sz, Sv, Ne, Tags#tags.next),
      Sv_l = cuter_symbolic:tpl_to_list(Sv, Ne, Fd),
      pattern_match_all(Es, BitInfo, Mode, Cv_l, Sv_l, CMaps, SMaps, Servers, Fd);
    _ ->
      %% CONSTRAINT: Sv is a tuple of not Ne elements
      visit_tag(Servers#svs.code, Tags#tags.next),
      cuter_log:log_tuple(Fd, not_sz, Sv, Ne, Tags#tags.this),
      false
  end;
pattern_match({c_tuple, Anno, Es}, _BitInfo, _Mode, _Cv, Sv, _CMaps, _SMaps, Servers, Fd) ->
  Ne = length(Es),
  Tags = cuter_cerl:get_tags(Anno),
  %% CONSTRAINT: Sv is not a tuple
  visit_tag(Servers#svs.code, Tags#tags.next),
  cuter_log:log_tuple(Fd, not_tpl, Sv, Ne, Tags#tags.this),
  false;

%% List constructor pattern
pattern_match({c_cons, Anno, _Hd, _Tl}, _BitInfo, _Mode, [], Sv, _CMaps, _SMaps, Servers, Fd) ->
  Tags = cuter_cerl:get_tags(Anno),
  %% CONSTRAINT: Sv is an empty list
  visit_tag(Servers#svs.code, Tags#tags.next),
  cuter_log:log_list(Fd, empty, Sv, Tags#tags.this),
  false;
pattern_match({c_cons, Anno, Hd, Tl}, BitInfo, Mode, [Cv|Cvs], Sv, CMaps, SMaps, Servers, Fd) ->
  Tags = cuter_cerl:get_tags(Anno),
  %% CONSTRAINT: S is a non empty list
  visit_tag(Servers#svs.code, Tags#tags.this),
  cuter_log:log_list(Fd, nonempty, Sv, Tags#tags.next),
  Sv_h = cuter_symbolic:head(Sv, Fd),
  Sv_t = cuter_symbolic:tail(Sv, Fd),
  case pattern_match(Hd, BitInfo, Mode, Cv, Sv_h, CMaps, SMaps, Servers, Fd) of
    false -> false;
    {true, {CMs, SMs}} -> pattern_match(Tl, BitInfo, Mode, Cvs, Sv_t, CMs, SMs, Servers, Fd)
  end;
pattern_match({c_cons, Anno, _Hd, _Tl}, _BitInfo, _Mode, _Cv, Sv, _CMaps, _SMaps, Servers, Fd) ->
  Tags = cuter_cerl:get_tags(Anno),
  %% CONSTRAINT: Sv is not a list
  visit_tag(Servers#svs.code, Tags#tags.next),
  cuter_log:log_list(Fd, not_lst, Sv, Tags#tags.this),
  false;

%% Alias pattern
pattern_match({c_alias, _Anno, Var, Pat}, BitInfo, Mode, Cv, Sv, CMaps, SMaps, Servers, Fd) ->
  Match = pattern_match(Pat, BitInfo, Mode, Cv, Sv, CMaps, SMaps, Servers, Fd),
  case Match of
    false -> false;
    {true, {CMs, SMs}} ->
      VName = Var#c_var.name,
      Cs = [{VName, Cv} | CMs],
      Ss = [{VName, Sv} | SMs],
      {true, {Cs, Ss}}
  end;

%% Binary pattern
pattern_match({c_binary, _Anno, Segments}, BitInfo, Mode, Cv, Sv, CMaps, SMaps, Servers, Fd) ->
  bit_pattern_match(Segments, BitInfo, Mode, Cv, Sv, CMaps, SMaps, Servers, Fd).

%% --------------------------------------------------------
%% bit_pattern_match
%% 
%% --------------------------------------------------------

bit_pattern_match([], _BitInfo, _Mode, <<>>, Sv, CMaps, SMaps, _Servers, Fd) ->
  %% CONSTRAINT: Sv =:= <<>>
  cuter_log:log_equal(Fd, true, <<>>, Sv, cuter_cerl:empty_tag()),
  {true, {CMaps, SMaps}};
bit_pattern_match([], _BitInfo, _Mode, _Cv, Sv, _CMaps, _SMaps, _Servers, Fd) ->
  %% CONSTRAINT: Sv =/= <<>>
  cuter_log:log_equal(Fd, false, <<>>, Sv, cuter_cerl:empty_tag()),
  false;

bit_pattern_match([{c_bitstr, Anno, {c_literal, _, LVal}, Sz, Unit, Tp, Fgs}|Bs], {M, Cenv, Senv} = Bnfo, Mode, Cv, Sv, CMaps, SMaps, Svs, Fd) ->
  {Size_c, Size_s} = eval_expr(Sz, M, Cenv, Senv, Svs, Fd),
  {Unit_c, Unit_s} = eval_expr(Unit, M, Cenv, Senv, Svs, Fd),
  {Type_c, Type_s} = eval_expr(Tp, M, Cenv, Senv, Svs, Fd),
  {Flags_c, Flags_s} = eval_expr(Fgs, M, Cenv, Senv, Svs, Fd),
  Enc_s = {Size_s, Unit_s, Type_s, Flags_s},
  Tags = cuter_cerl:get_tags(Anno),
  try cuter_binlib:match_bitstring_const(LVal, Size_c, Unit_c, Type_c, Flags_c, Cv) of
    Rest_c ->
      visit_tag(Svs#svs.code, Tags#tags.this),
      Rest_s = cuter_symbolic:match_bitstring_const_true(LVal, Enc_s, Sv, Rest_c, Size_c, Tags#tags.next, Fd),
      bit_pattern_match(Bs, Bnfo, Mode,  Rest_c, Rest_s, CMaps, SMaps, Svs, Fd)
  catch
    error:_e ->
      visit_tag(Svs#svs.code, Tags#tags.next),
      cuter_symbolic:match_bitstring_const_false(LVal, Enc_s, Sv, Size_c, Tags#tags.this, Fd),
      false
  end;

bit_pattern_match([{c_bitstr, Anno, {c_var, _, VarName}, Sz, Unit, Tp, Fgs}|Bs], {M, Cenv, Senv}, Mode, Cv, Sv, CMaps, SMaps, Svs, Fd) ->
  {Size_c, Size_s} = eval_expr(Sz, M, Cenv, Senv, Svs, Fd),
  {Unit_c, Unit_s} = eval_expr(Unit, M, Cenv, Senv, Svs, Fd),
  {Type_c, Type_s} = eval_expr(Tp, M, Cenv, Senv, Svs, Fd),
  {Flags_c, Flags_s} = eval_expr(Fgs, M, Cenv, Senv, Svs, Fd),
  Enc_s = {Size_s, Unit_s, Type_s, Flags_s},
  Tags = cuter_cerl:get_tags(Anno),
  try cuter_binlib:match_bitstring_var(Size_c, Unit_c, Type_c, Flags_c, Cv) of
    {X_c, Rest_c} ->
      visit_tag(Svs#svs.code, Tags#tags.this),
      {X_s, Rest_s} = cuter_symbolic:match_bitstring_var_true(Enc_s, Sv, X_c, Rest_c, Size_c, Tags#tags.next, Fd),
      {CMs, SMs} =
        case lists:keymember(VarName, 1, CMaps) of
          true ->
            {lists:keyreplace(VarName, 1, CMaps, {VarName, X_c}),
             lists:keyreplace(VarName, 1, SMaps, {VarName, X_s})};
          false ->
            {[{VarName, X_c} | CMaps],
             [{VarName, X_s} | SMaps]}
        end,
      NCenv = cuter_env:add_binding(VarName, X_c, Cenv),
      NSenv = cuter_env:add_binding(VarName, X_s, Senv),
      bit_pattern_match(Bs, {M, NCenv, NSenv}, Mode, Rest_c, Rest_s, CMs, SMs, Svs, Fd)
  catch
    error:_E ->
      visit_tag(Svs#svs.code, Tags#tags.next),
      cuter_symbolic:match_bitstring_var_false(Enc_s, Sv, Size_c, Tags#tags.this, Fd),
      false
  end.

%% --------------------------------------------------------
%% Wrap calls to make_fun/8 when creating a closure to be
%% passed as an argument to a function call
%% --------------------------------------------------------

%% Create a Closure of a local function
create_closure(M, F, Arity, local, Servers, Fd) ->
  %% Module is already loaded since create_closure is called by eval_expr
  {NM, {Def, _Exported}} = access_mfa_code({M, F, Arity}, Servers),
  Cenv = cuter_env:new_environment(),
  Senv = cuter_env:new_environment(),
  make_fun(Def#c_fun.vars, Def#c_fun.body, NM, Arity, Cenv, Senv, Servers, Fd);

%% Create a Closure when the MFA is a function bound in a letrec
create_closure(M, _F, Arity, {letrec_func, {Def, Cenv, Senv}}, Servers, Fd) ->
  make_fun(Def#c_fun.vars, Def#c_fun.body, M, Arity, Cenv, Senv, Servers, Fd).

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
make_fun(Vars, Body, Mod, Arity, Cenv, Senv, Servers, Fd) ->
  Creator = self(),
  case Arity of
    0 ->
      fun() ->
        make_fun_h1(Mod, [], Servers, Vars, Body, Cenv, Senv, Creator, Fd)
      end;
    1 ->
      fun(A) ->
        Args = [A],
        make_fun_h1(Mod, Args, Servers, Vars, Body, Cenv, Senv, Creator, Fd)
      end;
    2 ->
      fun(A, B) ->
        Args = [A, B],
        make_fun_h1(Mod, Args, Servers, Vars, Body, Cenv, Senv, Creator, Fd)
      end;
    3 ->
      fun(A, B, C) ->
        Args = [A, B, C],
        make_fun_h1(Mod, Args, Servers, Vars, Body, Cenv, Senv, Creator, Fd)
      end;
    4 ->
      fun(A, B, C, D) ->
        Args = [A, B, C, D],
        make_fun_h1(Mod, Args, Servers, Vars, Body, Cenv, Senv, Creator, Fd)
      end;
    5 ->
      fun(A, B, C, D, E) ->
        Args = [A, B, C, D, E],
        make_fun_h1(Mod, Args, Servers, Vars, Body, Cenv, Senv, Creator, Fd)
      end;
    6 ->
      fun(A, B, C, D, E, F) ->
        Args = [A, B, C, D, E, F],
        make_fun_h1(Mod, Args, Servers, Vars, Body, Cenv, Senv, Creator, Fd)
      end;
    7 ->
      fun(A, B, C, D, E, F, G) ->
        Args = [A, B, C, D, E, F, G],
        make_fun_h1(Mod, Args, Servers, Vars, Body, Cenv, Senv, Creator, Fd)
      end;
    8 ->
      fun(A, B, C, D, E, F, G, H) ->
        Args = [A, B, C, D, E, F, G, H],
        make_fun_h1(Mod, Args, Servers, Vars, Body, Cenv, Senv, Creator, Fd)
      end;
    9 ->
      fun(A, B, C, D, E, F, G, H, I) ->
        Args = [A, B, C, D, E, F, G, H, I],
        make_fun_h1(Mod, Args, Servers, Vars, Body, Cenv, Senv, Creator, Fd)
      end;
    10 ->
      fun(A, B, C, D, E, F, G, H, I, J) ->
        Args = [A, B, C, D, E, F, G, H, I, J],
        make_fun_h1(Mod, Args, Servers, Vars, Body, Cenv, Senv, Creator, Fd)
      end;
    11 ->
      fun(A, B, C, D, E, F, G, H, I, J, K) ->
        Args = [A, B, C, D, E, F, G, H, I, J, K],
        make_fun_h1(Mod, Args, Servers, Vars, Body, Cenv, Senv, Creator, Fd)
      end;
    12 ->
      fun(A, B, C, D, E, F, G, H, I, J, K, L) ->
        Args = [A, B, C, D, E, F, G, H, I, J, K, L],
        make_fun_h1(Mod, Args, Servers, Vars, Body, Cenv, Senv, Creator, Fd)
      end;
    13 ->
      fun(A, B, C, D, E, F, G, H, I, J, K, L, M) ->
        Args = [A, B, C, D, E, F, G, H, I, J, K, L, M],
        make_fun_h1(Mod, Args, Servers, Vars, Body, Cenv, Senv, Creator, Fd)
      end;
    14 ->
      fun(A, B, C, D, E, F, G, H, I, J, K, L, M, N) ->
        Args = [A, B, C, D, E, F, G, H, I, J, K, L, M, N],
        make_fun_h1(Mod, Args, Servers, Vars, Body, Cenv, Senv, Creator, Fd)
      end;
    15 ->
      fun(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) ->
        Args = [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O],
        make_fun_h1(Mod, Args, Servers, Vars, Body, Cenv, Senv, Creator, Fd)
      end;
    _ ->
      exception(error, {over_lambda_fun_argument_limit, Arity})
  end.

make_fun_h1(Mod, Args, Servers, Vars, Body, Cenv, Senv, Creator, FileDescr) ->
  {Ce, Se} = register_new_environments(Args, Vars, Cenv, Senv),
  NSvs = validate_servers(Servers),
  Fd = validate_file_descriptor(NSvs#svs.monitor, Creator, FileDescr),
  eval_expr(Body, Mod, Ce, Se, NSvs, Fd).

register_new_environments([], _Vars, Cenv, Senv) ->
  {Cenv, Senv};
register_new_environments(Args, Vars, Cenv, Senv) ->
  {CAs, SAs} = unzip_args(Args),
  Ce = cuter_env:bind_parameters(CAs, Vars, Cenv),
  Se = cuter_env:bind_parameters(SAs, Vars, Senv),
  {Ce, Se}.


%% Creates a closure from an MFA (emulates the behaviour
%% of erlang:make_fun/3) 
make_fun(Mod, Func, Arity, Servers, Tag, Fd) ->
  Creator = self(),
  case Arity of
    0 ->
      fun() ->
        make_fun_h(Mod, Func, [], Servers, Creator, Tag, Fd)
      end;
    1 ->
      fun(A) ->
        Args = [A],
        make_fun_h(Mod, Func, Args, Servers, Creator, Tag, Fd)
      end;
    2 ->
      fun(A, B) ->
        Args = [A, B],
        make_fun_h(Mod, Func, Args, Servers, Creator, Tag, Fd)
      end;
    3 ->
      fun(A, B, C) ->
        Args = [A, B, C],
        make_fun_h(Mod, Func, Args, Servers, Creator, Tag, Fd)
      end;
    4 ->
      fun(A, B, C, D) ->
        Args = [A, B, C, D],
        make_fun_h(Mod, Func, Args, Servers, Creator, Tag, Fd)
      end;
    5 ->
      fun(A, B, C, D, E) ->
        Args = [A, B, C, D, E],
        make_fun_h(Mod, Func, Args, Servers, Creator, Tag, Fd)
      end;
    6 ->
      fun(A, B, C, D, E, F) ->
        Args = [A, B, C, D, E, F],
        make_fun_h(Mod, Func, Args, Servers, Creator, Tag, Fd)
      end;
    7 ->
      fun(A, B, C, D, E, F, G) ->
        Args = [A, B, C, D, E, F, G],
        make_fun_h(Mod, Func, Args, Servers, Creator, Tag, Fd)
      end;
    8 ->
      fun(A, B, C, D, E, F, G, H) ->
        Args = [A, B, C, D, E, F, G, H],
        make_fun_h(Mod, Func, Args, Servers, Creator, Tag, Fd)
      end;
    9 ->
      fun(A, B, C, D, E, F, G, H, I) ->
        Args = [A, B, C, D, E, F, G, H, I],
        make_fun_h(Mod, Func, Args, Servers, Creator, Tag, Fd)
      end;
    10 ->
      fun(A, B, C, D, E, F, G, H, I, J) ->
        Args = [A, B, C, D, E, F, G, H, I, J],
        make_fun_h(Mod, Func, Args, Servers, Creator, Tag, Fd)
      end;
    11 ->
      fun(A, B, C, D, E, F, G, H, I, J, K) ->
        Args = [A, B, C, D, E, F, G, H, I, J, K],
        make_fun_h(Mod, Func, Args, Servers, Creator, Tag, Fd)
      end;
    12 ->
      fun(A, B, C, D, E, F, G, H, I, J, K, L) ->
        Args = [A, B, C, D, E, F, G, H, I, J, K, L],
        make_fun_h(Mod, Func, Args, Servers, Creator, Tag, Fd)
      end;
    13 ->
      fun(A, B, C, D, E, F, G, H, I, J, K, L, M) ->
        Args = [A, B, C, D, E, F, G, H, I, J, K, L, M],
        make_fun_h(Mod, Func, Args, Servers, Creator, Tag, Fd)
      end;
    14 ->
      fun(A, B, C, D, E, F, G, H, I, J, K, L, M, N) ->
        Args = [A, B, C, D, E, F, G, H, I, J, K, L, M, N],
        make_fun_h(Mod, Func, Args, Servers, Creator, Tag, Fd)
      end;
    15 ->
      fun(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) ->
        Args = [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O],
        make_fun_h(Mod, Func, Args, Servers, Creator, Tag, Fd)
      end;
    _ ->
      exception(error, {over_lambda_fun_argument_limit, Arity})
  end.

make_fun_h(Mod, Func, Args, Servers, Creator, Tag, FileDescr) ->
  {CAs, SAs} = unzip_args(Args), %% If Args =:= [] then unzip_args([]) will return {[], []}
  NSvs = validate_servers(Servers),
  Fd = validate_file_descriptor(NSvs#svs.monitor, Creator, FileDescr),
  eval({named, Mod, Func}, CAs, SAs, external, NSvs, Tag, Fd).

%% --------------------------------------------------------
%% Adjust the arguments of a function
%% --------------------------------------------------------

%% slave:start/{1,2,3}, slave:start_link/{1,2,3}
%% A new slave node will be given the path to the ebin of the tool
adjust_arguments(slave, F, CAs, SAs, Fd) when F =:= start; F =:= start_link ->
  Arity = length(CAs),
  SAs_e = cuter_symbolic:ensure_list(SAs, Arity, Fd),
  Ebin = " -pa " ++ ?EBIN,
  case CAs of
    [Host] ->
      Name = hd(string:tokens(atom_to_list(node()), "@")),
      [SHost] = SAs_e,
      {[Host, Name, Ebin], [SHost, Name, Ebin]};
    [Host, Name] ->
      [SHost, SName] = SAs_e,
      {[Host, Name, Ebin], [SHost, SName, Ebin]};
    [Host, Name, Args] ->
      %% Also add the path to the symbolic Args ???
      {[Host, Name, Args ++ Ebin], SAs_e};
    _ -> {CAs, SAs_e} %% Do not the arguments of the other calls
  end;
%% erlang:register/1
%% Append the unique execution prefix to the registered name
adjust_arguments(erlang, register, [RegName, PidOrPort], SAs, _Fd) ->
  {[append_execution_prefix(RegName), PidOrPort], SAs};
%% erlang:unregister/1
%% Append the unique execution prefix to the registered name
adjust_arguments(erlang, unregister, [RegName], SAs, _Fd) ->
  {[append_execution_prefix(RegName)], SAs};
%% All other functions will have their arguments unaltered
adjust_arguments(_M, _F, CAs, SAs, _Fd) -> {CAs, SAs}.


%% Adjust the target of a message by
%% appending the unique execution prefix to the registered name
adjust_message_destination(Dest) ->
  case is_atom(Dest) of
   false -> Dest;
   true  -> append_execution_prefix(Dest)
  end.

%% Append the unique execution prefix to a registered name
append_execution_prefix(Name) ->
  case get(?EXECUTION_PREFIX) of
    undefined ->
      exception(exit, {undefined_execution_prefix, self()});
    Prefix ->
      S = Prefix ++ atom_to_list(Name),
      list_to_atom(S)
  end.

%% --------------------------------------------------------
%% Ensures compatibility between the type of the call
%% and the exported status of the MFA
%% --------------------------------------------------------
-spec check_exported(boolean(), calltype(), mfa()) -> ok.
check_exported(true, _CallType, _MFA) -> ok;
check_exported(false, local, _MFA) -> ok;
check_exported(false, external, MFA) -> exception(error, {not_exported, MFA}).

%% --------------------------------------------------------
%% Check if the given servers correspond to the current 
%% node. If not, get the pids of the proper servers.
%% Needed when a closure is created in node A and is 
%% executed in node B.
%% --------------------------------------------------------
validate_servers(Servers) ->
  Node = node(),
  case Node =:= node(Servers#svs.monitor) of
    true  -> Servers;
    false -> cuter_monitor:node_servers(Servers#svs.monitor, Node)
  end.

%% --------------------------------------------------------
%% Check if the given file descriptor correponds to this 
%% process. If not get the proper file descriptor.
%% Needed when a closure is created in process A and is
%% executed in process B.
%% --------------------------------------------------------
validate_file_descriptor(MonitorServer, Pid, Fd) ->
  case Pid =:= self() of
    true  -> Fd;
    false -> cuter_monitor:file_descriptor(MonitorServer)
  end.

%% -------------------------------------------------------------------
%% Initializations called when a new process is spawned.
%% *  Register the parent process to the monitor server of the node
%% *  Open a new file to store the process's log data
%% *  Then proceed with interpreting the MFA call
%% -------------------------------------------------------------------
subscribe_and_apply(MonitorServer, Parent, Args, Ref) ->
  fun() ->
    {ok, Fd} = cuter_monitor:subscribe(MonitorServer, Parent),
    cuter_log:log_spawned(Fd, Parent, Ref),
    Parent ! {self(), registered},
    erlang:apply(?MODULE, eval, Args ++ [Fd]),
    cuter_log:close_file(Fd)
  end.

%% --------------------------------------------------------
%% Resolve the type of an MFA call
%% --------------------------------------------------------
find_call_type(_M, _M)   -> local;
find_call_type(_M1, _M2) -> external.

%% --------------------------------------------------------
%% Encode and Decode Msgs
%% --------------------------------------------------------

%% Encode a Message
encode_msg(MonitorServer, Dest, CMsg, SMsg, Fd) ->
  Ref = erlang:make_ref(),
  P = cuter_lib:ensure_port_or_pid(Dest),
  case cuter_monitor:is_monitored(MonitorServer, P) of
    false -> CMsg;
    true  ->
      cuter_log:log_message_sent(Fd, P, Ref),
      {?CONCOLIC_PREFIX_MSG, self(), Ref, zip_one(CMsg, SMsg)}
  end.

%% Decode a Message
decode_msg({?CONCOLIC_PREFIX_MSG, From, Ref, Msg}, Fd) when is_pid(From), is_reference(Ref) ->
  cuter_log:log_message_received(Fd, From, Ref),
  Fun = fun() -> cuter_log:log_message_consumed(Fd, From, Ref) end,
  {{withLogger, Fun}, unzip_msg(Msg)};
decode_msg(Msg, _Fd) ->
  {withoutLogger, unzip_msg(Msg)}.

%% --------------------------------------------------------
%% Zip and Unzip concrete-semantic values
%%
%% A zipped value is a {?ZIPPED_VALUE_PREFIX, CVal, SVal}.
%% Zipped values are used when sending messages, raising 
%% exceptions and  passing arguments to closures.
%% --------------------------------------------------------

%% zip_one
zip_one(Cv, Sv) -> {?ZIPPED_VALUE_PREFIX, Cv, Sv}.

%% unzip_one
unzip_one({?ZIPPED_VALUE_PREFIX, Cv, Sv}) -> {Cv, Sv};
unzip_one(V) -> {V, V}.

%% zip_args
zip_args(CAs, SAs) when is_list(CAs), is_list(SAs) ->
  lists:zipwith(fun zip_one/2, CAs, SAs).

%% unzip_args
unzip_args(As) when is_list(As) ->
   lists:unzip([unzip_one(A) || A <- As]).

%% unzip_error // for exception reasons
-spec unzip_error(term()) -> result().
unzip_error({nocatch, {?ZIPPED_VALUE_PREFIX, Cv, Sv}}) ->
  {Cv, Sv};
unzip_error({{?ZIPPED_VALUE_PREFIX, Cv, Sv}, Stack}) when is_list(Stack) ->
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
%% Calculates if the number of patterns in a clause is 
%% compatible to the numbers of actual values that are 
%% to be matched against
%% --------------------------------------------------------
is_patlist_compatible(Pats, Values) ->
  case {length(Pats), Values} of
    {1, {valuelist, _Vals, _N}} -> false;
    {1, _Val} -> true;
    {N, {valuelist, _Vals, N}} -> true;
    {_N, _Val} -> false
  end.

%% --------------------------------------------------------
%% Concatenate a list of bistrings
%% --------------------------------------------------------
append_segments([], [], _Fd) ->
  {<<>>, <<>>};
append_segments([Cv], [Sv], _Fd) ->
  {Cv, Sv};
append_segments(Cs, Ss, Fd) ->
  [Cv|RCs] = lists:reverse(Cs),
  [Sv|RSs] = lists:reverse(Ss),
  append_segments(RCs, Cv, RSs, Sv, Fd).

append_segments([], CAcc, [], SAcc, _Fd) ->
  {CAcc, SAcc};
append_segments([Cv|Cs], CAcc, [Sv|Ss], SAcc, Fd) ->
  case not cuter_symbolic:is_symbolic(Sv) andalso not cuter_symbolic:is_symbolic(SAcc) of
    true ->
      append_segments(Cs, <<Cv/bitstring, CAcc/bitstring>>, Ss, <<Sv/bitstring, SAcc/bitstring>>, Fd);
    false ->
      case cuter_symbolic:is_symbolic(Sv) of
        false ->
          Bits = split_concrete_segment(Cv, []),
          SAcc1 = cuter_symbolic:concat_segments(Bits, SAcc, Fd),
          append_segments(Cs, <<Cv/bitstring, CAcc/bitstring>>, Ss, SAcc1, Fd);
        true ->
          Bits = split_symbolic_segment(Cv, Sv, [], Fd),
          SAcc1 = cuter_symbolic:concat_segments(Bits, SAcc, Fd),
          append_segments(Cs, <<Cv/bitstring, CAcc/bitstring>>, Ss, SAcc1, Fd)
      end
  end.

split_concrete_segment(Seg, Acc) ->
  case Seg of
    <<>> -> lists:reverse(Acc);
    <<B:1, Bits/bitstring>> -> split_concrete_segment(Bits, [B|Acc])
  end.

split_symbolic_segment(<<>>, Sv, Acc, Fd) ->
  cuter_log:log_empty_bitstring(Fd, Sv),
  lists:reverse(Acc);
split_symbolic_segment(<<_:1, Bits/bitstring>>, Sv, Acc, Fd) ->
  cuter_log:reduce_constraint_counter(),
  {SB, SBits} = cuter_symbolic:non_empty_binary(Sv, Fd),
  split_symbolic_segment(Bits, SBits, [SB|Acc], Fd).

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
  exception(error, {invalid_timeout, Timeout}).

%% --------------------------------------------------------
%% Y combinator for a function with arity 0
%% --------------------------------------------------------
y(M) ->
  G = fun(F) -> M(fun() -> (F(F))() end) end,
  G(G).

%% --------------------------------------------------------
%% Reports visiting a tag.
%% --------------------------------------------------------
-spec visited_tags() -> cuter_cerl:visited_tags().
visited_tags() ->
  case get(?VISITED_TAGS_PREFIX) of
    undefined -> gb_sets:new();
    Tags -> Tags
  end.

-spec visit_tag(pid(), cuter_cerl:tag()) -> ok.
visit_tag(CodeServer, Tag) ->
  TagIDs = visited_tags(),
  TagID = cuter_cerl:id_of_tag(Tag),
  case TagID =:= ?EMPTY_TAG_ID orelse gb_sets:is_element(TagID, TagIDs) of
    true -> ok;
    false ->
      cuter_codeserver:visit_tag(CodeServer, Tag),
      put(?VISITED_TAGS_PREFIX, gb_sets:insert(TagID, TagIDs)),
      ok
  end.

%% --------------------------------------------------------
%% Logs the matching of literals.
%% --------------------------------------------------------

%% Creates appropriate logs when a matching with a literal succeeded.
log_literal_match_success(Fd, Lit, Sv, Tag) ->
  case cuter_symbolic:is_symbolic(Sv) of
    true  -> cuter_log:log_equal(Fd, true, Lit, Sv, Tag);
    false -> log_literal_match_success_rec(Fd, Lit, Sv, Tag)
  end.

log_literal_match_success_rec(Fd, [X|Xs], [Y|Ys], Tag) ->
  log_literal_match_success(Fd, X, Y, Tag),
  log_literal_match_success(Fd, Xs, Ys, Tag);
log_literal_match_success_rec(Fd, Lit, Sv, Tag) when is_tuple(Lit), is_tuple(Sv) ->
  Xs = erlang:tuple_to_list(Lit),
  Ys = erlang:tuple_to_list(Sv),
  log_literal_match_success(Fd, Xs, Ys, Tag);
log_literal_match_success_rec(Fd, Lit, Sv, Tag) ->
  cuter_log:log_equal(Fd, true, Lit, Sv, Tag).

%% Creates appropriate logs when a matching with a literal failed.
log_literal_match_failure(Fd, Lit, Sv, Tag) ->
  case cuter_symbolic:is_symbolic(Sv) of
    true  -> cuter_log:log_equal(Fd, false, Lit, Sv, Tag);
    false -> log_literal_match_failure_rec(Fd, Lit, Sv, Tag)
  end.

log_literal_match_failure_rec(Fd, [X|Xs], [X|Ys], Tag) ->
  log_literal_match_failure(Fd, Xs, Ys, Tag);
log_literal_match_failure_rec(Fd, [X|Xs], [Y|Ys], Tag) ->
  log_literal_match_failure(Fd, X, Y, Tag),
  log_literal_match_failure(Fd, Xs, Ys, Tag);
log_literal_match_failure_rec(Fd, Lit, Sv, Tag) when is_tuple(Lit), is_tuple(Sv) ->
  Xs = erlang:tuple_to_list(Lit),
  Ys = erlang:tuple_to_list(Sv),
  log_literal_match_failure(Fd, Xs, Ys, Tag);
log_literal_match_failure_rec(Fd, Lit, Sv, Tag) ->
  cuter_log:log_equal(Fd, false, Lit, Sv, Tag).
