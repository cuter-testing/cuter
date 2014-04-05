%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_eval_tests).

-export([
  doDouble/1, doDouble2/1, doCons/1, doTuple/1, doLet/1,
  doLetRec/1, doBinary/0, doDoubleFun/1, doEcho/0, doReceive/1,
  lambdaEval/1, doBitMatch/0, doRegister/0
]).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_config.hrl").

-spec test() -> ok | {error | term()}. %% Silence dialyzer warning

-spec eval_cerl_test_() -> term().
eval_cerl_test_() ->
  Is = [
    {"Function application", {doDouble, [2], 4}},
    {"Intermodular call", {doDouble2, [2], 4}},
    {"Create List", {doCons, [2], [2,2]}},
    {"Create Tuple", {doTuple, [2], {2,2}}},
    {"Let definition", {doLet, [2], 5}},
    {"Letrec definition", {doLetRec, [[1,2,3]], [1,2,3]}},
    {"Create bitstring / binary", {doBinary, [], <<"42">>}},
    {"Create closure", {doDoubleFun, [2], 4}},
    {"Send / Receive messages", {doReceive, [ping], ok}},
    {"Pattern Mathing", {lambdaEval, [{{{$\\,x,{$\\,y,{$+,x,y}}},5},4}], 9}},
    {"Bit Pattern Matching", {doBitMatch, [], {42, <<"ok">>}}},
    {"Naming Processes", {doRegister, [], true}}
  ],
  Setup = fun(I) -> fun() -> setup(I) end end,
  Cleanup = fun cleanup/1,
  Inst = fun eval_cerl/1,
  [{"Basic Cerl Evaluation: " ++ C, {setup, Setup(I), Cleanup, Inst}} || {C, I} <- Is].

eval_cerl({F, As, Result, Dir}) ->
  Server = cuter_iserver:start(?MODULE, F, As, Dir, Dir, ?TRACE_DEPTH),
  R = execution_result(Server),
  ok = wait_for_iserver(Server),
  [{atom_to_list(F), ?_assertMatch({success, {Result, _}}, R)}].

execution_result(Server) -> 
  receive
    {Server, ExStatus, _Result} -> ExStatus
  end.

wait_for_iserver(Server) ->
  receive
    {'EXIT', Server, normal} -> ok 
  after 
    5000 -> ok 
  end.

setup({F, As, Result}) ->
  process_flag(trap_exit, true),
  Dir = cuter_tests_lib:setup_dir(),
  {F, As, Result, Dir}.

cleanup({_F, _As, _Result, Dir}) -> cuter_tests_lib:cleanup_dir(Dir).

%% --------------------------------------------------------
%% Tests
%% --------------------------------------------------------

%% Basic Cerl Evaluation

double(X) -> X + X.

-spec doDouble(number()) -> number().
doDouble(Y) -> double(Y).

-spec doDouble2(number()) -> number().
doDouble2(Y) -> ?MODULE:double(Y).

-spec doCons(any()) -> [any()].
doCons(X) -> [X, X].

-spec doTuple(any()) -> {any(), any()}.
doTuple(X) -> {X, X}.

-spec doLet(number()) -> number().
doLet(X) ->
  Y = X + X,
  Y + 1.

-spec doLetRec([any()]) -> [any()].
doLetRec(X) -> [Y || Y <- X].

-spec doBinary() -> binary().
doBinary() -> <<"42">>.

-spec doDoubleFun(number()) -> number().
doDoubleFun(X) ->
  F = fun(Y) -> double(Y) end,
  F(X).

-spec doEcho() -> any().
doEcho() -> receive {From, What} -> From ! {self(), What} end.

-spec doReceive(any()) -> ok.
doReceive(X) ->
  P = spawn(?MODULE, doEcho, []),
  P ! {self(), X},
  receive
    {P, X} -> ok
  end.

%% Simple Pattern Matching

lookupVar([], _Var) -> throw(free_variable);
lookupVar([{Var, Val}|_Env], Var) -> Val;
lookupVar([_|Env], Var) -> lookupVar(Env, Var).

-spec lambdaEval(any()) -> integer().
lambdaEval(Exp) -> lambdaEval(Exp, []).

lambdaEval({Fun, Exp}, Env) ->
  E = lambdaEval(Exp, Env),
  F = lambdaEval(Fun, Env),
  F(E);
lambdaEval({$\\, Var, Exp}, Env) ->
  fun(V) -> lambdaEval(Exp, [{Var, V}|Env]) end;
lambdaEval({$+, Exp1, Exp2}, Env) ->
  E1 = lambdaEval(Exp1, Env),
  E2 = lambdaEval(Exp2, Env),
  E1 + E2;
lambdaEval(Var, Env) when is_atom(Var)->
  lookupVar(Env, Var);
lambdaEval(Val, _Env) when is_integer(Val)->
  Val.

%% Bit Pattern Matching

-spec doBitMatch() -> {integer(), binary()}.
doBitMatch() ->
  Bin = <<"Answer", 42, "ok">>,
  <<"Answer", Int, Result/binary>> = Bin,
  {Int, Result}.

%% Naming Processes

-spec doRegister() -> true | ok.
doRegister() -> 
  register(me, self()),
  me ! hello,
  receive
    hello -> unregister(me)
  after
    1000 -> ok
  end.
  
