-module(types_and_specs).

-export([foo/0, f11/1, f12/1]).

-export_type([t1/0, t2/0, t3/0, t4/0, t5/0, t7/0, t8/0, t9/0]).

-type t1() :: atom().
-type t2() :: {integer(), float(), tuple()}.
-type t3() :: any() | nil().
-type t4() :: [list() | bitstring()].
-type t5() :: {binary(), [number(), ...], string(), char()}.
-type t6() :: types_and_specs2:e1(float()).
-type t7() :: <<_:64>> | <<_:_*3>> | <<_:128, _:_*12>>.
-type t8() :: map().
-type t9() :: #{atom() => list(integer()), float() := float()}.

-spec foo() -> ok.
foo() -> ok.

-spec f11(t6()) -> float().
f11(X) -> X + 3.14.

-spec f12(X) -> float() when X :: t6().
f12(X) -> X + 3.14.
