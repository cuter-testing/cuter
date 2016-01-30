-module(types_and_specs).

-export([foo/0]).

-export_type([t1/0, t2/0, t3/0, t4/0, t5/0]).

-type t1() :: atom().
-type t2() :: {integer(), float(), tuple()}.
-type t3() :: any() | nil().
-type t4() :: [list() | bitstring()].
-type t5() :: {binary(), [number(), ...], string(), char()}.

-spec foo() -> ok.
foo() -> ok.
