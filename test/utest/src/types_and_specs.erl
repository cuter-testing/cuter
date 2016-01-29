-module(types_and_specs).

-export([foo/0]).

-export_type([t1/0, t2/0]).

-type t1() :: atom().
-type t2() :: {integer(), float(), tuple()}.

-spec foo() -> ok.
foo() -> ok.
