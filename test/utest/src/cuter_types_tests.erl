%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_types_tests).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> 'ok' | {'error', term()}.  %% This should be provided by EUnit

-spec parse_basic_types_test_() -> [{string(), {'setup', fun(), fun(), fun()}}].
parse_basic_types_test_() ->
  Is = [types_and_specs],
  Setup = fun(I) -> fun() -> setup(I) end end,
  Cleanup = fun cleanup/1,
  Inst = fun parse_types/1,
  [{"Parse basic types", {setup, Setup(I), Cleanup, Inst}} || I <- Is].

parse_types({types_and_specs, Attrs}) ->
  RecordForms = cuter_cerl:extract_record_forms(Attrs),
  TypeForms = cuter_cerl:extract_type_forms(Attrs),
  Types = cuter_types:retrieve_types(TypeForms, RecordForms),
  Ts = [
    {"t1()"
    , {type, t1, 0}
    , {cuter_types:t_atom(), []}
    },
    {"t2()"
    , {type, t2, 0}
    , {cuter_types:t_tuple([cuter_types:t_integer(), cuter_types:t_float(), cuter_types:t_tuple()])
      , []
      }
    },
    {"t3()"
    , {type, t3, 0}
    , {cuter_types:t_union([cuter_types:t_any(), cuter_types:t_nil()])
      , []
      }
    },
    {"t4()"
    , {type, t4, 0}
    , {cuter_types:t_list(cuter_types:t_union([cuter_types:t_list(), cuter_types:t_bitstring()]))
      , []
      }
    },
    {"t5()"
    , {type, t5, 0}
    , {cuter_types:t_tuple([
         cuter_types:t_binary(), cuter_types:t_nonempty_list(cuter_types:t_number()),
         cuter_types:t_string(), cuter_types:t_char()
       ])
      , []
      }
    },
    {"t7()"
    , {type, t7, 0}
    , {cuter_types:t_union([
         cuter_types:t_bitstring(64, 0), cuter_types:t_bitstring(0, 3), cuter_types:t_bitstring(128, 12)
        ])
      , []
      }
    },
    {"t8()"
    , {type, t8, 0}
    , {cuter_types:t_map(), []}
    },
    {"t9()"
    , {type, t9, 0}
    , {cuter_types:t_map([
        {map_field_assoc, cuter_types:t_atom(), cuter_types:t_list(cuter_types:t_integer())},
        {map_field_exact, cuter_types:t_float(), cuter_types:t_float()}
        ])
      , []
    }
    }
  ],
  [{Txt, ?_assertEqual(Expected, dict:fetch(Key, Types))} || {Txt, Key, Expected} <- Ts].

setup(Mod) ->
  Attrs = cuter_tests_lib:get_module_attrs(Mod, true),
  {Mod, Attrs}.

cleanup(_) -> ok.

-spec convert_types_test() -> 'ok'.
convert_types_test() ->
  Modules = [examples_for_spec_conversion, examples_for_spec_conversion_pair],
  Fn = fun(M) -> {ok, AST} = cuter_cerl:get_core(M, false), AST end,
  Xs = [{M, Fn(M)} || M <- Modules],
  TagGen = fun() -> ok end,
  Kmodules = [cuter_cerl:kmodule(M, AST, TagGen) || {M, AST} <- Xs],
  Specs = cuter_types:specs_as_erl_types(Kmodules),
  Expect = spec_conversion_tests(),
  lists:foreach(fun (E) -> spec_assertions(E, Specs) end, Expect),
  ExpectMfas = lists:sort([Mfa || {Mfa, _Spec} <- Expect]),
  GotMfas = lists:sort(dict:fetch_keys(Specs)),
  ?assertEqual(ExpectMfas, GotMfas),
  ok.

spec_assertions({Mfa, Expect}, R) ->
  case dict:find(Mfa, R) of
    error ->
      Comment = cuter_types:mfa_to_string(Mfa) ++ " should exist",
      ?assert(dict:is_key(Mfa, R), Comment);
    {ok, Got} ->
      Comment = "Spec of " ++ cuter_types:mfa_to_string(Mfa),
      ?assertEqual(Expect, Got, Comment)
  end.

spec_conversion_tests() ->
  T_Animal = erl_types:t_atoms([cat, dog]),
  T_Point = erl_types:t_tuple([erl_types:t_atom(point), 
			       erl_types:t_number(), erl_types:t_number()]),
  [
    {
      {examples_for_spec_conversion, id, 1},
      [erl_types:t_fun([erl_types:t_any()], erl_types:t_any())]
    },
    {
      {examples_for_spec_conversion, inc, 1},
      [erl_types:t_fun([erl_types:t_integer()], erl_types:t_integer())]
    },
    {
      {examples_for_spec_conversion, to_atom, 1},
      [erl_types:t_fun(
	 [erl_types:t_sup(erl_types:t_integer(), erl_types:t_atom())],
	 erl_types:t_atom())]
    },
    {
      {examples_for_spec_conversion, translate, 3},
      [erl_types:t_fun(
        [T_Point, erl_types:t_number(), erl_types:t_number()],
        T_Point)]
    },
    {
      {examples_for_spec_conversion, root, 1},
      []  %% FIX: We do not support recursive types.
    },
    {
      {examples_for_spec_conversion, max_x, 1},
      [erl_types:t_fun([erl_types:t_list(T_Point)], erl_types:t_number())]
    },
    {
      {examples_for_spec_conversion, is_dog, 1},
      [erl_types:t_fun([T_Animal], erl_types:t_boolean())]
    },
    {
      {examples_for_spec_conversion_pair, to_int, 1},
      [erl_types:t_fun(
        [erl_types:t_sup(erl_types:t_integer(), erl_types:t_atom())],
        erl_types:t_integer())]
    },
    {
      {examples_for_spec_conversion_pair, can_bark, 1},
      [erl_types:t_fun([erl_types:t_list(T_Animal)], erl_types:t_boolean())]
    },
    {
      {examples_for_spec_conversion_pair, count_trees, 1},
      []  %% FIX: We do not support mutually recursive declarations in bounded funs.
    },
    {
      {examples_for_spec_conversion_pair, tree_height, 1},
      []  %% FIX: We do not support recursive declarations in bounded funs.
    }
  ].
