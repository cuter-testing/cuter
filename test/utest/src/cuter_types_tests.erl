%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_types_tests).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> ok | {error | term()}. %% Silence dialyzer warning

-spec parse_basic_types_test_() -> term().
parse_basic_types_test_() ->
  Is = [types_and_specs],
  Setup = fun(I) -> fun() -> setup(I) end end,
  Cleanup = fun cleanup/1,
  Inst = fun parse_types/1,
  [{"Parse basic types", {setup, Setup(I), Cleanup, Inst}} || I <- Is].

parse_types({types_and_specs, Attrs}) ->
  {TypeAttrs, _} = cuter_cerl:classify_attributes(Attrs),
  Types = cuter_types:retrieve_types(TypeAttrs),
  Ts = [
    {"t1()"
    , {type, t1, 0}
    , { cuter_types:t_atom()
       , []
      }
    },
    {"t2()"
    , {type, t2, 0}
    , { cuter_types:t_tuple([cuter_types:t_integer(), cuter_types:t_float(), cuter_types:t_tuple()])
      , []
      }
    },
    {"t3()"
    , {type, t3, 0}
    , { cuter_types:t_union([cuter_types:t_any(), cuter_types:t_nil()])
      , []
      }
    },
    {"t4()"
    , {type, t4, 0}
    , { cuter_types:t_list(cuter_types:t_union([cuter_types:t_list(), cuter_types:t_bitstring()]))
      , []
      }
    },
    {"t5()"
    , {type, t5, 0}
    , { cuter_types:t_tuple([
          cuter_types:t_binary(), cuter_types:t_nonempty_list(cuter_types:t_number()),
          cuter_types:t_string(), cuter_types:t_char()
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
