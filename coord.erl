-module(coord).
-compile(export_all).

run(M, F, As) ->
  process_flag(trap_exit, true),
  CoreDir = "core_temp",
  Concolic = init_concolic(M, F, As, CoreDir),
  receive
    {'EXIT', Concolic, Why} ->
      {error, Why};
    {Concolic, Results} ->
      Results
  end,
  process_flag(trap_exit, false).
