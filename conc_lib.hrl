%% Used to represent list of values for Core Erlang interpretation
-record(valuelist, {
  values, %% [term()]
  degree  %% non_neg_integer()
}).
