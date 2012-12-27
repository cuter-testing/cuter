%% if degree = 1
%%   value :: term()
%%   degree :: non_neg_integer()
%% else if degree > 1
%%   value :: [#semantic{}]
%%   degree :: non_neg_integer()
-record(semantic, {
  value,
  degree
}).

