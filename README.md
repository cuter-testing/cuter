conc-test-ref
=============

c(rush), c(foo), c(conc), c(conc_cserver), c(conc_load), c(conc_eval), c(conc_tserver), c(conc_lib).
c(rush), c(foo), c(conc,[native]), c(conc_cserver,[native]), c(conc_load,[native]), c(conc_eval,[native]), c(conc_tserver,[native]), c(conc_lib,[native]).
conc:start(foo,mymin,[[a,b,c]]).
conc:start(foo,spawn,[lists,reverse,[[1,2,3]]]).
conc:start(rush, solve, [6,6,{2,0},[{0,{2,3},{2,4}},{1,{0,0},{2,0}},{2,{4,0},{4,1}},{3,{5,1},{5,3}},{4,{1,2},{3,2}},{5,{0,4},{0,5}},{6,{1,5},{3,5}},{7,{4,5},{5,5}}]]).
conc:start(rush,solve,[6,6,{5,2},[{0,{0,2},{1,2}},{1,{0,3},{1,3}},{2,{2,3},{3,3}},{3,{1,4},{2,4}},{4,{3,4},{3,5}},{5,{4,5},{5,5}},{6,{5,3},{5,4}},{7,{4,2},{4,4}},{8,{2,0},{3,0}},{9,{2,1},{3,1}},{10,{4,0},{5,0}},{11,{4,1},{5,1}}]]).

errors
------
{undef, M}
{cover_compiled, M}
{undef, FuncKey}
{not_exported, MFA}
{lambda_fun_argument_limit, Arity}
{invalid_timeout, Timeout}

