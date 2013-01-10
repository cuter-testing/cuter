-module(conc_lib).

-include("conc_lib.hrl").

%% External exported functions
-export([new_environment/0, add_binding/3, is_bound/2, get_value/2,
  add_mappings_to_environment/2, is_bif/3, make_bitstring/5]).

%% External exported types
-export_type([environment/0, semantic_var/0, semantic_value/0]).

%% Type definitions
-type environment() :: orddict:orddict().
-type semantic_var() :: cerl:var_name().
-type semantic_value() :: term() | #valuelist{}.


%%====================================================================
%% External exports
%%====================================================================

%% Creates a new empty environment
-spec new_environment() -> environment().
new_environment() -> 
  orddict:new().
  
%% Adds a new binding to the environment
%% and returns the new environment
-spec add_binding(semantic_var(), semantic_value(), environment()) -> environment().
add_binding(Var, Val, Env) ->
  orddict:store(Var, Val, Env).
  
%% Checks if Var is bound in the environment
-spec is_bound(semantic_var(), environment()) -> boolean().
is_bound(Var, Environment) ->
  orddict:is_key(Var, Environment).
  
%% Gets the Value of a bound Variable
%% Returns {ok, Value} if Var is bound,
%% or error if Var is unbound.
-spec get_value(semantic_var(), environment()) -> semantic_value().
get_value(Var, Environment) ->
  try orddict:fetch(Var, Environment) of
    Val -> {ok, Val}
  catch
    error:_Error -> error
  end.
  
%% Add new mappings to environment
%% Mappings may be a deeply nested list
-spec add_mappings_to_environment([{semantic_var(), semantic_value()}], environment()) -> environment().
add_mappings_to_environment([], Env) ->
  Env;
add_mappings_to_environment([M | Ms], Env)
  when is_list(M) ->
    NEnv = add_mappings_to_environment(M, Env),
    add_mappings_to_environment(Ms, NEnv);
add_mappings_to_environment([{Var, Val} | Ms], Env) ->
  NEnv = add_binding(Var, Val, Env),
  add_mappings_to_environment(Ms, NEnv).
  
  
%% Returns true if an MFA is an Erlang BIF
%% Module erlang
is_bif(erlang, _F, _A)    -> true;
%% Module binary
is_bif(binary, compile_pattern, 1) -> true;
is_bif(binary, match, 2) -> true;
is_bif(binary, match, 3) -> true;
is_bif(binary, matches, 2) -> true;
is_bif(binary, matches, 3) -> true;
is_bif(binary, longest_common_prefix, 1) -> true;
is_bif(binary, longest_common_suffix, 1) -> true;
is_bif(binary, first, 1) -> true;
is_bif(binary, last, 1) -> true;
is_bif(binary, at, 2) -> true;
is_bif(binary, part, 2) -> true;
is_bif(binary, part, 3) -> true;
is_bif(binary, bin_to_list, 1) -> true;
is_bif(binary, bin_to_list, 2) -> true;
is_bif(binary, bin_to_list, 3) -> true;
is_bif(binary, list_to_bin, 1) -> true;
is_bif(binary, copy, 1) -> true;
is_bif(binary, copy, 2) -> true;
is_bif(binary, referenced_byte_size, 1) -> true;
is_bif(binary, decode_unsigned, 1) -> true;
is_bif(binary, decode_unsigned, 2) -> true;
%% Module lists
is_bif(lists, member, 2)  -> true;
is_bif(lists, reverse, 2) -> true;
is_bif(lists, keymember, 3) -> true;
is_bif(lists, keysearch, 3) -> true;
is_bif(lists, keyfind, 3) -> true;
%% Module net_kernel
is_bif(net_kernel, dflag_unicode_io, 1) -> true;
%% Rest are not BiFs
is_bif(_M, _F, _A) -> false.


get_signedness([unsigned | _Fls]) -> unsigned;
get_signedness([signed | _Fls]) -> signed;
get_signedness([_Fl | Fls]) -> get_signedness(Fls).

get_endianess([big | _Fls]) -> big;
get_endianess([little | _Fls]) -> little;
get_endianess([native | _Fls]) -> native;
get_endianess([_Fl | Fls]) -> get_endianess(Fls).
  
make_bitstring(Val, Size, Unit, Type, Flags) ->
  Sign = get_signedness(Flags),
  End = get_endianess(Flags),
  case {Size, Unit, Type, Sign, End} of
    {_, 1, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:1>>;
    {_, 2, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:2>>;
    {_, 3, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:3>>;
    {_, 4, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:4>>;
    {_, 5, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:5>>;
    {_, 6, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:6>>;
    {_, 7, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:7>>;
    {_, 8, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:8>>;
    {_, 9, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:9>>;
    {_, 10, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:10>>;
    {_, 11, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:11>>;
    {_, 12, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:12>>;
    {_, 13, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:13>>;
    {_, 14, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:14>>;
    {_, 15, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:15>>;
    {_, 16, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:16>>;
    {_, 17, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:17>>;
    {_, 18, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:18>>;
    {_, 19, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:19>>;
    {_, 20, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:20>>;
    {_, 21, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:21>>;
    {_, 22, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:22>>;
    {_, 23, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:23>>;
    {_, 24, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:24>>;
    {_, 25, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:25>>;
    {_, 26, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:26>>;
    {_, 27, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:27>>;
    {_, 28, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:28>>;
    {_, 29, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:29>>;
    {_, 30, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:30>>;
    {_, 31, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:31>>;
    {_, 32, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:32>>;
    {_, 33, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:33>>;
    {_, 34, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:34>>;
    {_, 35, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:35>>;
    {_, 36, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:36>>;
    {_, 37, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:37>>;
    {_, 38, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:38>>;
    {_, 39, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:39>>;
    {_, 40, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:40>>;
    {_, 41, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:41>>;
    {_, 42, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:42>>;
    {_, 43, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:43>>;
    {_, 44, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:44>>;
    {_, 45, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:45>>;
    {_, 46, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:46>>;
    {_, 47, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:47>>;
    {_, 48, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:48>>;
    {_, 49, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:49>>;
    {_, 50, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:50>>;
    {_, 51, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:51>>;
    {_, 52, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:52>>;
    {_, 53, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:53>>;
    {_, 54, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:54>>;
    {_, 55, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:55>>;
    {_, 56, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:56>>;
    {_, 57, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:57>>;
    {_, 58, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:58>>;
    {_, 59, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:59>>;
    {_, 60, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:60>>;
    {_, 61, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:61>>;
    {_, 62, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:62>>;
    {_, 63, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:63>>;
    {_, 64, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:64>>;
    {_, 65, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:65>>;
    {_, 66, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:66>>;
    {_, 67, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:67>>;
    {_, 68, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:68>>;
    {_, 69, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:69>>;
    {_, 70, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:70>>;
    {_, 71, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:71>>;
    {_, 72, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:72>>;
    {_, 73, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:73>>;
    {_, 74, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:74>>;
    {_, 75, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:75>>;
    {_, 76, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:76>>;
    {_, 77, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:77>>;
    {_, 78, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:78>>;
    {_, 79, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:79>>;
    {_, 80, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:80>>;
    {_, 81, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:81>>;
    {_, 82, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:82>>;
    {_, 83, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:83>>;
    {_, 84, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:84>>;
    {_, 85, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:85>>;
    {_, 86, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:86>>;
    {_, 87, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:87>>;
    {_, 88, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:88>>;
    {_, 89, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:89>>;
    {_, 90, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:90>>;
    {_, 91, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:91>>;
    {_, 92, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:92>>;
    {_, 93, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:93>>;
    {_, 94, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:94>>;
    {_, 95, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:95>>;
    {_, 96, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:96>>;
    {_, 97, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:97>>;
    {_, 98, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:98>>;
    {_, 99, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:99>>;
    {_, 100, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:100>>;
    {_, 101, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:101>>;
    {_, 102, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:102>>;
    {_, 103, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:103>>;
    {_, 104, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:104>>;
    {_, 105, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:105>>;
    {_, 106, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:106>>;
    {_, 107, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:107>>;
    {_, 108, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:108>>;
    {_, 109, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:109>>;
    {_, 110, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:110>>;
    {_, 111, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:111>>;
    {_, 112, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:112>>;
    {_, 113, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:113>>;
    {_, 114, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:114>>;
    {_, 115, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:115>>;
    {_, 116, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:116>>;
    {_, 117, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:117>>;
    {_, 118, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:118>>;
    {_, 119, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:119>>;
    {_, 120, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:120>>;
    {_, 121, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:121>>;
    {_, 122, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:122>>;
    {_, 123, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:123>>;
    {_, 124, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:124>>;
    {_, 125, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:125>>;
    {_, 126, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:126>>;
    {_, 127, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:127>>;
    {_, 128, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:128>>;
    {_, 129, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:129>>;
    {_, 130, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:130>>;
    {_, 131, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:131>>;
    {_, 132, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:132>>;
    {_, 133, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:133>>;
    {_, 134, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:134>>;
    {_, 135, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:135>>;
    {_, 136, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:136>>;
    {_, 137, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:137>>;
    {_, 138, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:138>>;
    {_, 139, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:139>>;
    {_, 140, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:140>>;
    {_, 141, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:141>>;
    {_, 142, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:142>>;
    {_, 143, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:143>>;
    {_, 144, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:144>>;
    {_, 145, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:145>>;
    {_, 146, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:146>>;
    {_, 147, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:147>>;
    {_, 148, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:148>>;
    {_, 149, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:149>>;
    {_, 150, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:150>>;
    {_, 151, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:151>>;
    {_, 152, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:152>>;
    {_, 153, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:153>>;
    {_, 154, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:154>>;
    {_, 155, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:155>>;
    {_, 156, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:156>>;
    {_, 157, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:157>>;
    {_, 158, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:158>>;
    {_, 159, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:159>>;
    {_, 160, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:160>>;
    {_, 161, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:161>>;
    {_, 162, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:162>>;
    {_, 163, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:163>>;
    {_, 164, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:164>>;
    {_, 165, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:165>>;
    {_, 166, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:166>>;
    {_, 167, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:167>>;
    {_, 168, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:168>>;
    {_, 169, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:169>>;
    {_, 170, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:170>>;
    {_, 171, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:171>>;
    {_, 172, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:172>>;
    {_, 173, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:173>>;
    {_, 174, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:174>>;
    {_, 175, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:175>>;
    {_, 176, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:176>>;
    {_, 177, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:177>>;
    {_, 178, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:178>>;
    {_, 179, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:179>>;
    {_, 180, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:180>>;
    {_, 181, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:181>>;
    {_, 182, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:182>>;
    {_, 183, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:183>>;
    {_, 184, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:184>>;
    {_, 185, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:185>>;
    {_, 186, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:186>>;
    {_, 187, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:187>>;
    {_, 188, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:188>>;
    {_, 189, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:189>>;
    {_, 190, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:190>>;
    {_, 191, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:191>>;
    {_, 192, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:192>>;
    {_, 193, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:193>>;
    {_, 194, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:194>>;
    {_, 195, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:195>>;
    {_, 196, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:196>>;
    {_, 197, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:197>>;
    {_, 198, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:198>>;
    {_, 199, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:199>>;
    {_, 200, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:200>>;
    {_, 201, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:201>>;
    {_, 202, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:202>>;
    {_, 203, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:203>>;
    {_, 204, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:204>>;
    {_, 205, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:205>>;
    {_, 206, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:206>>;
    {_, 207, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:207>>;
    {_, 208, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:208>>;
    {_, 209, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:209>>;
    {_, 210, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:210>>;
    {_, 211, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:211>>;
    {_, 212, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:212>>;
    {_, 213, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:213>>;
    {_, 214, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:214>>;
    {_, 215, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:215>>;
    {_, 216, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:216>>;
    {_, 217, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:217>>;
    {_, 218, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:218>>;
    {_, 219, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:219>>;
    {_, 220, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:220>>;
    {_, 221, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:221>>;
    {_, 222, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:222>>;
    {_, 223, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:223>>;
    {_, 224, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:224>>;
    {_, 225, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:225>>;
    {_, 226, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:226>>;
    {_, 227, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:227>>;
    {_, 228, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:228>>;
    {_, 229, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:229>>;
    {_, 230, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:230>>;
    {_, 231, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:231>>;
    {_, 232, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:232>>;
    {_, 233, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:233>>;
    {_, 234, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:234>>;
    {_, 235, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:235>>;
    {_, 236, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:236>>;
    {_, 237, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:237>>;
    {_, 238, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:238>>;
    {_, 239, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:239>>;
    {_, 240, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:240>>;
    {_, 241, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:241>>;
    {_, 242, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:242>>;
    {_, 243, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:243>>;
    {_, 244, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:244>>;
    {_, 245, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:245>>;
    {_, 246, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:246>>;
    {_, 247, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:247>>;
    {_, 248, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:248>>;
    {_, 249, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:249>>;
    {_, 250, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:250>>;
    {_, 251, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:251>>;
    {_, 252, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:252>>;
    {_, 253, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:253>>;
    {_, 254, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:254>>;
    {_, 255, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:255>>;
    {_, 256, integer, signed, big} ->
      <<Val:Size/signed-big-integer-unit:256>>;
    {_, 1, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:1>>;
    {_, 2, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:2>>;
    {_, 3, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:3>>;
    {_, 4, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:4>>;
    {_, 5, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:5>>;
    {_, 6, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:6>>;
    {_, 7, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:7>>;
    {_, 8, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:8>>;
    {_, 9, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:9>>;
    {_, 10, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:10>>;
    {_, 11, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:11>>;
    {_, 12, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:12>>;
    {_, 13, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:13>>;
    {_, 14, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:14>>;
    {_, 15, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:15>>;
    {_, 16, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:16>>;
    {_, 17, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:17>>;
    {_, 18, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:18>>;
    {_, 19, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:19>>;
    {_, 20, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:20>>;
    {_, 21, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:21>>;
    {_, 22, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:22>>;
    {_, 23, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:23>>;
    {_, 24, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:24>>;
    {_, 25, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:25>>;
    {_, 26, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:26>>;
    {_, 27, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:27>>;
    {_, 28, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:28>>;
    {_, 29, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:29>>;
    {_, 30, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:30>>;
    {_, 31, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:31>>;
    {_, 32, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:32>>;
    {_, 33, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:33>>;
    {_, 34, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:34>>;
    {_, 35, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:35>>;
    {_, 36, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:36>>;
    {_, 37, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:37>>;
    {_, 38, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:38>>;
    {_, 39, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:39>>;
    {_, 40, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:40>>;
    {_, 41, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:41>>;
    {_, 42, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:42>>;
    {_, 43, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:43>>;
    {_, 44, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:44>>;
    {_, 45, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:45>>;
    {_, 46, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:46>>;
    {_, 47, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:47>>;
    {_, 48, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:48>>;
    {_, 49, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:49>>;
    {_, 50, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:50>>;
    {_, 51, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:51>>;
    {_, 52, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:52>>;
    {_, 53, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:53>>;
    {_, 54, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:54>>;
    {_, 55, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:55>>;
    {_, 56, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:56>>;
    {_, 57, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:57>>;
    {_, 58, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:58>>;
    {_, 59, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:59>>;
    {_, 60, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:60>>;
    {_, 61, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:61>>;
    {_, 62, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:62>>;
    {_, 63, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:63>>;
    {_, 64, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:64>>;
    {_, 65, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:65>>;
    {_, 66, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:66>>;
    {_, 67, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:67>>;
    {_, 68, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:68>>;
    {_, 69, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:69>>;
    {_, 70, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:70>>;
    {_, 71, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:71>>;
    {_, 72, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:72>>;
    {_, 73, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:73>>;
    {_, 74, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:74>>;
    {_, 75, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:75>>;
    {_, 76, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:76>>;
    {_, 77, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:77>>;
    {_, 78, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:78>>;
    {_, 79, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:79>>;
    {_, 80, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:80>>;
    {_, 81, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:81>>;
    {_, 82, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:82>>;
    {_, 83, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:83>>;
    {_, 84, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:84>>;
    {_, 85, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:85>>;
    {_, 86, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:86>>;
    {_, 87, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:87>>;
    {_, 88, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:88>>;
    {_, 89, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:89>>;
    {_, 90, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:90>>;
    {_, 91, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:91>>;
    {_, 92, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:92>>;
    {_, 93, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:93>>;
    {_, 94, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:94>>;
    {_, 95, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:95>>;
    {_, 96, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:96>>;
    {_, 97, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:97>>;
    {_, 98, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:98>>;
    {_, 99, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:99>>;
    {_, 100, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:100>>;
    {_, 101, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:101>>;
    {_, 102, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:102>>;
    {_, 103, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:103>>;
    {_, 104, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:104>>;
    {_, 105, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:105>>;
    {_, 106, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:106>>;
    {_, 107, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:107>>;
    {_, 108, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:108>>;
    {_, 109, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:109>>;
    {_, 110, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:110>>;
    {_, 111, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:111>>;
    {_, 112, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:112>>;
    {_, 113, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:113>>;
    {_, 114, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:114>>;
    {_, 115, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:115>>;
    {_, 116, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:116>>;
    {_, 117, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:117>>;
    {_, 118, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:118>>;
    {_, 119, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:119>>;
    {_, 120, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:120>>;
    {_, 121, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:121>>;
    {_, 122, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:122>>;
    {_, 123, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:123>>;
    {_, 124, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:124>>;
    {_, 125, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:125>>;
    {_, 126, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:126>>;
    {_, 127, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:127>>;
    {_, 128, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:128>>;
    {_, 129, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:129>>;
    {_, 130, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:130>>;
    {_, 131, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:131>>;
    {_, 132, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:132>>;
    {_, 133, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:133>>;
    {_, 134, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:134>>;
    {_, 135, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:135>>;
    {_, 136, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:136>>;
    {_, 137, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:137>>;
    {_, 138, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:138>>;
    {_, 139, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:139>>;
    {_, 140, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:140>>;
    {_, 141, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:141>>;
    {_, 142, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:142>>;
    {_, 143, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:143>>;
    {_, 144, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:144>>;
    {_, 145, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:145>>;
    {_, 146, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:146>>;
    {_, 147, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:147>>;
    {_, 148, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:148>>;
    {_, 149, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:149>>;
    {_, 150, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:150>>;
    {_, 151, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:151>>;
    {_, 152, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:152>>;
    {_, 153, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:153>>;
    {_, 154, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:154>>;
    {_, 155, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:155>>;
    {_, 156, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:156>>;
    {_, 157, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:157>>;
    {_, 158, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:158>>;
    {_, 159, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:159>>;
    {_, 160, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:160>>;
    {_, 161, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:161>>;
    {_, 162, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:162>>;
    {_, 163, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:163>>;
    {_, 164, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:164>>;
    {_, 165, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:165>>;
    {_, 166, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:166>>;
    {_, 167, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:167>>;
    {_, 168, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:168>>;
    {_, 169, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:169>>;
    {_, 170, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:170>>;
    {_, 171, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:171>>;
    {_, 172, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:172>>;
    {_, 173, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:173>>;
    {_, 174, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:174>>;
    {_, 175, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:175>>;
    {_, 176, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:176>>;
    {_, 177, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:177>>;
    {_, 178, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:178>>;
    {_, 179, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:179>>;
    {_, 180, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:180>>;
    {_, 181, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:181>>;
    {_, 182, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:182>>;
    {_, 183, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:183>>;
    {_, 184, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:184>>;
    {_, 185, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:185>>;
    {_, 186, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:186>>;
    {_, 187, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:187>>;
    {_, 188, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:188>>;
    {_, 189, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:189>>;
    {_, 190, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:190>>;
    {_, 191, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:191>>;
    {_, 192, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:192>>;
    {_, 193, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:193>>;
    {_, 194, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:194>>;
    {_, 195, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:195>>;
    {_, 196, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:196>>;
    {_, 197, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:197>>;
    {_, 198, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:198>>;
    {_, 199, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:199>>;
    {_, 200, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:200>>;
    {_, 201, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:201>>;
    {_, 202, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:202>>;
    {_, 203, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:203>>;
    {_, 204, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:204>>;
    {_, 205, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:205>>;
    {_, 206, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:206>>;
    {_, 207, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:207>>;
    {_, 208, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:208>>;
    {_, 209, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:209>>;
    {_, 210, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:210>>;
    {_, 211, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:211>>;
    {_, 212, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:212>>;
    {_, 213, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:213>>;
    {_, 214, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:214>>;
    {_, 215, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:215>>;
    {_, 216, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:216>>;
    {_, 217, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:217>>;
    {_, 218, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:218>>;
    {_, 219, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:219>>;
    {_, 220, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:220>>;
    {_, 221, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:221>>;
    {_, 222, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:222>>;
    {_, 223, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:223>>;
    {_, 224, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:224>>;
    {_, 225, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:225>>;
    {_, 226, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:226>>;
    {_, 227, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:227>>;
    {_, 228, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:228>>;
    {_, 229, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:229>>;
    {_, 230, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:230>>;
    {_, 231, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:231>>;
    {_, 232, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:232>>;
    {_, 233, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:233>>;
    {_, 234, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:234>>;
    {_, 235, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:235>>;
    {_, 236, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:236>>;
    {_, 237, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:237>>;
    {_, 238, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:238>>;
    {_, 239, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:239>>;
    {_, 240, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:240>>;
    {_, 241, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:241>>;
    {_, 242, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:242>>;
    {_, 243, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:243>>;
    {_, 244, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:244>>;
    {_, 245, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:245>>;
    {_, 246, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:246>>;
    {_, 247, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:247>>;
    {_, 248, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:248>>;
    {_, 249, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:249>>;
    {_, 250, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:250>>;
    {_, 251, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:251>>;
    {_, 252, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:252>>;
    {_, 253, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:253>>;
    {_, 254, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:254>>;
    {_, 255, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:255>>;
    {_, 256, float, signed, big} ->
      <<Val:Size/signed-big-float-unit:256>>;
    {all, 1, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:1>>;
    {all, 2, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:2>>;
    {all, 3, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:3>>;
    {all, 4, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:4>>;
    {all, 5, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:5>>;
    {all, 6, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:6>>;
    {all, 7, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:7>>;
    {all, 8, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:8>>;
    {all, 9, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:9>>;
    {all, 10, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:10>>;
    {all, 11, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:11>>;
    {all, 12, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:12>>;
    {all, 13, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:13>>;
    {all, 14, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:14>>;
    {all, 15, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:15>>;
    {all, 16, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:16>>;
    {all, 17, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:17>>;
    {all, 18, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:18>>;
    {all, 19, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:19>>;
    {all, 20, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:20>>;
    {all, 21, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:21>>;
    {all, 22, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:22>>;
    {all, 23, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:23>>;
    {all, 24, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:24>>;
    {all, 25, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:25>>;
    {all, 26, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:26>>;
    {all, 27, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:27>>;
    {all, 28, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:28>>;
    {all, 29, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:29>>;
    {all, 30, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:30>>;
    {all, 31, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:31>>;
    {all, 32, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:32>>;
    {all, 33, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:33>>;
    {all, 34, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:34>>;
    {all, 35, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:35>>;
    {all, 36, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:36>>;
    {all, 37, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:37>>;
    {all, 38, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:38>>;
    {all, 39, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:39>>;
    {all, 40, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:40>>;
    {all, 41, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:41>>;
    {all, 42, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:42>>;
    {all, 43, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:43>>;
    {all, 44, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:44>>;
    {all, 45, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:45>>;
    {all, 46, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:46>>;
    {all, 47, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:47>>;
    {all, 48, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:48>>;
    {all, 49, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:49>>;
    {all, 50, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:50>>;
    {all, 51, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:51>>;
    {all, 52, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:52>>;
    {all, 53, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:53>>;
    {all, 54, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:54>>;
    {all, 55, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:55>>;
    {all, 56, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:56>>;
    {all, 57, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:57>>;
    {all, 58, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:58>>;
    {all, 59, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:59>>;
    {all, 60, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:60>>;
    {all, 61, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:61>>;
    {all, 62, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:62>>;
    {all, 63, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:63>>;
    {all, 64, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:64>>;
    {all, 65, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:65>>;
    {all, 66, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:66>>;
    {all, 67, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:67>>;
    {all, 68, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:68>>;
    {all, 69, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:69>>;
    {all, 70, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:70>>;
    {all, 71, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:71>>;
    {all, 72, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:72>>;
    {all, 73, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:73>>;
    {all, 74, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:74>>;
    {all, 75, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:75>>;
    {all, 76, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:76>>;
    {all, 77, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:77>>;
    {all, 78, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:78>>;
    {all, 79, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:79>>;
    {all, 80, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:80>>;
    {all, 81, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:81>>;
    {all, 82, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:82>>;
    {all, 83, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:83>>;
    {all, 84, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:84>>;
    {all, 85, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:85>>;
    {all, 86, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:86>>;
    {all, 87, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:87>>;
    {all, 88, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:88>>;
    {all, 89, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:89>>;
    {all, 90, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:90>>;
    {all, 91, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:91>>;
    {all, 92, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:92>>;
    {all, 93, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:93>>;
    {all, 94, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:94>>;
    {all, 95, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:95>>;
    {all, 96, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:96>>;
    {all, 97, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:97>>;
    {all, 98, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:98>>;
    {all, 99, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:99>>;
    {all, 100, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:100>>;
    {all, 101, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:101>>;
    {all, 102, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:102>>;
    {all, 103, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:103>>;
    {all, 104, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:104>>;
    {all, 105, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:105>>;
    {all, 106, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:106>>;
    {all, 107, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:107>>;
    {all, 108, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:108>>;
    {all, 109, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:109>>;
    {all, 110, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:110>>;
    {all, 111, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:111>>;
    {all, 112, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:112>>;
    {all, 113, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:113>>;
    {all, 114, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:114>>;
    {all, 115, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:115>>;
    {all, 116, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:116>>;
    {all, 117, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:117>>;
    {all, 118, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:118>>;
    {all, 119, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:119>>;
    {all, 120, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:120>>;
    {all, 121, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:121>>;
    {all, 122, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:122>>;
    {all, 123, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:123>>;
    {all, 124, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:124>>;
    {all, 125, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:125>>;
    {all, 126, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:126>>;
    {all, 127, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:127>>;
    {all, 128, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:128>>;
    {all, 129, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:129>>;
    {all, 130, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:130>>;
    {all, 131, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:131>>;
    {all, 132, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:132>>;
    {all, 133, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:133>>;
    {all, 134, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:134>>;
    {all, 135, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:135>>;
    {all, 136, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:136>>;
    {all, 137, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:137>>;
    {all, 138, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:138>>;
    {all, 139, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:139>>;
    {all, 140, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:140>>;
    {all, 141, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:141>>;
    {all, 142, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:142>>;
    {all, 143, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:143>>;
    {all, 144, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:144>>;
    {all, 145, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:145>>;
    {all, 146, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:146>>;
    {all, 147, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:147>>;
    {all, 148, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:148>>;
    {all, 149, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:149>>;
    {all, 150, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:150>>;
    {all, 151, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:151>>;
    {all, 152, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:152>>;
    {all, 153, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:153>>;
    {all, 154, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:154>>;
    {all, 155, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:155>>;
    {all, 156, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:156>>;
    {all, 157, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:157>>;
    {all, 158, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:158>>;
    {all, 159, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:159>>;
    {all, 160, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:160>>;
    {all, 161, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:161>>;
    {all, 162, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:162>>;
    {all, 163, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:163>>;
    {all, 164, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:164>>;
    {all, 165, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:165>>;
    {all, 166, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:166>>;
    {all, 167, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:167>>;
    {all, 168, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:168>>;
    {all, 169, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:169>>;
    {all, 170, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:170>>;
    {all, 171, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:171>>;
    {all, 172, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:172>>;
    {all, 173, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:173>>;
    {all, 174, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:174>>;
    {all, 175, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:175>>;
    {all, 176, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:176>>;
    {all, 177, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:177>>;
    {all, 178, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:178>>;
    {all, 179, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:179>>;
    {all, 180, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:180>>;
    {all, 181, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:181>>;
    {all, 182, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:182>>;
    {all, 183, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:183>>;
    {all, 184, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:184>>;
    {all, 185, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:185>>;
    {all, 186, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:186>>;
    {all, 187, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:187>>;
    {all, 188, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:188>>;
    {all, 189, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:189>>;
    {all, 190, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:190>>;
    {all, 191, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:191>>;
    {all, 192, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:192>>;
    {all, 193, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:193>>;
    {all, 194, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:194>>;
    {all, 195, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:195>>;
    {all, 196, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:196>>;
    {all, 197, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:197>>;
    {all, 198, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:198>>;
    {all, 199, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:199>>;
    {all, 200, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:200>>;
    {all, 201, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:201>>;
    {all, 202, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:202>>;
    {all, 203, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:203>>;
    {all, 204, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:204>>;
    {all, 205, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:205>>;
    {all, 206, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:206>>;
    {all, 207, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:207>>;
    {all, 208, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:208>>;
    {all, 209, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:209>>;
    {all, 210, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:210>>;
    {all, 211, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:211>>;
    {all, 212, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:212>>;
    {all, 213, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:213>>;
    {all, 214, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:214>>;
    {all, 215, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:215>>;
    {all, 216, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:216>>;
    {all, 217, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:217>>;
    {all, 218, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:218>>;
    {all, 219, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:219>>;
    {all, 220, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:220>>;
    {all, 221, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:221>>;
    {all, 222, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:222>>;
    {all, 223, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:223>>;
    {all, 224, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:224>>;
    {all, 225, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:225>>;
    {all, 226, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:226>>;
    {all, 227, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:227>>;
    {all, 228, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:228>>;
    {all, 229, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:229>>;
    {all, 230, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:230>>;
    {all, 231, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:231>>;
    {all, 232, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:232>>;
    {all, 233, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:233>>;
    {all, 234, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:234>>;
    {all, 235, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:235>>;
    {all, 236, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:236>>;
    {all, 237, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:237>>;
    {all, 238, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:238>>;
    {all, 239, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:239>>;
    {all, 240, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:240>>;
    {all, 241, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:241>>;
    {all, 242, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:242>>;
    {all, 243, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:243>>;
    {all, 244, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:244>>;
    {all, 245, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:245>>;
    {all, 246, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:246>>;
    {all, 247, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:247>>;
    {all, 248, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:248>>;
    {all, 249, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:249>>;
    {all, 250, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:250>>;
    {all, 251, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:251>>;
    {all, 252, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:252>>;
    {all, 253, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:253>>;
    {all, 254, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:254>>;
    {all, 255, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:255>>;
    {all, 256, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:256>>;
    {_, 1, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:1>>;
    {_, 2, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:2>>;
    {_, 3, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:3>>;
    {_, 4, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:4>>;
    {_, 5, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:5>>;
    {_, 6, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:6>>;
    {_, 7, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:7>>;
    {_, 8, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:8>>;
    {_, 9, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:9>>;
    {_, 10, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:10>>;
    {_, 11, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:11>>;
    {_, 12, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:12>>;
    {_, 13, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:13>>;
    {_, 14, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:14>>;
    {_, 15, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:15>>;
    {_, 16, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:16>>;
    {_, 17, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:17>>;
    {_, 18, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:18>>;
    {_, 19, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:19>>;
    {_, 20, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:20>>;
    {_, 21, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:21>>;
    {_, 22, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:22>>;
    {_, 23, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:23>>;
    {_, 24, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:24>>;
    {_, 25, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:25>>;
    {_, 26, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:26>>;
    {_, 27, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:27>>;
    {_, 28, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:28>>;
    {_, 29, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:29>>;
    {_, 30, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:30>>;
    {_, 31, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:31>>;
    {_, 32, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:32>>;
    {_, 33, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:33>>;
    {_, 34, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:34>>;
    {_, 35, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:35>>;
    {_, 36, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:36>>;
    {_, 37, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:37>>;
    {_, 38, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:38>>;
    {_, 39, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:39>>;
    {_, 40, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:40>>;
    {_, 41, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:41>>;
    {_, 42, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:42>>;
    {_, 43, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:43>>;
    {_, 44, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:44>>;
    {_, 45, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:45>>;
    {_, 46, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:46>>;
    {_, 47, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:47>>;
    {_, 48, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:48>>;
    {_, 49, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:49>>;
    {_, 50, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:50>>;
    {_, 51, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:51>>;
    {_, 52, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:52>>;
    {_, 53, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:53>>;
    {_, 54, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:54>>;
    {_, 55, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:55>>;
    {_, 56, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:56>>;
    {_, 57, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:57>>;
    {_, 58, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:58>>;
    {_, 59, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:59>>;
    {_, 60, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:60>>;
    {_, 61, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:61>>;
    {_, 62, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:62>>;
    {_, 63, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:63>>;
    {_, 64, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:64>>;
    {_, 65, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:65>>;
    {_, 66, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:66>>;
    {_, 67, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:67>>;
    {_, 68, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:68>>;
    {_, 69, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:69>>;
    {_, 70, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:70>>;
    {_, 71, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:71>>;
    {_, 72, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:72>>;
    {_, 73, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:73>>;
    {_, 74, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:74>>;
    {_, 75, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:75>>;
    {_, 76, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:76>>;
    {_, 77, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:77>>;
    {_, 78, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:78>>;
    {_, 79, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:79>>;
    {_, 80, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:80>>;
    {_, 81, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:81>>;
    {_, 82, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:82>>;
    {_, 83, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:83>>;
    {_, 84, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:84>>;
    {_, 85, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:85>>;
    {_, 86, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:86>>;
    {_, 87, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:87>>;
    {_, 88, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:88>>;
    {_, 89, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:89>>;
    {_, 90, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:90>>;
    {_, 91, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:91>>;
    {_, 92, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:92>>;
    {_, 93, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:93>>;
    {_, 94, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:94>>;
    {_, 95, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:95>>;
    {_, 96, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:96>>;
    {_, 97, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:97>>;
    {_, 98, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:98>>;
    {_, 99, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:99>>;
    {_, 100, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:100>>;
    {_, 101, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:101>>;
    {_, 102, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:102>>;
    {_, 103, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:103>>;
    {_, 104, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:104>>;
    {_, 105, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:105>>;
    {_, 106, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:106>>;
    {_, 107, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:107>>;
    {_, 108, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:108>>;
    {_, 109, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:109>>;
    {_, 110, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:110>>;
    {_, 111, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:111>>;
    {_, 112, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:112>>;
    {_, 113, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:113>>;
    {_, 114, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:114>>;
    {_, 115, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:115>>;
    {_, 116, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:116>>;
    {_, 117, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:117>>;
    {_, 118, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:118>>;
    {_, 119, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:119>>;
    {_, 120, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:120>>;
    {_, 121, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:121>>;
    {_, 122, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:122>>;
    {_, 123, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:123>>;
    {_, 124, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:124>>;
    {_, 125, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:125>>;
    {_, 126, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:126>>;
    {_, 127, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:127>>;
    {_, 128, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:128>>;
    {_, 129, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:129>>;
    {_, 130, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:130>>;
    {_, 131, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:131>>;
    {_, 132, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:132>>;
    {_, 133, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:133>>;
    {_, 134, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:134>>;
    {_, 135, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:135>>;
    {_, 136, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:136>>;
    {_, 137, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:137>>;
    {_, 138, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:138>>;
    {_, 139, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:139>>;
    {_, 140, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:140>>;
    {_, 141, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:141>>;
    {_, 142, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:142>>;
    {_, 143, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:143>>;
    {_, 144, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:144>>;
    {_, 145, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:145>>;
    {_, 146, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:146>>;
    {_, 147, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:147>>;
    {_, 148, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:148>>;
    {_, 149, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:149>>;
    {_, 150, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:150>>;
    {_, 151, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:151>>;
    {_, 152, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:152>>;
    {_, 153, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:153>>;
    {_, 154, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:154>>;
    {_, 155, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:155>>;
    {_, 156, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:156>>;
    {_, 157, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:157>>;
    {_, 158, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:158>>;
    {_, 159, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:159>>;
    {_, 160, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:160>>;
    {_, 161, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:161>>;
    {_, 162, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:162>>;
    {_, 163, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:163>>;
    {_, 164, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:164>>;
    {_, 165, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:165>>;
    {_, 166, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:166>>;
    {_, 167, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:167>>;
    {_, 168, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:168>>;
    {_, 169, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:169>>;
    {_, 170, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:170>>;
    {_, 171, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:171>>;
    {_, 172, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:172>>;
    {_, 173, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:173>>;
    {_, 174, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:174>>;
    {_, 175, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:175>>;
    {_, 176, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:176>>;
    {_, 177, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:177>>;
    {_, 178, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:178>>;
    {_, 179, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:179>>;
    {_, 180, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:180>>;
    {_, 181, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:181>>;
    {_, 182, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:182>>;
    {_, 183, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:183>>;
    {_, 184, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:184>>;
    {_, 185, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:185>>;
    {_, 186, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:186>>;
    {_, 187, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:187>>;
    {_, 188, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:188>>;
    {_, 189, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:189>>;
    {_, 190, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:190>>;
    {_, 191, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:191>>;
    {_, 192, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:192>>;
    {_, 193, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:193>>;
    {_, 194, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:194>>;
    {_, 195, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:195>>;
    {_, 196, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:196>>;
    {_, 197, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:197>>;
    {_, 198, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:198>>;
    {_, 199, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:199>>;
    {_, 200, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:200>>;
    {_, 201, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:201>>;
    {_, 202, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:202>>;
    {_, 203, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:203>>;
    {_, 204, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:204>>;
    {_, 205, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:205>>;
    {_, 206, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:206>>;
    {_, 207, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:207>>;
    {_, 208, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:208>>;
    {_, 209, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:209>>;
    {_, 210, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:210>>;
    {_, 211, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:211>>;
    {_, 212, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:212>>;
    {_, 213, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:213>>;
    {_, 214, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:214>>;
    {_, 215, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:215>>;
    {_, 216, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:216>>;
    {_, 217, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:217>>;
    {_, 218, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:218>>;
    {_, 219, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:219>>;
    {_, 220, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:220>>;
    {_, 221, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:221>>;
    {_, 222, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:222>>;
    {_, 223, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:223>>;
    {_, 224, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:224>>;
    {_, 225, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:225>>;
    {_, 226, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:226>>;
    {_, 227, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:227>>;
    {_, 228, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:228>>;
    {_, 229, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:229>>;
    {_, 230, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:230>>;
    {_, 231, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:231>>;
    {_, 232, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:232>>;
    {_, 233, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:233>>;
    {_, 234, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:234>>;
    {_, 235, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:235>>;
    {_, 236, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:236>>;
    {_, 237, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:237>>;
    {_, 238, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:238>>;
    {_, 239, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:239>>;
    {_, 240, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:240>>;
    {_, 241, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:241>>;
    {_, 242, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:242>>;
    {_, 243, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:243>>;
    {_, 244, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:244>>;
    {_, 245, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:245>>;
    {_, 246, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:246>>;
    {_, 247, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:247>>;
    {_, 248, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:248>>;
    {_, 249, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:249>>;
    {_, 250, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:250>>;
    {_, 251, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:251>>;
    {_, 252, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:252>>;
    {_, 253, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:253>>;
    {_, 254, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:254>>;
    {_, 255, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:255>>;
    {_, 256, binary, signed, big} ->
      <<Val:Size/signed-big-binary-unit:256>>;
    {_, 1, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:1>>;
    {_, 2, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:2>>;
    {_, 3, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:3>>;
    {_, 4, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:4>>;
    {_, 5, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:5>>;
    {_, 6, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:6>>;
    {_, 7, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:7>>;
    {_, 8, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:8>>;
    {_, 9, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:9>>;
    {_, 10, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:10>>;
    {_, 11, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:11>>;
    {_, 12, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:12>>;
    {_, 13, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:13>>;
    {_, 14, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:14>>;
    {_, 15, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:15>>;
    {_, 16, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:16>>;
    {_, 17, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:17>>;
    {_, 18, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:18>>;
    {_, 19, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:19>>;
    {_, 20, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:20>>;
    {_, 21, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:21>>;
    {_, 22, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:22>>;
    {_, 23, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:23>>;
    {_, 24, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:24>>;
    {_, 25, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:25>>;
    {_, 26, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:26>>;
    {_, 27, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:27>>;
    {_, 28, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:28>>;
    {_, 29, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:29>>;
    {_, 30, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:30>>;
    {_, 31, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:31>>;
    {_, 32, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:32>>;
    {_, 33, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:33>>;
    {_, 34, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:34>>;
    {_, 35, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:35>>;
    {_, 36, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:36>>;
    {_, 37, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:37>>;
    {_, 38, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:38>>;
    {_, 39, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:39>>;
    {_, 40, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:40>>;
    {_, 41, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:41>>;
    {_, 42, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:42>>;
    {_, 43, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:43>>;
    {_, 44, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:44>>;
    {_, 45, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:45>>;
    {_, 46, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:46>>;
    {_, 47, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:47>>;
    {_, 48, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:48>>;
    {_, 49, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:49>>;
    {_, 50, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:50>>;
    {_, 51, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:51>>;
    {_, 52, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:52>>;
    {_, 53, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:53>>;
    {_, 54, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:54>>;
    {_, 55, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:55>>;
    {_, 56, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:56>>;
    {_, 57, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:57>>;
    {_, 58, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:58>>;
    {_, 59, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:59>>;
    {_, 60, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:60>>;
    {_, 61, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:61>>;
    {_, 62, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:62>>;
    {_, 63, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:63>>;
    {_, 64, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:64>>;
    {_, 65, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:65>>;
    {_, 66, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:66>>;
    {_, 67, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:67>>;
    {_, 68, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:68>>;
    {_, 69, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:69>>;
    {_, 70, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:70>>;
    {_, 71, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:71>>;
    {_, 72, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:72>>;
    {_, 73, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:73>>;
    {_, 74, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:74>>;
    {_, 75, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:75>>;
    {_, 76, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:76>>;
    {_, 77, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:77>>;
    {_, 78, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:78>>;
    {_, 79, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:79>>;
    {_, 80, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:80>>;
    {_, 81, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:81>>;
    {_, 82, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:82>>;
    {_, 83, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:83>>;
    {_, 84, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:84>>;
    {_, 85, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:85>>;
    {_, 86, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:86>>;
    {_, 87, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:87>>;
    {_, 88, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:88>>;
    {_, 89, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:89>>;
    {_, 90, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:90>>;
    {_, 91, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:91>>;
    {_, 92, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:92>>;
    {_, 93, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:93>>;
    {_, 94, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:94>>;
    {_, 95, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:95>>;
    {_, 96, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:96>>;
    {_, 97, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:97>>;
    {_, 98, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:98>>;
    {_, 99, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:99>>;
    {_, 100, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:100>>;
    {_, 101, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:101>>;
    {_, 102, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:102>>;
    {_, 103, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:103>>;
    {_, 104, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:104>>;
    {_, 105, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:105>>;
    {_, 106, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:106>>;
    {_, 107, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:107>>;
    {_, 108, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:108>>;
    {_, 109, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:109>>;
    {_, 110, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:110>>;
    {_, 111, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:111>>;
    {_, 112, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:112>>;
    {_, 113, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:113>>;
    {_, 114, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:114>>;
    {_, 115, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:115>>;
    {_, 116, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:116>>;
    {_, 117, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:117>>;
    {_, 118, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:118>>;
    {_, 119, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:119>>;
    {_, 120, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:120>>;
    {_, 121, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:121>>;
    {_, 122, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:122>>;
    {_, 123, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:123>>;
    {_, 124, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:124>>;
    {_, 125, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:125>>;
    {_, 126, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:126>>;
    {_, 127, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:127>>;
    {_, 128, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:128>>;
    {_, 129, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:129>>;
    {_, 130, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:130>>;
    {_, 131, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:131>>;
    {_, 132, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:132>>;
    {_, 133, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:133>>;
    {_, 134, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:134>>;
    {_, 135, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:135>>;
    {_, 136, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:136>>;
    {_, 137, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:137>>;
    {_, 138, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:138>>;
    {_, 139, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:139>>;
    {_, 140, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:140>>;
    {_, 141, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:141>>;
    {_, 142, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:142>>;
    {_, 143, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:143>>;
    {_, 144, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:144>>;
    {_, 145, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:145>>;
    {_, 146, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:146>>;
    {_, 147, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:147>>;
    {_, 148, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:148>>;
    {_, 149, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:149>>;
    {_, 150, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:150>>;
    {_, 151, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:151>>;
    {_, 152, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:152>>;
    {_, 153, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:153>>;
    {_, 154, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:154>>;
    {_, 155, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:155>>;
    {_, 156, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:156>>;
    {_, 157, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:157>>;
    {_, 158, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:158>>;
    {_, 159, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:159>>;
    {_, 160, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:160>>;
    {_, 161, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:161>>;
    {_, 162, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:162>>;
    {_, 163, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:163>>;
    {_, 164, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:164>>;
    {_, 165, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:165>>;
    {_, 166, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:166>>;
    {_, 167, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:167>>;
    {_, 168, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:168>>;
    {_, 169, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:169>>;
    {_, 170, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:170>>;
    {_, 171, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:171>>;
    {_, 172, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:172>>;
    {_, 173, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:173>>;
    {_, 174, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:174>>;
    {_, 175, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:175>>;
    {_, 176, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:176>>;
    {_, 177, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:177>>;
    {_, 178, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:178>>;
    {_, 179, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:179>>;
    {_, 180, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:180>>;
    {_, 181, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:181>>;
    {_, 182, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:182>>;
    {_, 183, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:183>>;
    {_, 184, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:184>>;
    {_, 185, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:185>>;
    {_, 186, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:186>>;
    {_, 187, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:187>>;
    {_, 188, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:188>>;
    {_, 189, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:189>>;
    {_, 190, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:190>>;
    {_, 191, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:191>>;
    {_, 192, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:192>>;
    {_, 193, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:193>>;
    {_, 194, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:194>>;
    {_, 195, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:195>>;
    {_, 196, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:196>>;
    {_, 197, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:197>>;
    {_, 198, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:198>>;
    {_, 199, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:199>>;
    {_, 200, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:200>>;
    {_, 201, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:201>>;
    {_, 202, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:202>>;
    {_, 203, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:203>>;
    {_, 204, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:204>>;
    {_, 205, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:205>>;
    {_, 206, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:206>>;
    {_, 207, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:207>>;
    {_, 208, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:208>>;
    {_, 209, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:209>>;
    {_, 210, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:210>>;
    {_, 211, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:211>>;
    {_, 212, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:212>>;
    {_, 213, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:213>>;
    {_, 214, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:214>>;
    {_, 215, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:215>>;
    {_, 216, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:216>>;
    {_, 217, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:217>>;
    {_, 218, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:218>>;
    {_, 219, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:219>>;
    {_, 220, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:220>>;
    {_, 221, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:221>>;
    {_, 222, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:222>>;
    {_, 223, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:223>>;
    {_, 224, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:224>>;
    {_, 225, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:225>>;
    {_, 226, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:226>>;
    {_, 227, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:227>>;
    {_, 228, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:228>>;
    {_, 229, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:229>>;
    {_, 230, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:230>>;
    {_, 231, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:231>>;
    {_, 232, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:232>>;
    {_, 233, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:233>>;
    {_, 234, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:234>>;
    {_, 235, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:235>>;
    {_, 236, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:236>>;
    {_, 237, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:237>>;
    {_, 238, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:238>>;
    {_, 239, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:239>>;
    {_, 240, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:240>>;
    {_, 241, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:241>>;
    {_, 242, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:242>>;
    {_, 243, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:243>>;
    {_, 244, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:244>>;
    {_, 245, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:245>>;
    {_, 246, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:246>>;
    {_, 247, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:247>>;
    {_, 248, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:248>>;
    {_, 249, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:249>>;
    {_, 250, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:250>>;
    {_, 251, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:251>>;
    {_, 252, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:252>>;
    {_, 253, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:253>>;
    {_, 254, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:254>>;
    {_, 255, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:255>>;
    {_, 256, integer, unsigned, big} ->
      <<Val:Size/unsigned-big-integer-unit:256>>;
    {_, 1, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:1>>;
    {_, 2, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:2>>;
    {_, 3, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:3>>;
    {_, 4, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:4>>;
    {_, 5, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:5>>;
    {_, 6, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:6>>;
    {_, 7, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:7>>;
    {_, 8, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:8>>;
    {_, 9, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:9>>;
    {_, 10, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:10>>;
    {_, 11, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:11>>;
    {_, 12, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:12>>;
    {_, 13, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:13>>;
    {_, 14, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:14>>;
    {_, 15, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:15>>;
    {_, 16, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:16>>;
    {_, 17, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:17>>;
    {_, 18, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:18>>;
    {_, 19, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:19>>;
    {_, 20, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:20>>;
    {_, 21, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:21>>;
    {_, 22, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:22>>;
    {_, 23, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:23>>;
    {_, 24, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:24>>;
    {_, 25, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:25>>;
    {_, 26, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:26>>;
    {_, 27, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:27>>;
    {_, 28, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:28>>;
    {_, 29, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:29>>;
    {_, 30, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:30>>;
    {_, 31, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:31>>;
    {_, 32, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:32>>;
    {_, 33, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:33>>;
    {_, 34, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:34>>;
    {_, 35, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:35>>;
    {_, 36, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:36>>;
    {_, 37, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:37>>;
    {_, 38, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:38>>;
    {_, 39, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:39>>;
    {_, 40, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:40>>;
    {_, 41, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:41>>;
    {_, 42, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:42>>;
    {_, 43, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:43>>;
    {_, 44, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:44>>;
    {_, 45, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:45>>;
    {_, 46, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:46>>;
    {_, 47, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:47>>;
    {_, 48, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:48>>;
    {_, 49, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:49>>;
    {_, 50, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:50>>;
    {_, 51, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:51>>;
    {_, 52, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:52>>;
    {_, 53, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:53>>;
    {_, 54, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:54>>;
    {_, 55, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:55>>;
    {_, 56, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:56>>;
    {_, 57, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:57>>;
    {_, 58, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:58>>;
    {_, 59, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:59>>;
    {_, 60, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:60>>;
    {_, 61, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:61>>;
    {_, 62, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:62>>;
    {_, 63, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:63>>;
    {_, 64, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:64>>;
    {_, 65, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:65>>;
    {_, 66, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:66>>;
    {_, 67, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:67>>;
    {_, 68, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:68>>;
    {_, 69, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:69>>;
    {_, 70, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:70>>;
    {_, 71, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:71>>;
    {_, 72, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:72>>;
    {_, 73, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:73>>;
    {_, 74, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:74>>;
    {_, 75, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:75>>;
    {_, 76, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:76>>;
    {_, 77, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:77>>;
    {_, 78, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:78>>;
    {_, 79, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:79>>;
    {_, 80, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:80>>;
    {_, 81, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:81>>;
    {_, 82, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:82>>;
    {_, 83, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:83>>;
    {_, 84, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:84>>;
    {_, 85, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:85>>;
    {_, 86, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:86>>;
    {_, 87, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:87>>;
    {_, 88, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:88>>;
    {_, 89, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:89>>;
    {_, 90, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:90>>;
    {_, 91, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:91>>;
    {_, 92, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:92>>;
    {_, 93, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:93>>;
    {_, 94, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:94>>;
    {_, 95, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:95>>;
    {_, 96, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:96>>;
    {_, 97, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:97>>;
    {_, 98, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:98>>;
    {_, 99, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:99>>;
    {_, 100, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:100>>;
    {_, 101, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:101>>;
    {_, 102, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:102>>;
    {_, 103, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:103>>;
    {_, 104, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:104>>;
    {_, 105, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:105>>;
    {_, 106, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:106>>;
    {_, 107, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:107>>;
    {_, 108, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:108>>;
    {_, 109, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:109>>;
    {_, 110, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:110>>;
    {_, 111, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:111>>;
    {_, 112, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:112>>;
    {_, 113, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:113>>;
    {_, 114, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:114>>;
    {_, 115, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:115>>;
    {_, 116, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:116>>;
    {_, 117, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:117>>;
    {_, 118, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:118>>;
    {_, 119, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:119>>;
    {_, 120, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:120>>;
    {_, 121, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:121>>;
    {_, 122, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:122>>;
    {_, 123, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:123>>;
    {_, 124, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:124>>;
    {_, 125, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:125>>;
    {_, 126, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:126>>;
    {_, 127, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:127>>;
    {_, 128, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:128>>;
    {_, 129, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:129>>;
    {_, 130, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:130>>;
    {_, 131, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:131>>;
    {_, 132, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:132>>;
    {_, 133, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:133>>;
    {_, 134, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:134>>;
    {_, 135, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:135>>;
    {_, 136, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:136>>;
    {_, 137, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:137>>;
    {_, 138, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:138>>;
    {_, 139, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:139>>;
    {_, 140, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:140>>;
    {_, 141, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:141>>;
    {_, 142, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:142>>;
    {_, 143, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:143>>;
    {_, 144, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:144>>;
    {_, 145, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:145>>;
    {_, 146, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:146>>;
    {_, 147, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:147>>;
    {_, 148, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:148>>;
    {_, 149, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:149>>;
    {_, 150, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:150>>;
    {_, 151, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:151>>;
    {_, 152, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:152>>;
    {_, 153, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:153>>;
    {_, 154, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:154>>;
    {_, 155, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:155>>;
    {_, 156, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:156>>;
    {_, 157, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:157>>;
    {_, 158, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:158>>;
    {_, 159, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:159>>;
    {_, 160, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:160>>;
    {_, 161, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:161>>;
    {_, 162, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:162>>;
    {_, 163, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:163>>;
    {_, 164, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:164>>;
    {_, 165, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:165>>;
    {_, 166, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:166>>;
    {_, 167, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:167>>;
    {_, 168, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:168>>;
    {_, 169, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:169>>;
    {_, 170, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:170>>;
    {_, 171, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:171>>;
    {_, 172, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:172>>;
    {_, 173, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:173>>;
    {_, 174, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:174>>;
    {_, 175, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:175>>;
    {_, 176, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:176>>;
    {_, 177, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:177>>;
    {_, 178, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:178>>;
    {_, 179, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:179>>;
    {_, 180, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:180>>;
    {_, 181, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:181>>;
    {_, 182, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:182>>;
    {_, 183, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:183>>;
    {_, 184, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:184>>;
    {_, 185, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:185>>;
    {_, 186, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:186>>;
    {_, 187, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:187>>;
    {_, 188, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:188>>;
    {_, 189, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:189>>;
    {_, 190, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:190>>;
    {_, 191, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:191>>;
    {_, 192, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:192>>;
    {_, 193, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:193>>;
    {_, 194, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:194>>;
    {_, 195, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:195>>;
    {_, 196, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:196>>;
    {_, 197, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:197>>;
    {_, 198, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:198>>;
    {_, 199, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:199>>;
    {_, 200, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:200>>;
    {_, 201, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:201>>;
    {_, 202, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:202>>;
    {_, 203, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:203>>;
    {_, 204, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:204>>;
    {_, 205, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:205>>;
    {_, 206, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:206>>;
    {_, 207, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:207>>;
    {_, 208, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:208>>;
    {_, 209, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:209>>;
    {_, 210, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:210>>;
    {_, 211, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:211>>;
    {_, 212, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:212>>;
    {_, 213, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:213>>;
    {_, 214, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:214>>;
    {_, 215, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:215>>;
    {_, 216, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:216>>;
    {_, 217, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:217>>;
    {_, 218, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:218>>;
    {_, 219, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:219>>;
    {_, 220, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:220>>;
    {_, 221, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:221>>;
    {_, 222, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:222>>;
    {_, 223, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:223>>;
    {_, 224, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:224>>;
    {_, 225, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:225>>;
    {_, 226, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:226>>;
    {_, 227, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:227>>;
    {_, 228, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:228>>;
    {_, 229, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:229>>;
    {_, 230, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:230>>;
    {_, 231, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:231>>;
    {_, 232, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:232>>;
    {_, 233, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:233>>;
    {_, 234, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:234>>;
    {_, 235, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:235>>;
    {_, 236, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:236>>;
    {_, 237, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:237>>;
    {_, 238, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:238>>;
    {_, 239, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:239>>;
    {_, 240, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:240>>;
    {_, 241, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:241>>;
    {_, 242, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:242>>;
    {_, 243, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:243>>;
    {_, 244, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:244>>;
    {_, 245, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:245>>;
    {_, 246, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:246>>;
    {_, 247, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:247>>;
    {_, 248, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:248>>;
    {_, 249, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:249>>;
    {_, 250, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:250>>;
    {_, 251, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:251>>;
    {_, 252, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:252>>;
    {_, 253, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:253>>;
    {_, 254, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:254>>;
    {_, 255, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:255>>;
    {_, 256, float, unsigned, big} ->
      <<Val:Size/unsigned-big-float-unit:256>>;
    {all, 1, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:1>>;
    {all, 2, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:2>>;
    {all, 3, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:3>>;
    {all, 4, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:4>>;
    {all, 5, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:5>>;
    {all, 6, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:6>>;
    {all, 7, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:7>>;
    {all, 8, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:8>>;
    {all, 9, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:9>>;
    {all, 10, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:10>>;
    {all, 11, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:11>>;
    {all, 12, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:12>>;
    {all, 13, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:13>>;
    {all, 14, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:14>>;
    {all, 15, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:15>>;
    {all, 16, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:16>>;
    {all, 17, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:17>>;
    {all, 18, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:18>>;
    {all, 19, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:19>>;
    {all, 20, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:20>>;
    {all, 21, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:21>>;
    {all, 22, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:22>>;
    {all, 23, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:23>>;
    {all, 24, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:24>>;
    {all, 25, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:25>>;
    {all, 26, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:26>>;
    {all, 27, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:27>>;
    {all, 28, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:28>>;
    {all, 29, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:29>>;
    {all, 30, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:30>>;
    {all, 31, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:31>>;
    {all, 32, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:32>>;
    {all, 33, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:33>>;
    {all, 34, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:34>>;
    {all, 35, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:35>>;
    {all, 36, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:36>>;
    {all, 37, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:37>>;
    {all, 38, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:38>>;
    {all, 39, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:39>>;
    {all, 40, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:40>>;
    {all, 41, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:41>>;
    {all, 42, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:42>>;
    {all, 43, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:43>>;
    {all, 44, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:44>>;
    {all, 45, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:45>>;
    {all, 46, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:46>>;
    {all, 47, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:47>>;
    {all, 48, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:48>>;
    {all, 49, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:49>>;
    {all, 50, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:50>>;
    {all, 51, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:51>>;
    {all, 52, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:52>>;
    {all, 53, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:53>>;
    {all, 54, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:54>>;
    {all, 55, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:55>>;
    {all, 56, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:56>>;
    {all, 57, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:57>>;
    {all, 58, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:58>>;
    {all, 59, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:59>>;
    {all, 60, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:60>>;
    {all, 61, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:61>>;
    {all, 62, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:62>>;
    {all, 63, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:63>>;
    {all, 64, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:64>>;
    {all, 65, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:65>>;
    {all, 66, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:66>>;
    {all, 67, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:67>>;
    {all, 68, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:68>>;
    {all, 69, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:69>>;
    {all, 70, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:70>>;
    {all, 71, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:71>>;
    {all, 72, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:72>>;
    {all, 73, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:73>>;
    {all, 74, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:74>>;
    {all, 75, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:75>>;
    {all, 76, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:76>>;
    {all, 77, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:77>>;
    {all, 78, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:78>>;
    {all, 79, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:79>>;
    {all, 80, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:80>>;
    {all, 81, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:81>>;
    {all, 82, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:82>>;
    {all, 83, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:83>>;
    {all, 84, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:84>>;
    {all, 85, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:85>>;
    {all, 86, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:86>>;
    {all, 87, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:87>>;
    {all, 88, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:88>>;
    {all, 89, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:89>>;
    {all, 90, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:90>>;
    {all, 91, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:91>>;
    {all, 92, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:92>>;
    {all, 93, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:93>>;
    {all, 94, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:94>>;
    {all, 95, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:95>>;
    {all, 96, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:96>>;
    {all, 97, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:97>>;
    {all, 98, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:98>>;
    {all, 99, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:99>>;
    {all, 100, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:100>>;
    {all, 101, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:101>>;
    {all, 102, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:102>>;
    {all, 103, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:103>>;
    {all, 104, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:104>>;
    {all, 105, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:105>>;
    {all, 106, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:106>>;
    {all, 107, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:107>>;
    {all, 108, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:108>>;
    {all, 109, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:109>>;
    {all, 110, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:110>>;
    {all, 111, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:111>>;
    {all, 112, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:112>>;
    {all, 113, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:113>>;
    {all, 114, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:114>>;
    {all, 115, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:115>>;
    {all, 116, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:116>>;
    {all, 117, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:117>>;
    {all, 118, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:118>>;
    {all, 119, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:119>>;
    {all, 120, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:120>>;
    {all, 121, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:121>>;
    {all, 122, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:122>>;
    {all, 123, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:123>>;
    {all, 124, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:124>>;
    {all, 125, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:125>>;
    {all, 126, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:126>>;
    {all, 127, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:127>>;
    {all, 128, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:128>>;
    {all, 129, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:129>>;
    {all, 130, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:130>>;
    {all, 131, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:131>>;
    {all, 132, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:132>>;
    {all, 133, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:133>>;
    {all, 134, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:134>>;
    {all, 135, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:135>>;
    {all, 136, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:136>>;
    {all, 137, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:137>>;
    {all, 138, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:138>>;
    {all, 139, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:139>>;
    {all, 140, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:140>>;
    {all, 141, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:141>>;
    {all, 142, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:142>>;
    {all, 143, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:143>>;
    {all, 144, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:144>>;
    {all, 145, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:145>>;
    {all, 146, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:146>>;
    {all, 147, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:147>>;
    {all, 148, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:148>>;
    {all, 149, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:149>>;
    {all, 150, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:150>>;
    {all, 151, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:151>>;
    {all, 152, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:152>>;
    {all, 153, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:153>>;
    {all, 154, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:154>>;
    {all, 155, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:155>>;
    {all, 156, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:156>>;
    {all, 157, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:157>>;
    {all, 158, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:158>>;
    {all, 159, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:159>>;
    {all, 160, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:160>>;
    {all, 161, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:161>>;
    {all, 162, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:162>>;
    {all, 163, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:163>>;
    {all, 164, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:164>>;
    {all, 165, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:165>>;
    {all, 166, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:166>>;
    {all, 167, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:167>>;
    {all, 168, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:168>>;
    {all, 169, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:169>>;
    {all, 170, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:170>>;
    {all, 171, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:171>>;
    {all, 172, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:172>>;
    {all, 173, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:173>>;
    {all, 174, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:174>>;
    {all, 175, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:175>>;
    {all, 176, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:176>>;
    {all, 177, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:177>>;
    {all, 178, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:178>>;
    {all, 179, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:179>>;
    {all, 180, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:180>>;
    {all, 181, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:181>>;
    {all, 182, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:182>>;
    {all, 183, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:183>>;
    {all, 184, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:184>>;
    {all, 185, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:185>>;
    {all, 186, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:186>>;
    {all, 187, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:187>>;
    {all, 188, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:188>>;
    {all, 189, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:189>>;
    {all, 190, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:190>>;
    {all, 191, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:191>>;
    {all, 192, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:192>>;
    {all, 193, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:193>>;
    {all, 194, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:194>>;
    {all, 195, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:195>>;
    {all, 196, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:196>>;
    {all, 197, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:197>>;
    {all, 198, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:198>>;
    {all, 199, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:199>>;
    {all, 200, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:200>>;
    {all, 201, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:201>>;
    {all, 202, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:202>>;
    {all, 203, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:203>>;
    {all, 204, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:204>>;
    {all, 205, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:205>>;
    {all, 206, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:206>>;
    {all, 207, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:207>>;
    {all, 208, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:208>>;
    {all, 209, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:209>>;
    {all, 210, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:210>>;
    {all, 211, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:211>>;
    {all, 212, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:212>>;
    {all, 213, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:213>>;
    {all, 214, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:214>>;
    {all, 215, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:215>>;
    {all, 216, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:216>>;
    {all, 217, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:217>>;
    {all, 218, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:218>>;
    {all, 219, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:219>>;
    {all, 220, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:220>>;
    {all, 221, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:221>>;
    {all, 222, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:222>>;
    {all, 223, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:223>>;
    {all, 224, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:224>>;
    {all, 225, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:225>>;
    {all, 226, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:226>>;
    {all, 227, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:227>>;
    {all, 228, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:228>>;
    {all, 229, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:229>>;
    {all, 230, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:230>>;
    {all, 231, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:231>>;
    {all, 232, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:232>>;
    {all, 233, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:233>>;
    {all, 234, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:234>>;
    {all, 235, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:235>>;
    {all, 236, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:236>>;
    {all, 237, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:237>>;
    {all, 238, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:238>>;
    {all, 239, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:239>>;
    {all, 240, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:240>>;
    {all, 241, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:241>>;
    {all, 242, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:242>>;
    {all, 243, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:243>>;
    {all, 244, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:244>>;
    {all, 245, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:245>>;
    {all, 246, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:246>>;
    {all, 247, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:247>>;
    {all, 248, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:248>>;
    {all, 249, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:249>>;
    {all, 250, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:250>>;
    {all, 251, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:251>>;
    {all, 252, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:252>>;
    {all, 253, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:253>>;
    {all, 254, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:254>>;
    {all, 255, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:255>>;
    {all, 256, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:256>>;
    {_, 1, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:1>>;
    {_, 2, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:2>>;
    {_, 3, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:3>>;
    {_, 4, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:4>>;
    {_, 5, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:5>>;
    {_, 6, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:6>>;
    {_, 7, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:7>>;
    {_, 8, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:8>>;
    {_, 9, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:9>>;
    {_, 10, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:10>>;
    {_, 11, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:11>>;
    {_, 12, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:12>>;
    {_, 13, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:13>>;
    {_, 14, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:14>>;
    {_, 15, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:15>>;
    {_, 16, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:16>>;
    {_, 17, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:17>>;
    {_, 18, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:18>>;
    {_, 19, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:19>>;
    {_, 20, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:20>>;
    {_, 21, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:21>>;
    {_, 22, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:22>>;
    {_, 23, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:23>>;
    {_, 24, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:24>>;
    {_, 25, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:25>>;
    {_, 26, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:26>>;
    {_, 27, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:27>>;
    {_, 28, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:28>>;
    {_, 29, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:29>>;
    {_, 30, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:30>>;
    {_, 31, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:31>>;
    {_, 32, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:32>>;
    {_, 33, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:33>>;
    {_, 34, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:34>>;
    {_, 35, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:35>>;
    {_, 36, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:36>>;
    {_, 37, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:37>>;
    {_, 38, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:38>>;
    {_, 39, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:39>>;
    {_, 40, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:40>>;
    {_, 41, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:41>>;
    {_, 42, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:42>>;
    {_, 43, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:43>>;
    {_, 44, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:44>>;
    {_, 45, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:45>>;
    {_, 46, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:46>>;
    {_, 47, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:47>>;
    {_, 48, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:48>>;
    {_, 49, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:49>>;
    {_, 50, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:50>>;
    {_, 51, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:51>>;
    {_, 52, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:52>>;
    {_, 53, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:53>>;
    {_, 54, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:54>>;
    {_, 55, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:55>>;
    {_, 56, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:56>>;
    {_, 57, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:57>>;
    {_, 58, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:58>>;
    {_, 59, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:59>>;
    {_, 60, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:60>>;
    {_, 61, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:61>>;
    {_, 62, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:62>>;
    {_, 63, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:63>>;
    {_, 64, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:64>>;
    {_, 65, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:65>>;
    {_, 66, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:66>>;
    {_, 67, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:67>>;
    {_, 68, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:68>>;
    {_, 69, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:69>>;
    {_, 70, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:70>>;
    {_, 71, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:71>>;
    {_, 72, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:72>>;
    {_, 73, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:73>>;
    {_, 74, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:74>>;
    {_, 75, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:75>>;
    {_, 76, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:76>>;
    {_, 77, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:77>>;
    {_, 78, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:78>>;
    {_, 79, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:79>>;
    {_, 80, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:80>>;
    {_, 81, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:81>>;
    {_, 82, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:82>>;
    {_, 83, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:83>>;
    {_, 84, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:84>>;
    {_, 85, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:85>>;
    {_, 86, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:86>>;
    {_, 87, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:87>>;
    {_, 88, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:88>>;
    {_, 89, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:89>>;
    {_, 90, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:90>>;
    {_, 91, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:91>>;
    {_, 92, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:92>>;
    {_, 93, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:93>>;
    {_, 94, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:94>>;
    {_, 95, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:95>>;
    {_, 96, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:96>>;
    {_, 97, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:97>>;
    {_, 98, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:98>>;
    {_, 99, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:99>>;
    {_, 100, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:100>>;
    {_, 101, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:101>>;
    {_, 102, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:102>>;
    {_, 103, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:103>>;
    {_, 104, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:104>>;
    {_, 105, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:105>>;
    {_, 106, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:106>>;
    {_, 107, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:107>>;
    {_, 108, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:108>>;
    {_, 109, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:109>>;
    {_, 110, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:110>>;
    {_, 111, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:111>>;
    {_, 112, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:112>>;
    {_, 113, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:113>>;
    {_, 114, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:114>>;
    {_, 115, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:115>>;
    {_, 116, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:116>>;
    {_, 117, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:117>>;
    {_, 118, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:118>>;
    {_, 119, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:119>>;
    {_, 120, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:120>>;
    {_, 121, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:121>>;
    {_, 122, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:122>>;
    {_, 123, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:123>>;
    {_, 124, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:124>>;
    {_, 125, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:125>>;
    {_, 126, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:126>>;
    {_, 127, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:127>>;
    {_, 128, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:128>>;
    {_, 129, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:129>>;
    {_, 130, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:130>>;
    {_, 131, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:131>>;
    {_, 132, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:132>>;
    {_, 133, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:133>>;
    {_, 134, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:134>>;
    {_, 135, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:135>>;
    {_, 136, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:136>>;
    {_, 137, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:137>>;
    {_, 138, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:138>>;
    {_, 139, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:139>>;
    {_, 140, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:140>>;
    {_, 141, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:141>>;
    {_, 142, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:142>>;
    {_, 143, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:143>>;
    {_, 144, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:144>>;
    {_, 145, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:145>>;
    {_, 146, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:146>>;
    {_, 147, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:147>>;
    {_, 148, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:148>>;
    {_, 149, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:149>>;
    {_, 150, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:150>>;
    {_, 151, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:151>>;
    {_, 152, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:152>>;
    {_, 153, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:153>>;
    {_, 154, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:154>>;
    {_, 155, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:155>>;
    {_, 156, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:156>>;
    {_, 157, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:157>>;
    {_, 158, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:158>>;
    {_, 159, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:159>>;
    {_, 160, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:160>>;
    {_, 161, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:161>>;
    {_, 162, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:162>>;
    {_, 163, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:163>>;
    {_, 164, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:164>>;
    {_, 165, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:165>>;
    {_, 166, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:166>>;
    {_, 167, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:167>>;
    {_, 168, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:168>>;
    {_, 169, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:169>>;
    {_, 170, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:170>>;
    {_, 171, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:171>>;
    {_, 172, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:172>>;
    {_, 173, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:173>>;
    {_, 174, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:174>>;
    {_, 175, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:175>>;
    {_, 176, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:176>>;
    {_, 177, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:177>>;
    {_, 178, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:178>>;
    {_, 179, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:179>>;
    {_, 180, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:180>>;
    {_, 181, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:181>>;
    {_, 182, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:182>>;
    {_, 183, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:183>>;
    {_, 184, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:184>>;
    {_, 185, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:185>>;
    {_, 186, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:186>>;
    {_, 187, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:187>>;
    {_, 188, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:188>>;
    {_, 189, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:189>>;
    {_, 190, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:190>>;
    {_, 191, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:191>>;
    {_, 192, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:192>>;
    {_, 193, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:193>>;
    {_, 194, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:194>>;
    {_, 195, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:195>>;
    {_, 196, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:196>>;
    {_, 197, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:197>>;
    {_, 198, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:198>>;
    {_, 199, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:199>>;
    {_, 200, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:200>>;
    {_, 201, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:201>>;
    {_, 202, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:202>>;
    {_, 203, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:203>>;
    {_, 204, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:204>>;
    {_, 205, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:205>>;
    {_, 206, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:206>>;
    {_, 207, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:207>>;
    {_, 208, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:208>>;
    {_, 209, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:209>>;
    {_, 210, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:210>>;
    {_, 211, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:211>>;
    {_, 212, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:212>>;
    {_, 213, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:213>>;
    {_, 214, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:214>>;
    {_, 215, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:215>>;
    {_, 216, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:216>>;
    {_, 217, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:217>>;
    {_, 218, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:218>>;
    {_, 219, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:219>>;
    {_, 220, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:220>>;
    {_, 221, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:221>>;
    {_, 222, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:222>>;
    {_, 223, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:223>>;
    {_, 224, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:224>>;
    {_, 225, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:225>>;
    {_, 226, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:226>>;
    {_, 227, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:227>>;
    {_, 228, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:228>>;
    {_, 229, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:229>>;
    {_, 230, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:230>>;
    {_, 231, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:231>>;
    {_, 232, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:232>>;
    {_, 233, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:233>>;
    {_, 234, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:234>>;
    {_, 235, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:235>>;
    {_, 236, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:236>>;
    {_, 237, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:237>>;
    {_, 238, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:238>>;
    {_, 239, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:239>>;
    {_, 240, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:240>>;
    {_, 241, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:241>>;
    {_, 242, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:242>>;
    {_, 243, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:243>>;
    {_, 244, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:244>>;
    {_, 245, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:245>>;
    {_, 246, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:246>>;
    {_, 247, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:247>>;
    {_, 248, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:248>>;
    {_, 249, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:249>>;
    {_, 250, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:250>>;
    {_, 251, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:251>>;
    {_, 252, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:252>>;
    {_, 253, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:253>>;
    {_, 254, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:254>>;
    {_, 255, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:255>>;
    {_, 256, binary, unsigned, big} ->
      <<Val:Size/unsigned-big-binary-unit:256>>;
    {_, 1, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:1>>;
    {_, 2, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:2>>;
    {_, 3, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:3>>;
    {_, 4, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:4>>;
    {_, 5, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:5>>;
    {_, 6, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:6>>;
    {_, 7, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:7>>;
    {_, 8, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:8>>;
    {_, 9, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:9>>;
    {_, 10, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:10>>;
    {_, 11, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:11>>;
    {_, 12, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:12>>;
    {_, 13, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:13>>;
    {_, 14, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:14>>;
    {_, 15, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:15>>;
    {_, 16, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:16>>;
    {_, 17, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:17>>;
    {_, 18, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:18>>;
    {_, 19, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:19>>;
    {_, 20, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:20>>;
    {_, 21, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:21>>;
    {_, 22, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:22>>;
    {_, 23, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:23>>;
    {_, 24, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:24>>;
    {_, 25, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:25>>;
    {_, 26, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:26>>;
    {_, 27, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:27>>;
    {_, 28, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:28>>;
    {_, 29, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:29>>;
    {_, 30, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:30>>;
    {_, 31, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:31>>;
    {_, 32, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:32>>;
    {_, 33, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:33>>;
    {_, 34, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:34>>;
    {_, 35, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:35>>;
    {_, 36, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:36>>;
    {_, 37, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:37>>;
    {_, 38, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:38>>;
    {_, 39, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:39>>;
    {_, 40, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:40>>;
    {_, 41, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:41>>;
    {_, 42, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:42>>;
    {_, 43, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:43>>;
    {_, 44, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:44>>;
    {_, 45, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:45>>;
    {_, 46, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:46>>;
    {_, 47, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:47>>;
    {_, 48, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:48>>;
    {_, 49, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:49>>;
    {_, 50, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:50>>;
    {_, 51, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:51>>;
    {_, 52, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:52>>;
    {_, 53, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:53>>;
    {_, 54, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:54>>;
    {_, 55, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:55>>;
    {_, 56, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:56>>;
    {_, 57, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:57>>;
    {_, 58, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:58>>;
    {_, 59, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:59>>;
    {_, 60, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:60>>;
    {_, 61, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:61>>;
    {_, 62, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:62>>;
    {_, 63, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:63>>;
    {_, 64, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:64>>;
    {_, 65, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:65>>;
    {_, 66, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:66>>;
    {_, 67, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:67>>;
    {_, 68, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:68>>;
    {_, 69, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:69>>;
    {_, 70, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:70>>;
    {_, 71, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:71>>;
    {_, 72, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:72>>;
    {_, 73, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:73>>;
    {_, 74, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:74>>;
    {_, 75, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:75>>;
    {_, 76, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:76>>;
    {_, 77, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:77>>;
    {_, 78, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:78>>;
    {_, 79, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:79>>;
    {_, 80, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:80>>;
    {_, 81, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:81>>;
    {_, 82, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:82>>;
    {_, 83, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:83>>;
    {_, 84, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:84>>;
    {_, 85, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:85>>;
    {_, 86, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:86>>;
    {_, 87, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:87>>;
    {_, 88, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:88>>;
    {_, 89, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:89>>;
    {_, 90, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:90>>;
    {_, 91, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:91>>;
    {_, 92, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:92>>;
    {_, 93, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:93>>;
    {_, 94, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:94>>;
    {_, 95, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:95>>;
    {_, 96, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:96>>;
    {_, 97, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:97>>;
    {_, 98, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:98>>;
    {_, 99, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:99>>;
    {_, 100, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:100>>;
    {_, 101, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:101>>;
    {_, 102, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:102>>;
    {_, 103, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:103>>;
    {_, 104, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:104>>;
    {_, 105, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:105>>;
    {_, 106, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:106>>;
    {_, 107, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:107>>;
    {_, 108, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:108>>;
    {_, 109, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:109>>;
    {_, 110, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:110>>;
    {_, 111, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:111>>;
    {_, 112, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:112>>;
    {_, 113, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:113>>;
    {_, 114, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:114>>;
    {_, 115, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:115>>;
    {_, 116, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:116>>;
    {_, 117, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:117>>;
    {_, 118, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:118>>;
    {_, 119, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:119>>;
    {_, 120, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:120>>;
    {_, 121, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:121>>;
    {_, 122, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:122>>;
    {_, 123, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:123>>;
    {_, 124, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:124>>;
    {_, 125, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:125>>;
    {_, 126, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:126>>;
    {_, 127, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:127>>;
    {_, 128, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:128>>;
    {_, 129, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:129>>;
    {_, 130, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:130>>;
    {_, 131, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:131>>;
    {_, 132, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:132>>;
    {_, 133, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:133>>;
    {_, 134, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:134>>;
    {_, 135, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:135>>;
    {_, 136, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:136>>;
    {_, 137, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:137>>;
    {_, 138, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:138>>;
    {_, 139, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:139>>;
    {_, 140, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:140>>;
    {_, 141, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:141>>;
    {_, 142, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:142>>;
    {_, 143, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:143>>;
    {_, 144, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:144>>;
    {_, 145, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:145>>;
    {_, 146, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:146>>;
    {_, 147, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:147>>;
    {_, 148, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:148>>;
    {_, 149, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:149>>;
    {_, 150, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:150>>;
    {_, 151, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:151>>;
    {_, 152, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:152>>;
    {_, 153, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:153>>;
    {_, 154, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:154>>;
    {_, 155, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:155>>;
    {_, 156, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:156>>;
    {_, 157, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:157>>;
    {_, 158, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:158>>;
    {_, 159, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:159>>;
    {_, 160, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:160>>;
    {_, 161, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:161>>;
    {_, 162, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:162>>;
    {_, 163, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:163>>;
    {_, 164, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:164>>;
    {_, 165, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:165>>;
    {_, 166, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:166>>;
    {_, 167, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:167>>;
    {_, 168, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:168>>;
    {_, 169, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:169>>;
    {_, 170, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:170>>;
    {_, 171, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:171>>;
    {_, 172, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:172>>;
    {_, 173, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:173>>;
    {_, 174, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:174>>;
    {_, 175, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:175>>;
    {_, 176, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:176>>;
    {_, 177, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:177>>;
    {_, 178, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:178>>;
    {_, 179, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:179>>;
    {_, 180, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:180>>;
    {_, 181, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:181>>;
    {_, 182, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:182>>;
    {_, 183, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:183>>;
    {_, 184, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:184>>;
    {_, 185, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:185>>;
    {_, 186, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:186>>;
    {_, 187, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:187>>;
    {_, 188, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:188>>;
    {_, 189, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:189>>;
    {_, 190, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:190>>;
    {_, 191, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:191>>;
    {_, 192, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:192>>;
    {_, 193, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:193>>;
    {_, 194, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:194>>;
    {_, 195, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:195>>;
    {_, 196, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:196>>;
    {_, 197, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:197>>;
    {_, 198, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:198>>;
    {_, 199, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:199>>;
    {_, 200, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:200>>;
    {_, 201, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:201>>;
    {_, 202, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:202>>;
    {_, 203, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:203>>;
    {_, 204, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:204>>;
    {_, 205, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:205>>;
    {_, 206, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:206>>;
    {_, 207, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:207>>;
    {_, 208, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:208>>;
    {_, 209, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:209>>;
    {_, 210, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:210>>;
    {_, 211, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:211>>;
    {_, 212, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:212>>;
    {_, 213, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:213>>;
    {_, 214, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:214>>;
    {_, 215, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:215>>;
    {_, 216, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:216>>;
    {_, 217, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:217>>;
    {_, 218, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:218>>;
    {_, 219, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:219>>;
    {_, 220, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:220>>;
    {_, 221, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:221>>;
    {_, 222, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:222>>;
    {_, 223, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:223>>;
    {_, 224, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:224>>;
    {_, 225, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:225>>;
    {_, 226, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:226>>;
    {_, 227, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:227>>;
    {_, 228, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:228>>;
    {_, 229, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:229>>;
    {_, 230, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:230>>;
    {_, 231, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:231>>;
    {_, 232, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:232>>;
    {_, 233, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:233>>;
    {_, 234, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:234>>;
    {_, 235, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:235>>;
    {_, 236, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:236>>;
    {_, 237, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:237>>;
    {_, 238, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:238>>;
    {_, 239, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:239>>;
    {_, 240, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:240>>;
    {_, 241, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:241>>;
    {_, 242, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:242>>;
    {_, 243, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:243>>;
    {_, 244, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:244>>;
    {_, 245, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:245>>;
    {_, 246, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:246>>;
    {_, 247, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:247>>;
    {_, 248, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:248>>;
    {_, 249, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:249>>;
    {_, 250, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:250>>;
    {_, 251, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:251>>;
    {_, 252, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:252>>;
    {_, 253, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:253>>;
    {_, 254, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:254>>;
    {_, 255, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:255>>;
    {_, 256, integer, signed, little} ->
      <<Val:Size/signed-little-integer-unit:256>>;
    {_, 1, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:1>>;
    {_, 2, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:2>>;
    {_, 3, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:3>>;
    {_, 4, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:4>>;
    {_, 5, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:5>>;
    {_, 6, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:6>>;
    {_, 7, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:7>>;
    {_, 8, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:8>>;
    {_, 9, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:9>>;
    {_, 10, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:10>>;
    {_, 11, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:11>>;
    {_, 12, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:12>>;
    {_, 13, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:13>>;
    {_, 14, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:14>>;
    {_, 15, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:15>>;
    {_, 16, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:16>>;
    {_, 17, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:17>>;
    {_, 18, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:18>>;
    {_, 19, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:19>>;
    {_, 20, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:20>>;
    {_, 21, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:21>>;
    {_, 22, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:22>>;
    {_, 23, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:23>>;
    {_, 24, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:24>>;
    {_, 25, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:25>>;
    {_, 26, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:26>>;
    {_, 27, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:27>>;
    {_, 28, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:28>>;
    {_, 29, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:29>>;
    {_, 30, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:30>>;
    {_, 31, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:31>>;
    {_, 32, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:32>>;
    {_, 33, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:33>>;
    {_, 34, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:34>>;
    {_, 35, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:35>>;
    {_, 36, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:36>>;
    {_, 37, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:37>>;
    {_, 38, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:38>>;
    {_, 39, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:39>>;
    {_, 40, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:40>>;
    {_, 41, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:41>>;
    {_, 42, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:42>>;
    {_, 43, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:43>>;
    {_, 44, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:44>>;
    {_, 45, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:45>>;
    {_, 46, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:46>>;
    {_, 47, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:47>>;
    {_, 48, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:48>>;
    {_, 49, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:49>>;
    {_, 50, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:50>>;
    {_, 51, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:51>>;
    {_, 52, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:52>>;
    {_, 53, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:53>>;
    {_, 54, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:54>>;
    {_, 55, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:55>>;
    {_, 56, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:56>>;
    {_, 57, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:57>>;
    {_, 58, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:58>>;
    {_, 59, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:59>>;
    {_, 60, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:60>>;
    {_, 61, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:61>>;
    {_, 62, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:62>>;
    {_, 63, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:63>>;
    {_, 64, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:64>>;
    {_, 65, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:65>>;
    {_, 66, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:66>>;
    {_, 67, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:67>>;
    {_, 68, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:68>>;
    {_, 69, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:69>>;
    {_, 70, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:70>>;
    {_, 71, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:71>>;
    {_, 72, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:72>>;
    {_, 73, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:73>>;
    {_, 74, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:74>>;
    {_, 75, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:75>>;
    {_, 76, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:76>>;
    {_, 77, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:77>>;
    {_, 78, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:78>>;
    {_, 79, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:79>>;
    {_, 80, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:80>>;
    {_, 81, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:81>>;
    {_, 82, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:82>>;
    {_, 83, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:83>>;
    {_, 84, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:84>>;
    {_, 85, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:85>>;
    {_, 86, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:86>>;
    {_, 87, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:87>>;
    {_, 88, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:88>>;
    {_, 89, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:89>>;
    {_, 90, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:90>>;
    {_, 91, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:91>>;
    {_, 92, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:92>>;
    {_, 93, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:93>>;
    {_, 94, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:94>>;
    {_, 95, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:95>>;
    {_, 96, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:96>>;
    {_, 97, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:97>>;
    {_, 98, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:98>>;
    {_, 99, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:99>>;
    {_, 100, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:100>>;
    {_, 101, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:101>>;
    {_, 102, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:102>>;
    {_, 103, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:103>>;
    {_, 104, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:104>>;
    {_, 105, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:105>>;
    {_, 106, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:106>>;
    {_, 107, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:107>>;
    {_, 108, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:108>>;
    {_, 109, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:109>>;
    {_, 110, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:110>>;
    {_, 111, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:111>>;
    {_, 112, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:112>>;
    {_, 113, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:113>>;
    {_, 114, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:114>>;
    {_, 115, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:115>>;
    {_, 116, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:116>>;
    {_, 117, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:117>>;
    {_, 118, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:118>>;
    {_, 119, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:119>>;
    {_, 120, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:120>>;
    {_, 121, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:121>>;
    {_, 122, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:122>>;
    {_, 123, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:123>>;
    {_, 124, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:124>>;
    {_, 125, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:125>>;
    {_, 126, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:126>>;
    {_, 127, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:127>>;
    {_, 128, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:128>>;
    {_, 129, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:129>>;
    {_, 130, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:130>>;
    {_, 131, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:131>>;
    {_, 132, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:132>>;
    {_, 133, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:133>>;
    {_, 134, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:134>>;
    {_, 135, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:135>>;
    {_, 136, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:136>>;
    {_, 137, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:137>>;
    {_, 138, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:138>>;
    {_, 139, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:139>>;
    {_, 140, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:140>>;
    {_, 141, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:141>>;
    {_, 142, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:142>>;
    {_, 143, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:143>>;
    {_, 144, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:144>>;
    {_, 145, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:145>>;
    {_, 146, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:146>>;
    {_, 147, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:147>>;
    {_, 148, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:148>>;
    {_, 149, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:149>>;
    {_, 150, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:150>>;
    {_, 151, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:151>>;
    {_, 152, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:152>>;
    {_, 153, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:153>>;
    {_, 154, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:154>>;
    {_, 155, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:155>>;
    {_, 156, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:156>>;
    {_, 157, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:157>>;
    {_, 158, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:158>>;
    {_, 159, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:159>>;
    {_, 160, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:160>>;
    {_, 161, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:161>>;
    {_, 162, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:162>>;
    {_, 163, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:163>>;
    {_, 164, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:164>>;
    {_, 165, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:165>>;
    {_, 166, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:166>>;
    {_, 167, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:167>>;
    {_, 168, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:168>>;
    {_, 169, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:169>>;
    {_, 170, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:170>>;
    {_, 171, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:171>>;
    {_, 172, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:172>>;
    {_, 173, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:173>>;
    {_, 174, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:174>>;
    {_, 175, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:175>>;
    {_, 176, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:176>>;
    {_, 177, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:177>>;
    {_, 178, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:178>>;
    {_, 179, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:179>>;
    {_, 180, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:180>>;
    {_, 181, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:181>>;
    {_, 182, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:182>>;
    {_, 183, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:183>>;
    {_, 184, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:184>>;
    {_, 185, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:185>>;
    {_, 186, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:186>>;
    {_, 187, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:187>>;
    {_, 188, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:188>>;
    {_, 189, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:189>>;
    {_, 190, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:190>>;
    {_, 191, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:191>>;
    {_, 192, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:192>>;
    {_, 193, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:193>>;
    {_, 194, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:194>>;
    {_, 195, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:195>>;
    {_, 196, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:196>>;
    {_, 197, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:197>>;
    {_, 198, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:198>>;
    {_, 199, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:199>>;
    {_, 200, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:200>>;
    {_, 201, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:201>>;
    {_, 202, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:202>>;
    {_, 203, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:203>>;
    {_, 204, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:204>>;
    {_, 205, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:205>>;
    {_, 206, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:206>>;
    {_, 207, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:207>>;
    {_, 208, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:208>>;
    {_, 209, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:209>>;
    {_, 210, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:210>>;
    {_, 211, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:211>>;
    {_, 212, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:212>>;
    {_, 213, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:213>>;
    {_, 214, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:214>>;
    {_, 215, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:215>>;
    {_, 216, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:216>>;
    {_, 217, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:217>>;
    {_, 218, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:218>>;
    {_, 219, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:219>>;
    {_, 220, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:220>>;
    {_, 221, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:221>>;
    {_, 222, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:222>>;
    {_, 223, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:223>>;
    {_, 224, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:224>>;
    {_, 225, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:225>>;
    {_, 226, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:226>>;
    {_, 227, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:227>>;
    {_, 228, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:228>>;
    {_, 229, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:229>>;
    {_, 230, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:230>>;
    {_, 231, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:231>>;
    {_, 232, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:232>>;
    {_, 233, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:233>>;
    {_, 234, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:234>>;
    {_, 235, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:235>>;
    {_, 236, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:236>>;
    {_, 237, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:237>>;
    {_, 238, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:238>>;
    {_, 239, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:239>>;
    {_, 240, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:240>>;
    {_, 241, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:241>>;
    {_, 242, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:242>>;
    {_, 243, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:243>>;
    {_, 244, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:244>>;
    {_, 245, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:245>>;
    {_, 246, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:246>>;
    {_, 247, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:247>>;
    {_, 248, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:248>>;
    {_, 249, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:249>>;
    {_, 250, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:250>>;
    {_, 251, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:251>>;
    {_, 252, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:252>>;
    {_, 253, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:253>>;
    {_, 254, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:254>>;
    {_, 255, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:255>>;
    {_, 256, float, signed, little} ->
      <<Val:Size/signed-little-float-unit:256>>;
    {all, 1, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:1>>;
    {all, 2, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:2>>;
    {all, 3, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:3>>;
    {all, 4, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:4>>;
    {all, 5, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:5>>;
    {all, 6, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:6>>;
    {all, 7, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:7>>;
    {all, 8, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:8>>;
    {all, 9, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:9>>;
    {all, 10, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:10>>;
    {all, 11, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:11>>;
    {all, 12, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:12>>;
    {all, 13, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:13>>;
    {all, 14, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:14>>;
    {all, 15, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:15>>;
    {all, 16, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:16>>;
    {all, 17, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:17>>;
    {all, 18, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:18>>;
    {all, 19, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:19>>;
    {all, 20, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:20>>;
    {all, 21, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:21>>;
    {all, 22, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:22>>;
    {all, 23, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:23>>;
    {all, 24, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:24>>;
    {all, 25, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:25>>;
    {all, 26, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:26>>;
    {all, 27, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:27>>;
    {all, 28, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:28>>;
    {all, 29, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:29>>;
    {all, 30, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:30>>;
    {all, 31, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:31>>;
    {all, 32, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:32>>;
    {all, 33, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:33>>;
    {all, 34, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:34>>;
    {all, 35, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:35>>;
    {all, 36, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:36>>;
    {all, 37, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:37>>;
    {all, 38, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:38>>;
    {all, 39, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:39>>;
    {all, 40, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:40>>;
    {all, 41, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:41>>;
    {all, 42, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:42>>;
    {all, 43, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:43>>;
    {all, 44, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:44>>;
    {all, 45, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:45>>;
    {all, 46, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:46>>;
    {all, 47, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:47>>;
    {all, 48, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:48>>;
    {all, 49, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:49>>;
    {all, 50, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:50>>;
    {all, 51, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:51>>;
    {all, 52, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:52>>;
    {all, 53, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:53>>;
    {all, 54, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:54>>;
    {all, 55, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:55>>;
    {all, 56, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:56>>;
    {all, 57, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:57>>;
    {all, 58, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:58>>;
    {all, 59, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:59>>;
    {all, 60, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:60>>;
    {all, 61, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:61>>;
    {all, 62, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:62>>;
    {all, 63, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:63>>;
    {all, 64, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:64>>;
    {all, 65, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:65>>;
    {all, 66, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:66>>;
    {all, 67, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:67>>;
    {all, 68, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:68>>;
    {all, 69, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:69>>;
    {all, 70, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:70>>;
    {all, 71, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:71>>;
    {all, 72, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:72>>;
    {all, 73, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:73>>;
    {all, 74, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:74>>;
    {all, 75, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:75>>;
    {all, 76, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:76>>;
    {all, 77, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:77>>;
    {all, 78, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:78>>;
    {all, 79, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:79>>;
    {all, 80, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:80>>;
    {all, 81, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:81>>;
    {all, 82, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:82>>;
    {all, 83, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:83>>;
    {all, 84, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:84>>;
    {all, 85, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:85>>;
    {all, 86, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:86>>;
    {all, 87, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:87>>;
    {all, 88, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:88>>;
    {all, 89, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:89>>;
    {all, 90, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:90>>;
    {all, 91, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:91>>;
    {all, 92, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:92>>;
    {all, 93, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:93>>;
    {all, 94, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:94>>;
    {all, 95, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:95>>;
    {all, 96, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:96>>;
    {all, 97, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:97>>;
    {all, 98, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:98>>;
    {all, 99, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:99>>;
    {all, 100, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:100>>;
    {all, 101, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:101>>;
    {all, 102, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:102>>;
    {all, 103, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:103>>;
    {all, 104, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:104>>;
    {all, 105, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:105>>;
    {all, 106, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:106>>;
    {all, 107, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:107>>;
    {all, 108, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:108>>;
    {all, 109, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:109>>;
    {all, 110, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:110>>;
    {all, 111, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:111>>;
    {all, 112, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:112>>;
    {all, 113, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:113>>;
    {all, 114, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:114>>;
    {all, 115, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:115>>;
    {all, 116, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:116>>;
    {all, 117, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:117>>;
    {all, 118, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:118>>;
    {all, 119, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:119>>;
    {all, 120, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:120>>;
    {all, 121, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:121>>;
    {all, 122, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:122>>;
    {all, 123, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:123>>;
    {all, 124, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:124>>;
    {all, 125, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:125>>;
    {all, 126, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:126>>;
    {all, 127, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:127>>;
    {all, 128, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:128>>;
    {all, 129, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:129>>;
    {all, 130, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:130>>;
    {all, 131, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:131>>;
    {all, 132, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:132>>;
    {all, 133, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:133>>;
    {all, 134, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:134>>;
    {all, 135, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:135>>;
    {all, 136, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:136>>;
    {all, 137, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:137>>;
    {all, 138, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:138>>;
    {all, 139, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:139>>;
    {all, 140, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:140>>;
    {all, 141, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:141>>;
    {all, 142, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:142>>;
    {all, 143, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:143>>;
    {all, 144, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:144>>;
    {all, 145, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:145>>;
    {all, 146, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:146>>;
    {all, 147, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:147>>;
    {all, 148, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:148>>;
    {all, 149, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:149>>;
    {all, 150, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:150>>;
    {all, 151, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:151>>;
    {all, 152, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:152>>;
    {all, 153, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:153>>;
    {all, 154, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:154>>;
    {all, 155, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:155>>;
    {all, 156, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:156>>;
    {all, 157, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:157>>;
    {all, 158, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:158>>;
    {all, 159, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:159>>;
    {all, 160, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:160>>;
    {all, 161, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:161>>;
    {all, 162, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:162>>;
    {all, 163, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:163>>;
    {all, 164, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:164>>;
    {all, 165, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:165>>;
    {all, 166, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:166>>;
    {all, 167, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:167>>;
    {all, 168, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:168>>;
    {all, 169, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:169>>;
    {all, 170, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:170>>;
    {all, 171, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:171>>;
    {all, 172, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:172>>;
    {all, 173, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:173>>;
    {all, 174, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:174>>;
    {all, 175, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:175>>;
    {all, 176, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:176>>;
    {all, 177, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:177>>;
    {all, 178, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:178>>;
    {all, 179, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:179>>;
    {all, 180, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:180>>;
    {all, 181, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:181>>;
    {all, 182, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:182>>;
    {all, 183, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:183>>;
    {all, 184, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:184>>;
    {all, 185, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:185>>;
    {all, 186, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:186>>;
    {all, 187, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:187>>;
    {all, 188, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:188>>;
    {all, 189, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:189>>;
    {all, 190, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:190>>;
    {all, 191, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:191>>;
    {all, 192, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:192>>;
    {all, 193, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:193>>;
    {all, 194, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:194>>;
    {all, 195, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:195>>;
    {all, 196, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:196>>;
    {all, 197, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:197>>;
    {all, 198, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:198>>;
    {all, 199, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:199>>;
    {all, 200, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:200>>;
    {all, 201, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:201>>;
    {all, 202, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:202>>;
    {all, 203, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:203>>;
    {all, 204, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:204>>;
    {all, 205, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:205>>;
    {all, 206, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:206>>;
    {all, 207, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:207>>;
    {all, 208, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:208>>;
    {all, 209, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:209>>;
    {all, 210, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:210>>;
    {all, 211, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:211>>;
    {all, 212, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:212>>;
    {all, 213, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:213>>;
    {all, 214, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:214>>;
    {all, 215, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:215>>;
    {all, 216, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:216>>;
    {all, 217, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:217>>;
    {all, 218, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:218>>;
    {all, 219, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:219>>;
    {all, 220, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:220>>;
    {all, 221, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:221>>;
    {all, 222, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:222>>;
    {all, 223, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:223>>;
    {all, 224, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:224>>;
    {all, 225, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:225>>;
    {all, 226, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:226>>;
    {all, 227, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:227>>;
    {all, 228, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:228>>;
    {all, 229, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:229>>;
    {all, 230, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:230>>;
    {all, 231, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:231>>;
    {all, 232, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:232>>;
    {all, 233, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:233>>;
    {all, 234, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:234>>;
    {all, 235, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:235>>;
    {all, 236, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:236>>;
    {all, 237, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:237>>;
    {all, 238, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:238>>;
    {all, 239, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:239>>;
    {all, 240, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:240>>;
    {all, 241, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:241>>;
    {all, 242, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:242>>;
    {all, 243, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:243>>;
    {all, 244, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:244>>;
    {all, 245, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:245>>;
    {all, 246, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:246>>;
    {all, 247, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:247>>;
    {all, 248, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:248>>;
    {all, 249, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:249>>;
    {all, 250, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:250>>;
    {all, 251, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:251>>;
    {all, 252, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:252>>;
    {all, 253, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:253>>;
    {all, 254, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:254>>;
    {all, 255, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:255>>;
    {all, 256, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:256>>;
    {_, 1, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:1>>;
    {_, 2, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:2>>;
    {_, 3, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:3>>;
    {_, 4, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:4>>;
    {_, 5, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:5>>;
    {_, 6, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:6>>;
    {_, 7, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:7>>;
    {_, 8, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:8>>;
    {_, 9, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:9>>;
    {_, 10, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:10>>;
    {_, 11, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:11>>;
    {_, 12, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:12>>;
    {_, 13, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:13>>;
    {_, 14, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:14>>;
    {_, 15, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:15>>;
    {_, 16, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:16>>;
    {_, 17, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:17>>;
    {_, 18, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:18>>;
    {_, 19, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:19>>;
    {_, 20, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:20>>;
    {_, 21, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:21>>;
    {_, 22, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:22>>;
    {_, 23, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:23>>;
    {_, 24, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:24>>;
    {_, 25, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:25>>;
    {_, 26, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:26>>;
    {_, 27, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:27>>;
    {_, 28, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:28>>;
    {_, 29, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:29>>;
    {_, 30, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:30>>;
    {_, 31, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:31>>;
    {_, 32, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:32>>;
    {_, 33, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:33>>;
    {_, 34, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:34>>;
    {_, 35, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:35>>;
    {_, 36, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:36>>;
    {_, 37, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:37>>;
    {_, 38, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:38>>;
    {_, 39, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:39>>;
    {_, 40, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:40>>;
    {_, 41, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:41>>;
    {_, 42, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:42>>;
    {_, 43, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:43>>;
    {_, 44, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:44>>;
    {_, 45, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:45>>;
    {_, 46, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:46>>;
    {_, 47, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:47>>;
    {_, 48, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:48>>;
    {_, 49, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:49>>;
    {_, 50, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:50>>;
    {_, 51, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:51>>;
    {_, 52, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:52>>;
    {_, 53, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:53>>;
    {_, 54, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:54>>;
    {_, 55, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:55>>;
    {_, 56, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:56>>;
    {_, 57, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:57>>;
    {_, 58, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:58>>;
    {_, 59, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:59>>;
    {_, 60, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:60>>;
    {_, 61, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:61>>;
    {_, 62, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:62>>;
    {_, 63, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:63>>;
    {_, 64, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:64>>;
    {_, 65, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:65>>;
    {_, 66, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:66>>;
    {_, 67, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:67>>;
    {_, 68, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:68>>;
    {_, 69, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:69>>;
    {_, 70, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:70>>;
    {_, 71, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:71>>;
    {_, 72, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:72>>;
    {_, 73, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:73>>;
    {_, 74, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:74>>;
    {_, 75, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:75>>;
    {_, 76, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:76>>;
    {_, 77, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:77>>;
    {_, 78, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:78>>;
    {_, 79, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:79>>;
    {_, 80, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:80>>;
    {_, 81, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:81>>;
    {_, 82, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:82>>;
    {_, 83, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:83>>;
    {_, 84, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:84>>;
    {_, 85, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:85>>;
    {_, 86, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:86>>;
    {_, 87, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:87>>;
    {_, 88, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:88>>;
    {_, 89, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:89>>;
    {_, 90, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:90>>;
    {_, 91, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:91>>;
    {_, 92, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:92>>;
    {_, 93, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:93>>;
    {_, 94, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:94>>;
    {_, 95, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:95>>;
    {_, 96, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:96>>;
    {_, 97, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:97>>;
    {_, 98, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:98>>;
    {_, 99, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:99>>;
    {_, 100, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:100>>;
    {_, 101, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:101>>;
    {_, 102, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:102>>;
    {_, 103, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:103>>;
    {_, 104, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:104>>;
    {_, 105, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:105>>;
    {_, 106, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:106>>;
    {_, 107, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:107>>;
    {_, 108, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:108>>;
    {_, 109, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:109>>;
    {_, 110, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:110>>;
    {_, 111, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:111>>;
    {_, 112, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:112>>;
    {_, 113, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:113>>;
    {_, 114, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:114>>;
    {_, 115, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:115>>;
    {_, 116, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:116>>;
    {_, 117, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:117>>;
    {_, 118, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:118>>;
    {_, 119, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:119>>;
    {_, 120, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:120>>;
    {_, 121, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:121>>;
    {_, 122, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:122>>;
    {_, 123, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:123>>;
    {_, 124, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:124>>;
    {_, 125, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:125>>;
    {_, 126, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:126>>;
    {_, 127, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:127>>;
    {_, 128, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:128>>;
    {_, 129, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:129>>;
    {_, 130, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:130>>;
    {_, 131, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:131>>;
    {_, 132, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:132>>;
    {_, 133, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:133>>;
    {_, 134, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:134>>;
    {_, 135, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:135>>;
    {_, 136, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:136>>;
    {_, 137, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:137>>;
    {_, 138, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:138>>;
    {_, 139, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:139>>;
    {_, 140, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:140>>;
    {_, 141, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:141>>;
    {_, 142, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:142>>;
    {_, 143, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:143>>;
    {_, 144, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:144>>;
    {_, 145, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:145>>;
    {_, 146, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:146>>;
    {_, 147, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:147>>;
    {_, 148, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:148>>;
    {_, 149, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:149>>;
    {_, 150, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:150>>;
    {_, 151, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:151>>;
    {_, 152, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:152>>;
    {_, 153, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:153>>;
    {_, 154, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:154>>;
    {_, 155, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:155>>;
    {_, 156, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:156>>;
    {_, 157, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:157>>;
    {_, 158, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:158>>;
    {_, 159, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:159>>;
    {_, 160, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:160>>;
    {_, 161, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:161>>;
    {_, 162, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:162>>;
    {_, 163, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:163>>;
    {_, 164, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:164>>;
    {_, 165, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:165>>;
    {_, 166, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:166>>;
    {_, 167, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:167>>;
    {_, 168, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:168>>;
    {_, 169, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:169>>;
    {_, 170, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:170>>;
    {_, 171, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:171>>;
    {_, 172, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:172>>;
    {_, 173, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:173>>;
    {_, 174, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:174>>;
    {_, 175, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:175>>;
    {_, 176, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:176>>;
    {_, 177, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:177>>;
    {_, 178, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:178>>;
    {_, 179, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:179>>;
    {_, 180, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:180>>;
    {_, 181, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:181>>;
    {_, 182, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:182>>;
    {_, 183, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:183>>;
    {_, 184, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:184>>;
    {_, 185, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:185>>;
    {_, 186, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:186>>;
    {_, 187, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:187>>;
    {_, 188, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:188>>;
    {_, 189, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:189>>;
    {_, 190, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:190>>;
    {_, 191, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:191>>;
    {_, 192, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:192>>;
    {_, 193, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:193>>;
    {_, 194, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:194>>;
    {_, 195, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:195>>;
    {_, 196, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:196>>;
    {_, 197, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:197>>;
    {_, 198, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:198>>;
    {_, 199, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:199>>;
    {_, 200, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:200>>;
    {_, 201, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:201>>;
    {_, 202, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:202>>;
    {_, 203, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:203>>;
    {_, 204, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:204>>;
    {_, 205, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:205>>;
    {_, 206, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:206>>;
    {_, 207, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:207>>;
    {_, 208, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:208>>;
    {_, 209, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:209>>;
    {_, 210, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:210>>;
    {_, 211, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:211>>;
    {_, 212, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:212>>;
    {_, 213, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:213>>;
    {_, 214, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:214>>;
    {_, 215, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:215>>;
    {_, 216, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:216>>;
    {_, 217, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:217>>;
    {_, 218, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:218>>;
    {_, 219, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:219>>;
    {_, 220, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:220>>;
    {_, 221, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:221>>;
    {_, 222, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:222>>;
    {_, 223, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:223>>;
    {_, 224, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:224>>;
    {_, 225, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:225>>;
    {_, 226, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:226>>;
    {_, 227, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:227>>;
    {_, 228, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:228>>;
    {_, 229, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:229>>;
    {_, 230, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:230>>;
    {_, 231, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:231>>;
    {_, 232, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:232>>;
    {_, 233, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:233>>;
    {_, 234, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:234>>;
    {_, 235, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:235>>;
    {_, 236, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:236>>;
    {_, 237, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:237>>;
    {_, 238, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:238>>;
    {_, 239, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:239>>;
    {_, 240, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:240>>;
    {_, 241, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:241>>;
    {_, 242, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:242>>;
    {_, 243, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:243>>;
    {_, 244, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:244>>;
    {_, 245, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:245>>;
    {_, 246, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:246>>;
    {_, 247, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:247>>;
    {_, 248, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:248>>;
    {_, 249, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:249>>;
    {_, 250, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:250>>;
    {_, 251, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:251>>;
    {_, 252, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:252>>;
    {_, 253, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:253>>;
    {_, 254, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:254>>;
    {_, 255, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:255>>;
    {_, 256, binary, signed, little} ->
      <<Val:Size/signed-little-binary-unit:256>>;
    {_, 1, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:1>>;
    {_, 2, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:2>>;
    {_, 3, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:3>>;
    {_, 4, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:4>>;
    {_, 5, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:5>>;
    {_, 6, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:6>>;
    {_, 7, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:7>>;
    {_, 8, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:8>>;
    {_, 9, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:9>>;
    {_, 10, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:10>>;
    {_, 11, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:11>>;
    {_, 12, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:12>>;
    {_, 13, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:13>>;
    {_, 14, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:14>>;
    {_, 15, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:15>>;
    {_, 16, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:16>>;
    {_, 17, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:17>>;
    {_, 18, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:18>>;
    {_, 19, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:19>>;
    {_, 20, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:20>>;
    {_, 21, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:21>>;
    {_, 22, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:22>>;
    {_, 23, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:23>>;
    {_, 24, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:24>>;
    {_, 25, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:25>>;
    {_, 26, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:26>>;
    {_, 27, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:27>>;
    {_, 28, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:28>>;
    {_, 29, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:29>>;
    {_, 30, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:30>>;
    {_, 31, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:31>>;
    {_, 32, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:32>>;
    {_, 33, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:33>>;
    {_, 34, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:34>>;
    {_, 35, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:35>>;
    {_, 36, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:36>>;
    {_, 37, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:37>>;
    {_, 38, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:38>>;
    {_, 39, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:39>>;
    {_, 40, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:40>>;
    {_, 41, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:41>>;
    {_, 42, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:42>>;
    {_, 43, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:43>>;
    {_, 44, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:44>>;
    {_, 45, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:45>>;
    {_, 46, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:46>>;
    {_, 47, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:47>>;
    {_, 48, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:48>>;
    {_, 49, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:49>>;
    {_, 50, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:50>>;
    {_, 51, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:51>>;
    {_, 52, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:52>>;
    {_, 53, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:53>>;
    {_, 54, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:54>>;
    {_, 55, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:55>>;
    {_, 56, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:56>>;
    {_, 57, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:57>>;
    {_, 58, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:58>>;
    {_, 59, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:59>>;
    {_, 60, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:60>>;
    {_, 61, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:61>>;
    {_, 62, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:62>>;
    {_, 63, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:63>>;
    {_, 64, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:64>>;
    {_, 65, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:65>>;
    {_, 66, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:66>>;
    {_, 67, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:67>>;
    {_, 68, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:68>>;
    {_, 69, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:69>>;
    {_, 70, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:70>>;
    {_, 71, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:71>>;
    {_, 72, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:72>>;
    {_, 73, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:73>>;
    {_, 74, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:74>>;
    {_, 75, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:75>>;
    {_, 76, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:76>>;
    {_, 77, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:77>>;
    {_, 78, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:78>>;
    {_, 79, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:79>>;
    {_, 80, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:80>>;
    {_, 81, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:81>>;
    {_, 82, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:82>>;
    {_, 83, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:83>>;
    {_, 84, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:84>>;
    {_, 85, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:85>>;
    {_, 86, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:86>>;
    {_, 87, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:87>>;
    {_, 88, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:88>>;
    {_, 89, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:89>>;
    {_, 90, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:90>>;
    {_, 91, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:91>>;
    {_, 92, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:92>>;
    {_, 93, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:93>>;
    {_, 94, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:94>>;
    {_, 95, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:95>>;
    {_, 96, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:96>>;
    {_, 97, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:97>>;
    {_, 98, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:98>>;
    {_, 99, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:99>>;
    {_, 100, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:100>>;
    {_, 101, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:101>>;
    {_, 102, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:102>>;
    {_, 103, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:103>>;
    {_, 104, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:104>>;
    {_, 105, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:105>>;
    {_, 106, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:106>>;
    {_, 107, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:107>>;
    {_, 108, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:108>>;
    {_, 109, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:109>>;
    {_, 110, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:110>>;
    {_, 111, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:111>>;
    {_, 112, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:112>>;
    {_, 113, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:113>>;
    {_, 114, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:114>>;
    {_, 115, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:115>>;
    {_, 116, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:116>>;
    {_, 117, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:117>>;
    {_, 118, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:118>>;
    {_, 119, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:119>>;
    {_, 120, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:120>>;
    {_, 121, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:121>>;
    {_, 122, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:122>>;
    {_, 123, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:123>>;
    {_, 124, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:124>>;
    {_, 125, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:125>>;
    {_, 126, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:126>>;
    {_, 127, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:127>>;
    {_, 128, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:128>>;
    {_, 129, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:129>>;
    {_, 130, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:130>>;
    {_, 131, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:131>>;
    {_, 132, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:132>>;
    {_, 133, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:133>>;
    {_, 134, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:134>>;
    {_, 135, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:135>>;
    {_, 136, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:136>>;
    {_, 137, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:137>>;
    {_, 138, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:138>>;
    {_, 139, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:139>>;
    {_, 140, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:140>>;
    {_, 141, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:141>>;
    {_, 142, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:142>>;
    {_, 143, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:143>>;
    {_, 144, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:144>>;
    {_, 145, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:145>>;
    {_, 146, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:146>>;
    {_, 147, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:147>>;
    {_, 148, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:148>>;
    {_, 149, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:149>>;
    {_, 150, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:150>>;
    {_, 151, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:151>>;
    {_, 152, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:152>>;
    {_, 153, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:153>>;
    {_, 154, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:154>>;
    {_, 155, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:155>>;
    {_, 156, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:156>>;
    {_, 157, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:157>>;
    {_, 158, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:158>>;
    {_, 159, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:159>>;
    {_, 160, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:160>>;
    {_, 161, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:161>>;
    {_, 162, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:162>>;
    {_, 163, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:163>>;
    {_, 164, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:164>>;
    {_, 165, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:165>>;
    {_, 166, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:166>>;
    {_, 167, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:167>>;
    {_, 168, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:168>>;
    {_, 169, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:169>>;
    {_, 170, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:170>>;
    {_, 171, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:171>>;
    {_, 172, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:172>>;
    {_, 173, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:173>>;
    {_, 174, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:174>>;
    {_, 175, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:175>>;
    {_, 176, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:176>>;
    {_, 177, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:177>>;
    {_, 178, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:178>>;
    {_, 179, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:179>>;
    {_, 180, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:180>>;
    {_, 181, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:181>>;
    {_, 182, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:182>>;
    {_, 183, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:183>>;
    {_, 184, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:184>>;
    {_, 185, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:185>>;
    {_, 186, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:186>>;
    {_, 187, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:187>>;
    {_, 188, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:188>>;
    {_, 189, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:189>>;
    {_, 190, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:190>>;
    {_, 191, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:191>>;
    {_, 192, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:192>>;
    {_, 193, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:193>>;
    {_, 194, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:194>>;
    {_, 195, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:195>>;
    {_, 196, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:196>>;
    {_, 197, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:197>>;
    {_, 198, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:198>>;
    {_, 199, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:199>>;
    {_, 200, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:200>>;
    {_, 201, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:201>>;
    {_, 202, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:202>>;
    {_, 203, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:203>>;
    {_, 204, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:204>>;
    {_, 205, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:205>>;
    {_, 206, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:206>>;
    {_, 207, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:207>>;
    {_, 208, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:208>>;
    {_, 209, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:209>>;
    {_, 210, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:210>>;
    {_, 211, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:211>>;
    {_, 212, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:212>>;
    {_, 213, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:213>>;
    {_, 214, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:214>>;
    {_, 215, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:215>>;
    {_, 216, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:216>>;
    {_, 217, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:217>>;
    {_, 218, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:218>>;
    {_, 219, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:219>>;
    {_, 220, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:220>>;
    {_, 221, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:221>>;
    {_, 222, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:222>>;
    {_, 223, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:223>>;
    {_, 224, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:224>>;
    {_, 225, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:225>>;
    {_, 226, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:226>>;
    {_, 227, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:227>>;
    {_, 228, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:228>>;
    {_, 229, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:229>>;
    {_, 230, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:230>>;
    {_, 231, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:231>>;
    {_, 232, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:232>>;
    {_, 233, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:233>>;
    {_, 234, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:234>>;
    {_, 235, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:235>>;
    {_, 236, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:236>>;
    {_, 237, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:237>>;
    {_, 238, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:238>>;
    {_, 239, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:239>>;
    {_, 240, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:240>>;
    {_, 241, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:241>>;
    {_, 242, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:242>>;
    {_, 243, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:243>>;
    {_, 244, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:244>>;
    {_, 245, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:245>>;
    {_, 246, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:246>>;
    {_, 247, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:247>>;
    {_, 248, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:248>>;
    {_, 249, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:249>>;
    {_, 250, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:250>>;
    {_, 251, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:251>>;
    {_, 252, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:252>>;
    {_, 253, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:253>>;
    {_, 254, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:254>>;
    {_, 255, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:255>>;
    {_, 256, integer, unsigned, little} ->
      <<Val:Size/unsigned-little-integer-unit:256>>;
    {_, 1, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:1>>;
    {_, 2, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:2>>;
    {_, 3, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:3>>;
    {_, 4, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:4>>;
    {_, 5, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:5>>;
    {_, 6, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:6>>;
    {_, 7, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:7>>;
    {_, 8, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:8>>;
    {_, 9, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:9>>;
    {_, 10, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:10>>;
    {_, 11, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:11>>;
    {_, 12, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:12>>;
    {_, 13, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:13>>;
    {_, 14, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:14>>;
    {_, 15, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:15>>;
    {_, 16, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:16>>;
    {_, 17, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:17>>;
    {_, 18, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:18>>;
    {_, 19, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:19>>;
    {_, 20, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:20>>;
    {_, 21, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:21>>;
    {_, 22, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:22>>;
    {_, 23, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:23>>;
    {_, 24, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:24>>;
    {_, 25, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:25>>;
    {_, 26, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:26>>;
    {_, 27, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:27>>;
    {_, 28, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:28>>;
    {_, 29, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:29>>;
    {_, 30, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:30>>;
    {_, 31, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:31>>;
    {_, 32, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:32>>;
    {_, 33, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:33>>;
    {_, 34, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:34>>;
    {_, 35, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:35>>;
    {_, 36, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:36>>;
    {_, 37, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:37>>;
    {_, 38, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:38>>;
    {_, 39, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:39>>;
    {_, 40, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:40>>;
    {_, 41, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:41>>;
    {_, 42, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:42>>;
    {_, 43, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:43>>;
    {_, 44, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:44>>;
    {_, 45, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:45>>;
    {_, 46, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:46>>;
    {_, 47, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:47>>;
    {_, 48, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:48>>;
    {_, 49, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:49>>;
    {_, 50, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:50>>;
    {_, 51, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:51>>;
    {_, 52, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:52>>;
    {_, 53, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:53>>;
    {_, 54, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:54>>;
    {_, 55, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:55>>;
    {_, 56, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:56>>;
    {_, 57, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:57>>;
    {_, 58, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:58>>;
    {_, 59, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:59>>;
    {_, 60, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:60>>;
    {_, 61, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:61>>;
    {_, 62, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:62>>;
    {_, 63, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:63>>;
    {_, 64, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:64>>;
    {_, 65, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:65>>;
    {_, 66, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:66>>;
    {_, 67, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:67>>;
    {_, 68, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:68>>;
    {_, 69, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:69>>;
    {_, 70, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:70>>;
    {_, 71, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:71>>;
    {_, 72, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:72>>;
    {_, 73, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:73>>;
    {_, 74, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:74>>;
    {_, 75, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:75>>;
    {_, 76, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:76>>;
    {_, 77, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:77>>;
    {_, 78, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:78>>;
    {_, 79, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:79>>;
    {_, 80, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:80>>;
    {_, 81, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:81>>;
    {_, 82, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:82>>;
    {_, 83, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:83>>;
    {_, 84, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:84>>;
    {_, 85, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:85>>;
    {_, 86, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:86>>;
    {_, 87, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:87>>;
    {_, 88, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:88>>;
    {_, 89, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:89>>;
    {_, 90, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:90>>;
    {_, 91, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:91>>;
    {_, 92, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:92>>;
    {_, 93, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:93>>;
    {_, 94, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:94>>;
    {_, 95, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:95>>;
    {_, 96, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:96>>;
    {_, 97, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:97>>;
    {_, 98, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:98>>;
    {_, 99, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:99>>;
    {_, 100, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:100>>;
    {_, 101, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:101>>;
    {_, 102, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:102>>;
    {_, 103, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:103>>;
    {_, 104, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:104>>;
    {_, 105, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:105>>;
    {_, 106, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:106>>;
    {_, 107, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:107>>;
    {_, 108, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:108>>;
    {_, 109, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:109>>;
    {_, 110, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:110>>;
    {_, 111, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:111>>;
    {_, 112, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:112>>;
    {_, 113, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:113>>;
    {_, 114, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:114>>;
    {_, 115, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:115>>;
    {_, 116, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:116>>;
    {_, 117, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:117>>;
    {_, 118, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:118>>;
    {_, 119, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:119>>;
    {_, 120, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:120>>;
    {_, 121, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:121>>;
    {_, 122, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:122>>;
    {_, 123, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:123>>;
    {_, 124, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:124>>;
    {_, 125, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:125>>;
    {_, 126, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:126>>;
    {_, 127, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:127>>;
    {_, 128, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:128>>;
    {_, 129, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:129>>;
    {_, 130, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:130>>;
    {_, 131, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:131>>;
    {_, 132, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:132>>;
    {_, 133, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:133>>;
    {_, 134, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:134>>;
    {_, 135, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:135>>;
    {_, 136, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:136>>;
    {_, 137, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:137>>;
    {_, 138, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:138>>;
    {_, 139, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:139>>;
    {_, 140, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:140>>;
    {_, 141, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:141>>;
    {_, 142, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:142>>;
    {_, 143, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:143>>;
    {_, 144, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:144>>;
    {_, 145, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:145>>;
    {_, 146, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:146>>;
    {_, 147, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:147>>;
    {_, 148, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:148>>;
    {_, 149, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:149>>;
    {_, 150, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:150>>;
    {_, 151, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:151>>;
    {_, 152, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:152>>;
    {_, 153, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:153>>;
    {_, 154, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:154>>;
    {_, 155, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:155>>;
    {_, 156, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:156>>;
    {_, 157, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:157>>;
    {_, 158, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:158>>;
    {_, 159, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:159>>;
    {_, 160, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:160>>;
    {_, 161, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:161>>;
    {_, 162, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:162>>;
    {_, 163, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:163>>;
    {_, 164, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:164>>;
    {_, 165, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:165>>;
    {_, 166, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:166>>;
    {_, 167, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:167>>;
    {_, 168, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:168>>;
    {_, 169, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:169>>;
    {_, 170, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:170>>;
    {_, 171, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:171>>;
    {_, 172, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:172>>;
    {_, 173, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:173>>;
    {_, 174, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:174>>;
    {_, 175, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:175>>;
    {_, 176, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:176>>;
    {_, 177, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:177>>;
    {_, 178, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:178>>;
    {_, 179, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:179>>;
    {_, 180, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:180>>;
    {_, 181, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:181>>;
    {_, 182, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:182>>;
    {_, 183, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:183>>;
    {_, 184, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:184>>;
    {_, 185, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:185>>;
    {_, 186, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:186>>;
    {_, 187, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:187>>;
    {_, 188, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:188>>;
    {_, 189, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:189>>;
    {_, 190, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:190>>;
    {_, 191, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:191>>;
    {_, 192, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:192>>;
    {_, 193, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:193>>;
    {_, 194, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:194>>;
    {_, 195, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:195>>;
    {_, 196, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:196>>;
    {_, 197, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:197>>;
    {_, 198, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:198>>;
    {_, 199, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:199>>;
    {_, 200, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:200>>;
    {_, 201, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:201>>;
    {_, 202, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:202>>;
    {_, 203, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:203>>;
    {_, 204, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:204>>;
    {_, 205, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:205>>;
    {_, 206, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:206>>;
    {_, 207, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:207>>;
    {_, 208, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:208>>;
    {_, 209, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:209>>;
    {_, 210, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:210>>;
    {_, 211, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:211>>;
    {_, 212, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:212>>;
    {_, 213, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:213>>;
    {_, 214, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:214>>;
    {_, 215, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:215>>;
    {_, 216, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:216>>;
    {_, 217, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:217>>;
    {_, 218, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:218>>;
    {_, 219, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:219>>;
    {_, 220, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:220>>;
    {_, 221, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:221>>;
    {_, 222, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:222>>;
    {_, 223, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:223>>;
    {_, 224, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:224>>;
    {_, 225, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:225>>;
    {_, 226, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:226>>;
    {_, 227, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:227>>;
    {_, 228, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:228>>;
    {_, 229, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:229>>;
    {_, 230, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:230>>;
    {_, 231, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:231>>;
    {_, 232, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:232>>;
    {_, 233, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:233>>;
    {_, 234, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:234>>;
    {_, 235, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:235>>;
    {_, 236, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:236>>;
    {_, 237, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:237>>;
    {_, 238, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:238>>;
    {_, 239, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:239>>;
    {_, 240, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:240>>;
    {_, 241, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:241>>;
    {_, 242, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:242>>;
    {_, 243, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:243>>;
    {_, 244, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:244>>;
    {_, 245, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:245>>;
    {_, 246, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:246>>;
    {_, 247, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:247>>;
    {_, 248, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:248>>;
    {_, 249, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:249>>;
    {_, 250, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:250>>;
    {_, 251, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:251>>;
    {_, 252, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:252>>;
    {_, 253, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:253>>;
    {_, 254, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:254>>;
    {_, 255, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:255>>;
    {_, 256, float, unsigned, little} ->
      <<Val:Size/unsigned-little-float-unit:256>>;
    {all, 1, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:1>>;
    {all, 2, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:2>>;
    {all, 3, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:3>>;
    {all, 4, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:4>>;
    {all, 5, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:5>>;
    {all, 6, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:6>>;
    {all, 7, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:7>>;
    {all, 8, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:8>>;
    {all, 9, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:9>>;
    {all, 10, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:10>>;
    {all, 11, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:11>>;
    {all, 12, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:12>>;
    {all, 13, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:13>>;
    {all, 14, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:14>>;
    {all, 15, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:15>>;
    {all, 16, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:16>>;
    {all, 17, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:17>>;
    {all, 18, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:18>>;
    {all, 19, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:19>>;
    {all, 20, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:20>>;
    {all, 21, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:21>>;
    {all, 22, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:22>>;
    {all, 23, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:23>>;
    {all, 24, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:24>>;
    {all, 25, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:25>>;
    {all, 26, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:26>>;
    {all, 27, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:27>>;
    {all, 28, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:28>>;
    {all, 29, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:29>>;
    {all, 30, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:30>>;
    {all, 31, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:31>>;
    {all, 32, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:32>>;
    {all, 33, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:33>>;
    {all, 34, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:34>>;
    {all, 35, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:35>>;
    {all, 36, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:36>>;
    {all, 37, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:37>>;
    {all, 38, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:38>>;
    {all, 39, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:39>>;
    {all, 40, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:40>>;
    {all, 41, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:41>>;
    {all, 42, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:42>>;
    {all, 43, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:43>>;
    {all, 44, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:44>>;
    {all, 45, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:45>>;
    {all, 46, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:46>>;
    {all, 47, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:47>>;
    {all, 48, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:48>>;
    {all, 49, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:49>>;
    {all, 50, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:50>>;
    {all, 51, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:51>>;
    {all, 52, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:52>>;
    {all, 53, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:53>>;
    {all, 54, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:54>>;
    {all, 55, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:55>>;
    {all, 56, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:56>>;
    {all, 57, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:57>>;
    {all, 58, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:58>>;
    {all, 59, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:59>>;
    {all, 60, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:60>>;
    {all, 61, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:61>>;
    {all, 62, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:62>>;
    {all, 63, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:63>>;
    {all, 64, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:64>>;
    {all, 65, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:65>>;
    {all, 66, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:66>>;
    {all, 67, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:67>>;
    {all, 68, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:68>>;
    {all, 69, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:69>>;
    {all, 70, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:70>>;
    {all, 71, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:71>>;
    {all, 72, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:72>>;
    {all, 73, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:73>>;
    {all, 74, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:74>>;
    {all, 75, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:75>>;
    {all, 76, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:76>>;
    {all, 77, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:77>>;
    {all, 78, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:78>>;
    {all, 79, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:79>>;
    {all, 80, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:80>>;
    {all, 81, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:81>>;
    {all, 82, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:82>>;
    {all, 83, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:83>>;
    {all, 84, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:84>>;
    {all, 85, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:85>>;
    {all, 86, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:86>>;
    {all, 87, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:87>>;
    {all, 88, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:88>>;
    {all, 89, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:89>>;
    {all, 90, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:90>>;
    {all, 91, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:91>>;
    {all, 92, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:92>>;
    {all, 93, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:93>>;
    {all, 94, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:94>>;
    {all, 95, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:95>>;
    {all, 96, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:96>>;
    {all, 97, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:97>>;
    {all, 98, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:98>>;
    {all, 99, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:99>>;
    {all, 100, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:100>>;
    {all, 101, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:101>>;
    {all, 102, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:102>>;
    {all, 103, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:103>>;
    {all, 104, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:104>>;
    {all, 105, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:105>>;
    {all, 106, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:106>>;
    {all, 107, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:107>>;
    {all, 108, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:108>>;
    {all, 109, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:109>>;
    {all, 110, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:110>>;
    {all, 111, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:111>>;
    {all, 112, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:112>>;
    {all, 113, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:113>>;
    {all, 114, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:114>>;
    {all, 115, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:115>>;
    {all, 116, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:116>>;
    {all, 117, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:117>>;
    {all, 118, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:118>>;
    {all, 119, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:119>>;
    {all, 120, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:120>>;
    {all, 121, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:121>>;
    {all, 122, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:122>>;
    {all, 123, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:123>>;
    {all, 124, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:124>>;
    {all, 125, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:125>>;
    {all, 126, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:126>>;
    {all, 127, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:127>>;
    {all, 128, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:128>>;
    {all, 129, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:129>>;
    {all, 130, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:130>>;
    {all, 131, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:131>>;
    {all, 132, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:132>>;
    {all, 133, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:133>>;
    {all, 134, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:134>>;
    {all, 135, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:135>>;
    {all, 136, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:136>>;
    {all, 137, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:137>>;
    {all, 138, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:138>>;
    {all, 139, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:139>>;
    {all, 140, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:140>>;
    {all, 141, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:141>>;
    {all, 142, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:142>>;
    {all, 143, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:143>>;
    {all, 144, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:144>>;
    {all, 145, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:145>>;
    {all, 146, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:146>>;
    {all, 147, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:147>>;
    {all, 148, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:148>>;
    {all, 149, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:149>>;
    {all, 150, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:150>>;
    {all, 151, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:151>>;
    {all, 152, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:152>>;
    {all, 153, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:153>>;
    {all, 154, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:154>>;
    {all, 155, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:155>>;
    {all, 156, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:156>>;
    {all, 157, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:157>>;
    {all, 158, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:158>>;
    {all, 159, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:159>>;
    {all, 160, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:160>>;
    {all, 161, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:161>>;
    {all, 162, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:162>>;
    {all, 163, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:163>>;
    {all, 164, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:164>>;
    {all, 165, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:165>>;
    {all, 166, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:166>>;
    {all, 167, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:167>>;
    {all, 168, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:168>>;
    {all, 169, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:169>>;
    {all, 170, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:170>>;
    {all, 171, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:171>>;
    {all, 172, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:172>>;
    {all, 173, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:173>>;
    {all, 174, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:174>>;
    {all, 175, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:175>>;
    {all, 176, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:176>>;
    {all, 177, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:177>>;
    {all, 178, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:178>>;
    {all, 179, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:179>>;
    {all, 180, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:180>>;
    {all, 181, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:181>>;
    {all, 182, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:182>>;
    {all, 183, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:183>>;
    {all, 184, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:184>>;
    {all, 185, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:185>>;
    {all, 186, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:186>>;
    {all, 187, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:187>>;
    {all, 188, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:188>>;
    {all, 189, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:189>>;
    {all, 190, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:190>>;
    {all, 191, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:191>>;
    {all, 192, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:192>>;
    {all, 193, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:193>>;
    {all, 194, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:194>>;
    {all, 195, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:195>>;
    {all, 196, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:196>>;
    {all, 197, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:197>>;
    {all, 198, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:198>>;
    {all, 199, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:199>>;
    {all, 200, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:200>>;
    {all, 201, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:201>>;
    {all, 202, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:202>>;
    {all, 203, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:203>>;
    {all, 204, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:204>>;
    {all, 205, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:205>>;
    {all, 206, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:206>>;
    {all, 207, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:207>>;
    {all, 208, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:208>>;
    {all, 209, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:209>>;
    {all, 210, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:210>>;
    {all, 211, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:211>>;
    {all, 212, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:212>>;
    {all, 213, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:213>>;
    {all, 214, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:214>>;
    {all, 215, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:215>>;
    {all, 216, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:216>>;
    {all, 217, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:217>>;
    {all, 218, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:218>>;
    {all, 219, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:219>>;
    {all, 220, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:220>>;
    {all, 221, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:221>>;
    {all, 222, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:222>>;
    {all, 223, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:223>>;
    {all, 224, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:224>>;
    {all, 225, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:225>>;
    {all, 226, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:226>>;
    {all, 227, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:227>>;
    {all, 228, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:228>>;
    {all, 229, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:229>>;
    {all, 230, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:230>>;
    {all, 231, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:231>>;
    {all, 232, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:232>>;
    {all, 233, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:233>>;
    {all, 234, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:234>>;
    {all, 235, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:235>>;
    {all, 236, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:236>>;
    {all, 237, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:237>>;
    {all, 238, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:238>>;
    {all, 239, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:239>>;
    {all, 240, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:240>>;
    {all, 241, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:241>>;
    {all, 242, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:242>>;
    {all, 243, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:243>>;
    {all, 244, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:244>>;
    {all, 245, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:245>>;
    {all, 246, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:246>>;
    {all, 247, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:247>>;
    {all, 248, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:248>>;
    {all, 249, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:249>>;
    {all, 250, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:250>>;
    {all, 251, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:251>>;
    {all, 252, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:252>>;
    {all, 253, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:253>>;
    {all, 254, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:254>>;
    {all, 255, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:255>>;
    {all, 256, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:256>>;
    {_, 1, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:1>>;
    {_, 2, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:2>>;
    {_, 3, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:3>>;
    {_, 4, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:4>>;
    {_, 5, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:5>>;
    {_, 6, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:6>>;
    {_, 7, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:7>>;
    {_, 8, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:8>>;
    {_, 9, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:9>>;
    {_, 10, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:10>>;
    {_, 11, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:11>>;
    {_, 12, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:12>>;
    {_, 13, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:13>>;
    {_, 14, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:14>>;
    {_, 15, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:15>>;
    {_, 16, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:16>>;
    {_, 17, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:17>>;
    {_, 18, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:18>>;
    {_, 19, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:19>>;
    {_, 20, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:20>>;
    {_, 21, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:21>>;
    {_, 22, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:22>>;
    {_, 23, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:23>>;
    {_, 24, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:24>>;
    {_, 25, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:25>>;
    {_, 26, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:26>>;
    {_, 27, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:27>>;
    {_, 28, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:28>>;
    {_, 29, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:29>>;
    {_, 30, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:30>>;
    {_, 31, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:31>>;
    {_, 32, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:32>>;
    {_, 33, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:33>>;
    {_, 34, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:34>>;
    {_, 35, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:35>>;
    {_, 36, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:36>>;
    {_, 37, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:37>>;
    {_, 38, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:38>>;
    {_, 39, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:39>>;
    {_, 40, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:40>>;
    {_, 41, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:41>>;
    {_, 42, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:42>>;
    {_, 43, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:43>>;
    {_, 44, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:44>>;
    {_, 45, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:45>>;
    {_, 46, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:46>>;
    {_, 47, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:47>>;
    {_, 48, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:48>>;
    {_, 49, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:49>>;
    {_, 50, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:50>>;
    {_, 51, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:51>>;
    {_, 52, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:52>>;
    {_, 53, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:53>>;
    {_, 54, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:54>>;
    {_, 55, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:55>>;
    {_, 56, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:56>>;
    {_, 57, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:57>>;
    {_, 58, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:58>>;
    {_, 59, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:59>>;
    {_, 60, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:60>>;
    {_, 61, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:61>>;
    {_, 62, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:62>>;
    {_, 63, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:63>>;
    {_, 64, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:64>>;
    {_, 65, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:65>>;
    {_, 66, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:66>>;
    {_, 67, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:67>>;
    {_, 68, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:68>>;
    {_, 69, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:69>>;
    {_, 70, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:70>>;
    {_, 71, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:71>>;
    {_, 72, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:72>>;
    {_, 73, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:73>>;
    {_, 74, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:74>>;
    {_, 75, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:75>>;
    {_, 76, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:76>>;
    {_, 77, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:77>>;
    {_, 78, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:78>>;
    {_, 79, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:79>>;
    {_, 80, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:80>>;
    {_, 81, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:81>>;
    {_, 82, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:82>>;
    {_, 83, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:83>>;
    {_, 84, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:84>>;
    {_, 85, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:85>>;
    {_, 86, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:86>>;
    {_, 87, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:87>>;
    {_, 88, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:88>>;
    {_, 89, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:89>>;
    {_, 90, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:90>>;
    {_, 91, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:91>>;
    {_, 92, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:92>>;
    {_, 93, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:93>>;
    {_, 94, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:94>>;
    {_, 95, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:95>>;
    {_, 96, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:96>>;
    {_, 97, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:97>>;
    {_, 98, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:98>>;
    {_, 99, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:99>>;
    {_, 100, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:100>>;
    {_, 101, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:101>>;
    {_, 102, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:102>>;
    {_, 103, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:103>>;
    {_, 104, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:104>>;
    {_, 105, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:105>>;
    {_, 106, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:106>>;
    {_, 107, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:107>>;
    {_, 108, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:108>>;
    {_, 109, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:109>>;
    {_, 110, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:110>>;
    {_, 111, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:111>>;
    {_, 112, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:112>>;
    {_, 113, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:113>>;
    {_, 114, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:114>>;
    {_, 115, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:115>>;
    {_, 116, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:116>>;
    {_, 117, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:117>>;
    {_, 118, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:118>>;
    {_, 119, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:119>>;
    {_, 120, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:120>>;
    {_, 121, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:121>>;
    {_, 122, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:122>>;
    {_, 123, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:123>>;
    {_, 124, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:124>>;
    {_, 125, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:125>>;
    {_, 126, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:126>>;
    {_, 127, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:127>>;
    {_, 128, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:128>>;
    {_, 129, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:129>>;
    {_, 130, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:130>>;
    {_, 131, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:131>>;
    {_, 132, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:132>>;
    {_, 133, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:133>>;
    {_, 134, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:134>>;
    {_, 135, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:135>>;
    {_, 136, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:136>>;
    {_, 137, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:137>>;
    {_, 138, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:138>>;
    {_, 139, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:139>>;
    {_, 140, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:140>>;
    {_, 141, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:141>>;
    {_, 142, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:142>>;
    {_, 143, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:143>>;
    {_, 144, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:144>>;
    {_, 145, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:145>>;
    {_, 146, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:146>>;
    {_, 147, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:147>>;
    {_, 148, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:148>>;
    {_, 149, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:149>>;
    {_, 150, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:150>>;
    {_, 151, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:151>>;
    {_, 152, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:152>>;
    {_, 153, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:153>>;
    {_, 154, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:154>>;
    {_, 155, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:155>>;
    {_, 156, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:156>>;
    {_, 157, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:157>>;
    {_, 158, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:158>>;
    {_, 159, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:159>>;
    {_, 160, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:160>>;
    {_, 161, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:161>>;
    {_, 162, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:162>>;
    {_, 163, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:163>>;
    {_, 164, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:164>>;
    {_, 165, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:165>>;
    {_, 166, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:166>>;
    {_, 167, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:167>>;
    {_, 168, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:168>>;
    {_, 169, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:169>>;
    {_, 170, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:170>>;
    {_, 171, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:171>>;
    {_, 172, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:172>>;
    {_, 173, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:173>>;
    {_, 174, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:174>>;
    {_, 175, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:175>>;
    {_, 176, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:176>>;
    {_, 177, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:177>>;
    {_, 178, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:178>>;
    {_, 179, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:179>>;
    {_, 180, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:180>>;
    {_, 181, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:181>>;
    {_, 182, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:182>>;
    {_, 183, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:183>>;
    {_, 184, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:184>>;
    {_, 185, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:185>>;
    {_, 186, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:186>>;
    {_, 187, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:187>>;
    {_, 188, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:188>>;
    {_, 189, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:189>>;
    {_, 190, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:190>>;
    {_, 191, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:191>>;
    {_, 192, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:192>>;
    {_, 193, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:193>>;
    {_, 194, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:194>>;
    {_, 195, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:195>>;
    {_, 196, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:196>>;
    {_, 197, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:197>>;
    {_, 198, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:198>>;
    {_, 199, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:199>>;
    {_, 200, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:200>>;
    {_, 201, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:201>>;
    {_, 202, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:202>>;
    {_, 203, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:203>>;
    {_, 204, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:204>>;
    {_, 205, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:205>>;
    {_, 206, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:206>>;
    {_, 207, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:207>>;
    {_, 208, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:208>>;
    {_, 209, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:209>>;
    {_, 210, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:210>>;
    {_, 211, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:211>>;
    {_, 212, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:212>>;
    {_, 213, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:213>>;
    {_, 214, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:214>>;
    {_, 215, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:215>>;
    {_, 216, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:216>>;
    {_, 217, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:217>>;
    {_, 218, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:218>>;
    {_, 219, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:219>>;
    {_, 220, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:220>>;
    {_, 221, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:221>>;
    {_, 222, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:222>>;
    {_, 223, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:223>>;
    {_, 224, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:224>>;
    {_, 225, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:225>>;
    {_, 226, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:226>>;
    {_, 227, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:227>>;
    {_, 228, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:228>>;
    {_, 229, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:229>>;
    {_, 230, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:230>>;
    {_, 231, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:231>>;
    {_, 232, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:232>>;
    {_, 233, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:233>>;
    {_, 234, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:234>>;
    {_, 235, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:235>>;
    {_, 236, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:236>>;
    {_, 237, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:237>>;
    {_, 238, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:238>>;
    {_, 239, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:239>>;
    {_, 240, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:240>>;
    {_, 241, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:241>>;
    {_, 242, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:242>>;
    {_, 243, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:243>>;
    {_, 244, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:244>>;
    {_, 245, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:245>>;
    {_, 246, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:246>>;
    {_, 247, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:247>>;
    {_, 248, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:248>>;
    {_, 249, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:249>>;
    {_, 250, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:250>>;
    {_, 251, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:251>>;
    {_, 252, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:252>>;
    {_, 253, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:253>>;
    {_, 254, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:254>>;
    {_, 255, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:255>>;
    {_, 256, binary, unsigned, little} ->
      <<Val:Size/unsigned-little-binary-unit:256>>;
    {_, 1, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:1>>;
    {_, 2, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:2>>;
    {_, 3, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:3>>;
    {_, 4, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:4>>;
    {_, 5, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:5>>;
    {_, 6, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:6>>;
    {_, 7, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:7>>;
    {_, 8, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:8>>;
    {_, 9, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:9>>;
    {_, 10, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:10>>;
    {_, 11, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:11>>;
    {_, 12, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:12>>;
    {_, 13, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:13>>;
    {_, 14, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:14>>;
    {_, 15, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:15>>;
    {_, 16, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:16>>;
    {_, 17, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:17>>;
    {_, 18, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:18>>;
    {_, 19, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:19>>;
    {_, 20, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:20>>;
    {_, 21, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:21>>;
    {_, 22, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:22>>;
    {_, 23, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:23>>;
    {_, 24, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:24>>;
    {_, 25, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:25>>;
    {_, 26, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:26>>;
    {_, 27, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:27>>;
    {_, 28, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:28>>;
    {_, 29, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:29>>;
    {_, 30, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:30>>;
    {_, 31, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:31>>;
    {_, 32, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:32>>;
    {_, 33, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:33>>;
    {_, 34, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:34>>;
    {_, 35, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:35>>;
    {_, 36, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:36>>;
    {_, 37, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:37>>;
    {_, 38, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:38>>;
    {_, 39, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:39>>;
    {_, 40, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:40>>;
    {_, 41, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:41>>;
    {_, 42, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:42>>;
    {_, 43, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:43>>;
    {_, 44, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:44>>;
    {_, 45, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:45>>;
    {_, 46, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:46>>;
    {_, 47, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:47>>;
    {_, 48, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:48>>;
    {_, 49, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:49>>;
    {_, 50, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:50>>;
    {_, 51, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:51>>;
    {_, 52, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:52>>;
    {_, 53, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:53>>;
    {_, 54, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:54>>;
    {_, 55, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:55>>;
    {_, 56, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:56>>;
    {_, 57, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:57>>;
    {_, 58, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:58>>;
    {_, 59, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:59>>;
    {_, 60, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:60>>;
    {_, 61, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:61>>;
    {_, 62, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:62>>;
    {_, 63, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:63>>;
    {_, 64, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:64>>;
    {_, 65, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:65>>;
    {_, 66, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:66>>;
    {_, 67, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:67>>;
    {_, 68, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:68>>;
    {_, 69, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:69>>;
    {_, 70, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:70>>;
    {_, 71, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:71>>;
    {_, 72, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:72>>;
    {_, 73, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:73>>;
    {_, 74, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:74>>;
    {_, 75, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:75>>;
    {_, 76, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:76>>;
    {_, 77, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:77>>;
    {_, 78, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:78>>;
    {_, 79, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:79>>;
    {_, 80, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:80>>;
    {_, 81, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:81>>;
    {_, 82, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:82>>;
    {_, 83, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:83>>;
    {_, 84, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:84>>;
    {_, 85, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:85>>;
    {_, 86, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:86>>;
    {_, 87, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:87>>;
    {_, 88, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:88>>;
    {_, 89, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:89>>;
    {_, 90, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:90>>;
    {_, 91, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:91>>;
    {_, 92, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:92>>;
    {_, 93, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:93>>;
    {_, 94, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:94>>;
    {_, 95, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:95>>;
    {_, 96, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:96>>;
    {_, 97, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:97>>;
    {_, 98, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:98>>;
    {_, 99, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:99>>;
    {_, 100, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:100>>;
    {_, 101, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:101>>;
    {_, 102, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:102>>;
    {_, 103, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:103>>;
    {_, 104, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:104>>;
    {_, 105, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:105>>;
    {_, 106, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:106>>;
    {_, 107, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:107>>;
    {_, 108, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:108>>;
    {_, 109, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:109>>;
    {_, 110, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:110>>;
    {_, 111, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:111>>;
    {_, 112, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:112>>;
    {_, 113, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:113>>;
    {_, 114, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:114>>;
    {_, 115, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:115>>;
    {_, 116, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:116>>;
    {_, 117, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:117>>;
    {_, 118, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:118>>;
    {_, 119, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:119>>;
    {_, 120, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:120>>;
    {_, 121, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:121>>;
    {_, 122, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:122>>;
    {_, 123, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:123>>;
    {_, 124, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:124>>;
    {_, 125, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:125>>;
    {_, 126, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:126>>;
    {_, 127, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:127>>;
    {_, 128, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:128>>;
    {_, 129, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:129>>;
    {_, 130, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:130>>;
    {_, 131, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:131>>;
    {_, 132, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:132>>;
    {_, 133, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:133>>;
    {_, 134, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:134>>;
    {_, 135, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:135>>;
    {_, 136, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:136>>;
    {_, 137, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:137>>;
    {_, 138, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:138>>;
    {_, 139, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:139>>;
    {_, 140, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:140>>;
    {_, 141, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:141>>;
    {_, 142, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:142>>;
    {_, 143, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:143>>;
    {_, 144, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:144>>;
    {_, 145, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:145>>;
    {_, 146, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:146>>;
    {_, 147, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:147>>;
    {_, 148, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:148>>;
    {_, 149, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:149>>;
    {_, 150, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:150>>;
    {_, 151, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:151>>;
    {_, 152, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:152>>;
    {_, 153, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:153>>;
    {_, 154, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:154>>;
    {_, 155, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:155>>;
    {_, 156, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:156>>;
    {_, 157, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:157>>;
    {_, 158, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:158>>;
    {_, 159, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:159>>;
    {_, 160, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:160>>;
    {_, 161, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:161>>;
    {_, 162, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:162>>;
    {_, 163, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:163>>;
    {_, 164, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:164>>;
    {_, 165, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:165>>;
    {_, 166, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:166>>;
    {_, 167, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:167>>;
    {_, 168, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:168>>;
    {_, 169, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:169>>;
    {_, 170, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:170>>;
    {_, 171, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:171>>;
    {_, 172, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:172>>;
    {_, 173, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:173>>;
    {_, 174, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:174>>;
    {_, 175, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:175>>;
    {_, 176, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:176>>;
    {_, 177, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:177>>;
    {_, 178, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:178>>;
    {_, 179, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:179>>;
    {_, 180, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:180>>;
    {_, 181, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:181>>;
    {_, 182, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:182>>;
    {_, 183, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:183>>;
    {_, 184, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:184>>;
    {_, 185, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:185>>;
    {_, 186, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:186>>;
    {_, 187, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:187>>;
    {_, 188, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:188>>;
    {_, 189, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:189>>;
    {_, 190, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:190>>;
    {_, 191, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:191>>;
    {_, 192, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:192>>;
    {_, 193, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:193>>;
    {_, 194, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:194>>;
    {_, 195, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:195>>;
    {_, 196, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:196>>;
    {_, 197, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:197>>;
    {_, 198, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:198>>;
    {_, 199, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:199>>;
    {_, 200, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:200>>;
    {_, 201, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:201>>;
    {_, 202, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:202>>;
    {_, 203, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:203>>;
    {_, 204, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:204>>;
    {_, 205, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:205>>;
    {_, 206, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:206>>;
    {_, 207, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:207>>;
    {_, 208, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:208>>;
    {_, 209, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:209>>;
    {_, 210, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:210>>;
    {_, 211, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:211>>;
    {_, 212, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:212>>;
    {_, 213, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:213>>;
    {_, 214, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:214>>;
    {_, 215, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:215>>;
    {_, 216, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:216>>;
    {_, 217, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:217>>;
    {_, 218, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:218>>;
    {_, 219, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:219>>;
    {_, 220, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:220>>;
    {_, 221, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:221>>;
    {_, 222, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:222>>;
    {_, 223, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:223>>;
    {_, 224, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:224>>;
    {_, 225, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:225>>;
    {_, 226, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:226>>;
    {_, 227, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:227>>;
    {_, 228, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:228>>;
    {_, 229, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:229>>;
    {_, 230, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:230>>;
    {_, 231, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:231>>;
    {_, 232, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:232>>;
    {_, 233, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:233>>;
    {_, 234, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:234>>;
    {_, 235, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:235>>;
    {_, 236, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:236>>;
    {_, 237, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:237>>;
    {_, 238, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:238>>;
    {_, 239, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:239>>;
    {_, 240, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:240>>;
    {_, 241, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:241>>;
    {_, 242, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:242>>;
    {_, 243, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:243>>;
    {_, 244, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:244>>;
    {_, 245, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:245>>;
    {_, 246, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:246>>;
    {_, 247, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:247>>;
    {_, 248, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:248>>;
    {_, 249, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:249>>;
    {_, 250, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:250>>;
    {_, 251, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:251>>;
    {_, 252, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:252>>;
    {_, 253, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:253>>;
    {_, 254, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:254>>;
    {_, 255, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:255>>;
    {_, 256, integer, signed, native} ->
      <<Val:Size/signed-native-integer-unit:256>>;
    {_, 1, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:1>>;
    {_, 2, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:2>>;
    {_, 3, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:3>>;
    {_, 4, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:4>>;
    {_, 5, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:5>>;
    {_, 6, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:6>>;
    {_, 7, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:7>>;
    {_, 8, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:8>>;
    {_, 9, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:9>>;
    {_, 10, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:10>>;
    {_, 11, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:11>>;
    {_, 12, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:12>>;
    {_, 13, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:13>>;
    {_, 14, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:14>>;
    {_, 15, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:15>>;
    {_, 16, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:16>>;
    {_, 17, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:17>>;
    {_, 18, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:18>>;
    {_, 19, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:19>>;
    {_, 20, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:20>>;
    {_, 21, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:21>>;
    {_, 22, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:22>>;
    {_, 23, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:23>>;
    {_, 24, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:24>>;
    {_, 25, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:25>>;
    {_, 26, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:26>>;
    {_, 27, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:27>>;
    {_, 28, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:28>>;
    {_, 29, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:29>>;
    {_, 30, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:30>>;
    {_, 31, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:31>>;
    {_, 32, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:32>>;
    {_, 33, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:33>>;
    {_, 34, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:34>>;
    {_, 35, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:35>>;
    {_, 36, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:36>>;
    {_, 37, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:37>>;
    {_, 38, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:38>>;
    {_, 39, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:39>>;
    {_, 40, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:40>>;
    {_, 41, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:41>>;
    {_, 42, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:42>>;
    {_, 43, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:43>>;
    {_, 44, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:44>>;
    {_, 45, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:45>>;
    {_, 46, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:46>>;
    {_, 47, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:47>>;
    {_, 48, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:48>>;
    {_, 49, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:49>>;
    {_, 50, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:50>>;
    {_, 51, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:51>>;
    {_, 52, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:52>>;
    {_, 53, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:53>>;
    {_, 54, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:54>>;
    {_, 55, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:55>>;
    {_, 56, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:56>>;
    {_, 57, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:57>>;
    {_, 58, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:58>>;
    {_, 59, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:59>>;
    {_, 60, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:60>>;
    {_, 61, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:61>>;
    {_, 62, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:62>>;
    {_, 63, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:63>>;
    {_, 64, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:64>>;
    {_, 65, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:65>>;
    {_, 66, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:66>>;
    {_, 67, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:67>>;
    {_, 68, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:68>>;
    {_, 69, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:69>>;
    {_, 70, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:70>>;
    {_, 71, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:71>>;
    {_, 72, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:72>>;
    {_, 73, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:73>>;
    {_, 74, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:74>>;
    {_, 75, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:75>>;
    {_, 76, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:76>>;
    {_, 77, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:77>>;
    {_, 78, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:78>>;
    {_, 79, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:79>>;
    {_, 80, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:80>>;
    {_, 81, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:81>>;
    {_, 82, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:82>>;
    {_, 83, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:83>>;
    {_, 84, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:84>>;
    {_, 85, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:85>>;
    {_, 86, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:86>>;
    {_, 87, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:87>>;
    {_, 88, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:88>>;
    {_, 89, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:89>>;
    {_, 90, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:90>>;
    {_, 91, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:91>>;
    {_, 92, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:92>>;
    {_, 93, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:93>>;
    {_, 94, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:94>>;
    {_, 95, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:95>>;
    {_, 96, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:96>>;
    {_, 97, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:97>>;
    {_, 98, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:98>>;
    {_, 99, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:99>>;
    {_, 100, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:100>>;
    {_, 101, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:101>>;
    {_, 102, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:102>>;
    {_, 103, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:103>>;
    {_, 104, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:104>>;
    {_, 105, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:105>>;
    {_, 106, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:106>>;
    {_, 107, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:107>>;
    {_, 108, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:108>>;
    {_, 109, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:109>>;
    {_, 110, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:110>>;
    {_, 111, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:111>>;
    {_, 112, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:112>>;
    {_, 113, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:113>>;
    {_, 114, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:114>>;
    {_, 115, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:115>>;
    {_, 116, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:116>>;
    {_, 117, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:117>>;
    {_, 118, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:118>>;
    {_, 119, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:119>>;
    {_, 120, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:120>>;
    {_, 121, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:121>>;
    {_, 122, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:122>>;
    {_, 123, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:123>>;
    {_, 124, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:124>>;
    {_, 125, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:125>>;
    {_, 126, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:126>>;
    {_, 127, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:127>>;
    {_, 128, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:128>>;
    {_, 129, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:129>>;
    {_, 130, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:130>>;
    {_, 131, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:131>>;
    {_, 132, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:132>>;
    {_, 133, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:133>>;
    {_, 134, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:134>>;
    {_, 135, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:135>>;
    {_, 136, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:136>>;
    {_, 137, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:137>>;
    {_, 138, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:138>>;
    {_, 139, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:139>>;
    {_, 140, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:140>>;
    {_, 141, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:141>>;
    {_, 142, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:142>>;
    {_, 143, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:143>>;
    {_, 144, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:144>>;
    {_, 145, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:145>>;
    {_, 146, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:146>>;
    {_, 147, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:147>>;
    {_, 148, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:148>>;
    {_, 149, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:149>>;
    {_, 150, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:150>>;
    {_, 151, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:151>>;
    {_, 152, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:152>>;
    {_, 153, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:153>>;
    {_, 154, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:154>>;
    {_, 155, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:155>>;
    {_, 156, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:156>>;
    {_, 157, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:157>>;
    {_, 158, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:158>>;
    {_, 159, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:159>>;
    {_, 160, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:160>>;
    {_, 161, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:161>>;
    {_, 162, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:162>>;
    {_, 163, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:163>>;
    {_, 164, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:164>>;
    {_, 165, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:165>>;
    {_, 166, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:166>>;
    {_, 167, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:167>>;
    {_, 168, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:168>>;
    {_, 169, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:169>>;
    {_, 170, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:170>>;
    {_, 171, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:171>>;
    {_, 172, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:172>>;
    {_, 173, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:173>>;
    {_, 174, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:174>>;
    {_, 175, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:175>>;
    {_, 176, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:176>>;
    {_, 177, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:177>>;
    {_, 178, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:178>>;
    {_, 179, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:179>>;
    {_, 180, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:180>>;
    {_, 181, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:181>>;
    {_, 182, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:182>>;
    {_, 183, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:183>>;
    {_, 184, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:184>>;
    {_, 185, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:185>>;
    {_, 186, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:186>>;
    {_, 187, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:187>>;
    {_, 188, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:188>>;
    {_, 189, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:189>>;
    {_, 190, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:190>>;
    {_, 191, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:191>>;
    {_, 192, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:192>>;
    {_, 193, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:193>>;
    {_, 194, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:194>>;
    {_, 195, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:195>>;
    {_, 196, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:196>>;
    {_, 197, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:197>>;
    {_, 198, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:198>>;
    {_, 199, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:199>>;
    {_, 200, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:200>>;
    {_, 201, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:201>>;
    {_, 202, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:202>>;
    {_, 203, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:203>>;
    {_, 204, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:204>>;
    {_, 205, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:205>>;
    {_, 206, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:206>>;
    {_, 207, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:207>>;
    {_, 208, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:208>>;
    {_, 209, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:209>>;
    {_, 210, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:210>>;
    {_, 211, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:211>>;
    {_, 212, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:212>>;
    {_, 213, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:213>>;
    {_, 214, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:214>>;
    {_, 215, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:215>>;
    {_, 216, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:216>>;
    {_, 217, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:217>>;
    {_, 218, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:218>>;
    {_, 219, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:219>>;
    {_, 220, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:220>>;
    {_, 221, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:221>>;
    {_, 222, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:222>>;
    {_, 223, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:223>>;
    {_, 224, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:224>>;
    {_, 225, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:225>>;
    {_, 226, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:226>>;
    {_, 227, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:227>>;
    {_, 228, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:228>>;
    {_, 229, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:229>>;
    {_, 230, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:230>>;
    {_, 231, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:231>>;
    {_, 232, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:232>>;
    {_, 233, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:233>>;
    {_, 234, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:234>>;
    {_, 235, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:235>>;
    {_, 236, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:236>>;
    {_, 237, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:237>>;
    {_, 238, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:238>>;
    {_, 239, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:239>>;
    {_, 240, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:240>>;
    {_, 241, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:241>>;
    {_, 242, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:242>>;
    {_, 243, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:243>>;
    {_, 244, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:244>>;
    {_, 245, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:245>>;
    {_, 246, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:246>>;
    {_, 247, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:247>>;
    {_, 248, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:248>>;
    {_, 249, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:249>>;
    {_, 250, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:250>>;
    {_, 251, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:251>>;
    {_, 252, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:252>>;
    {_, 253, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:253>>;
    {_, 254, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:254>>;
    {_, 255, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:255>>;
    {_, 256, float, signed, native} ->
      <<Val:Size/signed-native-float-unit:256>>;
    {all, 1, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:1>>;
    {all, 2, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:2>>;
    {all, 3, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:3>>;
    {all, 4, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:4>>;
    {all, 5, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:5>>;
    {all, 6, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:6>>;
    {all, 7, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:7>>;
    {all, 8, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:8>>;
    {all, 9, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:9>>;
    {all, 10, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:10>>;
    {all, 11, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:11>>;
    {all, 12, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:12>>;
    {all, 13, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:13>>;
    {all, 14, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:14>>;
    {all, 15, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:15>>;
    {all, 16, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:16>>;
    {all, 17, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:17>>;
    {all, 18, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:18>>;
    {all, 19, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:19>>;
    {all, 20, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:20>>;
    {all, 21, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:21>>;
    {all, 22, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:22>>;
    {all, 23, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:23>>;
    {all, 24, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:24>>;
    {all, 25, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:25>>;
    {all, 26, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:26>>;
    {all, 27, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:27>>;
    {all, 28, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:28>>;
    {all, 29, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:29>>;
    {all, 30, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:30>>;
    {all, 31, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:31>>;
    {all, 32, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:32>>;
    {all, 33, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:33>>;
    {all, 34, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:34>>;
    {all, 35, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:35>>;
    {all, 36, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:36>>;
    {all, 37, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:37>>;
    {all, 38, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:38>>;
    {all, 39, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:39>>;
    {all, 40, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:40>>;
    {all, 41, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:41>>;
    {all, 42, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:42>>;
    {all, 43, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:43>>;
    {all, 44, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:44>>;
    {all, 45, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:45>>;
    {all, 46, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:46>>;
    {all, 47, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:47>>;
    {all, 48, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:48>>;
    {all, 49, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:49>>;
    {all, 50, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:50>>;
    {all, 51, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:51>>;
    {all, 52, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:52>>;
    {all, 53, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:53>>;
    {all, 54, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:54>>;
    {all, 55, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:55>>;
    {all, 56, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:56>>;
    {all, 57, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:57>>;
    {all, 58, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:58>>;
    {all, 59, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:59>>;
    {all, 60, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:60>>;
    {all, 61, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:61>>;
    {all, 62, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:62>>;
    {all, 63, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:63>>;
    {all, 64, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:64>>;
    {all, 65, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:65>>;
    {all, 66, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:66>>;
    {all, 67, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:67>>;
    {all, 68, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:68>>;
    {all, 69, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:69>>;
    {all, 70, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:70>>;
    {all, 71, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:71>>;
    {all, 72, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:72>>;
    {all, 73, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:73>>;
    {all, 74, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:74>>;
    {all, 75, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:75>>;
    {all, 76, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:76>>;
    {all, 77, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:77>>;
    {all, 78, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:78>>;
    {all, 79, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:79>>;
    {all, 80, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:80>>;
    {all, 81, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:81>>;
    {all, 82, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:82>>;
    {all, 83, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:83>>;
    {all, 84, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:84>>;
    {all, 85, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:85>>;
    {all, 86, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:86>>;
    {all, 87, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:87>>;
    {all, 88, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:88>>;
    {all, 89, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:89>>;
    {all, 90, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:90>>;
    {all, 91, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:91>>;
    {all, 92, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:92>>;
    {all, 93, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:93>>;
    {all, 94, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:94>>;
    {all, 95, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:95>>;
    {all, 96, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:96>>;
    {all, 97, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:97>>;
    {all, 98, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:98>>;
    {all, 99, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:99>>;
    {all, 100, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:100>>;
    {all, 101, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:101>>;
    {all, 102, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:102>>;
    {all, 103, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:103>>;
    {all, 104, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:104>>;
    {all, 105, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:105>>;
    {all, 106, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:106>>;
    {all, 107, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:107>>;
    {all, 108, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:108>>;
    {all, 109, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:109>>;
    {all, 110, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:110>>;
    {all, 111, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:111>>;
    {all, 112, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:112>>;
    {all, 113, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:113>>;
    {all, 114, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:114>>;
    {all, 115, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:115>>;
    {all, 116, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:116>>;
    {all, 117, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:117>>;
    {all, 118, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:118>>;
    {all, 119, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:119>>;
    {all, 120, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:120>>;
    {all, 121, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:121>>;
    {all, 122, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:122>>;
    {all, 123, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:123>>;
    {all, 124, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:124>>;
    {all, 125, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:125>>;
    {all, 126, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:126>>;
    {all, 127, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:127>>;
    {all, 128, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:128>>;
    {all, 129, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:129>>;
    {all, 130, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:130>>;
    {all, 131, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:131>>;
    {all, 132, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:132>>;
    {all, 133, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:133>>;
    {all, 134, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:134>>;
    {all, 135, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:135>>;
    {all, 136, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:136>>;
    {all, 137, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:137>>;
    {all, 138, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:138>>;
    {all, 139, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:139>>;
    {all, 140, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:140>>;
    {all, 141, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:141>>;
    {all, 142, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:142>>;
    {all, 143, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:143>>;
    {all, 144, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:144>>;
    {all, 145, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:145>>;
    {all, 146, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:146>>;
    {all, 147, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:147>>;
    {all, 148, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:148>>;
    {all, 149, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:149>>;
    {all, 150, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:150>>;
    {all, 151, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:151>>;
    {all, 152, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:152>>;
    {all, 153, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:153>>;
    {all, 154, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:154>>;
    {all, 155, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:155>>;
    {all, 156, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:156>>;
    {all, 157, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:157>>;
    {all, 158, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:158>>;
    {all, 159, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:159>>;
    {all, 160, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:160>>;
    {all, 161, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:161>>;
    {all, 162, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:162>>;
    {all, 163, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:163>>;
    {all, 164, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:164>>;
    {all, 165, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:165>>;
    {all, 166, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:166>>;
    {all, 167, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:167>>;
    {all, 168, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:168>>;
    {all, 169, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:169>>;
    {all, 170, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:170>>;
    {all, 171, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:171>>;
    {all, 172, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:172>>;
    {all, 173, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:173>>;
    {all, 174, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:174>>;
    {all, 175, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:175>>;
    {all, 176, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:176>>;
    {all, 177, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:177>>;
    {all, 178, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:178>>;
    {all, 179, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:179>>;
    {all, 180, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:180>>;
    {all, 181, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:181>>;
    {all, 182, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:182>>;
    {all, 183, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:183>>;
    {all, 184, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:184>>;
    {all, 185, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:185>>;
    {all, 186, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:186>>;
    {all, 187, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:187>>;
    {all, 188, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:188>>;
    {all, 189, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:189>>;
    {all, 190, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:190>>;
    {all, 191, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:191>>;
    {all, 192, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:192>>;
    {all, 193, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:193>>;
    {all, 194, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:194>>;
    {all, 195, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:195>>;
    {all, 196, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:196>>;
    {all, 197, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:197>>;
    {all, 198, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:198>>;
    {all, 199, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:199>>;
    {all, 200, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:200>>;
    {all, 201, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:201>>;
    {all, 202, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:202>>;
    {all, 203, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:203>>;
    {all, 204, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:204>>;
    {all, 205, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:205>>;
    {all, 206, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:206>>;
    {all, 207, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:207>>;
    {all, 208, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:208>>;
    {all, 209, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:209>>;
    {all, 210, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:210>>;
    {all, 211, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:211>>;
    {all, 212, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:212>>;
    {all, 213, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:213>>;
    {all, 214, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:214>>;
    {all, 215, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:215>>;
    {all, 216, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:216>>;
    {all, 217, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:217>>;
    {all, 218, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:218>>;
    {all, 219, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:219>>;
    {all, 220, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:220>>;
    {all, 221, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:221>>;
    {all, 222, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:222>>;
    {all, 223, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:223>>;
    {all, 224, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:224>>;
    {all, 225, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:225>>;
    {all, 226, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:226>>;
    {all, 227, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:227>>;
    {all, 228, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:228>>;
    {all, 229, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:229>>;
    {all, 230, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:230>>;
    {all, 231, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:231>>;
    {all, 232, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:232>>;
    {all, 233, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:233>>;
    {all, 234, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:234>>;
    {all, 235, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:235>>;
    {all, 236, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:236>>;
    {all, 237, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:237>>;
    {all, 238, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:238>>;
    {all, 239, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:239>>;
    {all, 240, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:240>>;
    {all, 241, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:241>>;
    {all, 242, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:242>>;
    {all, 243, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:243>>;
    {all, 244, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:244>>;
    {all, 245, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:245>>;
    {all, 246, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:246>>;
    {all, 247, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:247>>;
    {all, 248, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:248>>;
    {all, 249, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:249>>;
    {all, 250, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:250>>;
    {all, 251, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:251>>;
    {all, 252, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:252>>;
    {all, 253, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:253>>;
    {all, 254, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:254>>;
    {all, 255, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:255>>;
    {all, 256, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:256>>;
    {_, 1, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:1>>;
    {_, 2, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:2>>;
    {_, 3, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:3>>;
    {_, 4, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:4>>;
    {_, 5, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:5>>;
    {_, 6, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:6>>;
    {_, 7, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:7>>;
    {_, 8, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:8>>;
    {_, 9, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:9>>;
    {_, 10, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:10>>;
    {_, 11, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:11>>;
    {_, 12, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:12>>;
    {_, 13, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:13>>;
    {_, 14, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:14>>;
    {_, 15, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:15>>;
    {_, 16, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:16>>;
    {_, 17, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:17>>;
    {_, 18, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:18>>;
    {_, 19, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:19>>;
    {_, 20, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:20>>;
    {_, 21, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:21>>;
    {_, 22, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:22>>;
    {_, 23, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:23>>;
    {_, 24, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:24>>;
    {_, 25, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:25>>;
    {_, 26, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:26>>;
    {_, 27, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:27>>;
    {_, 28, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:28>>;
    {_, 29, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:29>>;
    {_, 30, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:30>>;
    {_, 31, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:31>>;
    {_, 32, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:32>>;
    {_, 33, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:33>>;
    {_, 34, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:34>>;
    {_, 35, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:35>>;
    {_, 36, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:36>>;
    {_, 37, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:37>>;
    {_, 38, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:38>>;
    {_, 39, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:39>>;
    {_, 40, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:40>>;
    {_, 41, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:41>>;
    {_, 42, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:42>>;
    {_, 43, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:43>>;
    {_, 44, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:44>>;
    {_, 45, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:45>>;
    {_, 46, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:46>>;
    {_, 47, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:47>>;
    {_, 48, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:48>>;
    {_, 49, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:49>>;
    {_, 50, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:50>>;
    {_, 51, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:51>>;
    {_, 52, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:52>>;
    {_, 53, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:53>>;
    {_, 54, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:54>>;
    {_, 55, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:55>>;
    {_, 56, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:56>>;
    {_, 57, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:57>>;
    {_, 58, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:58>>;
    {_, 59, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:59>>;
    {_, 60, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:60>>;
    {_, 61, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:61>>;
    {_, 62, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:62>>;
    {_, 63, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:63>>;
    {_, 64, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:64>>;
    {_, 65, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:65>>;
    {_, 66, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:66>>;
    {_, 67, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:67>>;
    {_, 68, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:68>>;
    {_, 69, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:69>>;
    {_, 70, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:70>>;
    {_, 71, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:71>>;
    {_, 72, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:72>>;
    {_, 73, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:73>>;
    {_, 74, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:74>>;
    {_, 75, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:75>>;
    {_, 76, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:76>>;
    {_, 77, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:77>>;
    {_, 78, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:78>>;
    {_, 79, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:79>>;
    {_, 80, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:80>>;
    {_, 81, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:81>>;
    {_, 82, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:82>>;
    {_, 83, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:83>>;
    {_, 84, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:84>>;
    {_, 85, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:85>>;
    {_, 86, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:86>>;
    {_, 87, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:87>>;
    {_, 88, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:88>>;
    {_, 89, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:89>>;
    {_, 90, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:90>>;
    {_, 91, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:91>>;
    {_, 92, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:92>>;
    {_, 93, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:93>>;
    {_, 94, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:94>>;
    {_, 95, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:95>>;
    {_, 96, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:96>>;
    {_, 97, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:97>>;
    {_, 98, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:98>>;
    {_, 99, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:99>>;
    {_, 100, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:100>>;
    {_, 101, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:101>>;
    {_, 102, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:102>>;
    {_, 103, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:103>>;
    {_, 104, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:104>>;
    {_, 105, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:105>>;
    {_, 106, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:106>>;
    {_, 107, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:107>>;
    {_, 108, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:108>>;
    {_, 109, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:109>>;
    {_, 110, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:110>>;
    {_, 111, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:111>>;
    {_, 112, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:112>>;
    {_, 113, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:113>>;
    {_, 114, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:114>>;
    {_, 115, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:115>>;
    {_, 116, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:116>>;
    {_, 117, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:117>>;
    {_, 118, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:118>>;
    {_, 119, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:119>>;
    {_, 120, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:120>>;
    {_, 121, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:121>>;
    {_, 122, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:122>>;
    {_, 123, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:123>>;
    {_, 124, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:124>>;
    {_, 125, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:125>>;
    {_, 126, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:126>>;
    {_, 127, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:127>>;
    {_, 128, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:128>>;
    {_, 129, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:129>>;
    {_, 130, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:130>>;
    {_, 131, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:131>>;
    {_, 132, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:132>>;
    {_, 133, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:133>>;
    {_, 134, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:134>>;
    {_, 135, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:135>>;
    {_, 136, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:136>>;
    {_, 137, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:137>>;
    {_, 138, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:138>>;
    {_, 139, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:139>>;
    {_, 140, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:140>>;
    {_, 141, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:141>>;
    {_, 142, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:142>>;
    {_, 143, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:143>>;
    {_, 144, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:144>>;
    {_, 145, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:145>>;
    {_, 146, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:146>>;
    {_, 147, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:147>>;
    {_, 148, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:148>>;
    {_, 149, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:149>>;
    {_, 150, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:150>>;
    {_, 151, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:151>>;
    {_, 152, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:152>>;
    {_, 153, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:153>>;
    {_, 154, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:154>>;
    {_, 155, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:155>>;
    {_, 156, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:156>>;
    {_, 157, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:157>>;
    {_, 158, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:158>>;
    {_, 159, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:159>>;
    {_, 160, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:160>>;
    {_, 161, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:161>>;
    {_, 162, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:162>>;
    {_, 163, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:163>>;
    {_, 164, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:164>>;
    {_, 165, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:165>>;
    {_, 166, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:166>>;
    {_, 167, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:167>>;
    {_, 168, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:168>>;
    {_, 169, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:169>>;
    {_, 170, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:170>>;
    {_, 171, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:171>>;
    {_, 172, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:172>>;
    {_, 173, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:173>>;
    {_, 174, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:174>>;
    {_, 175, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:175>>;
    {_, 176, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:176>>;
    {_, 177, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:177>>;
    {_, 178, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:178>>;
    {_, 179, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:179>>;
    {_, 180, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:180>>;
    {_, 181, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:181>>;
    {_, 182, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:182>>;
    {_, 183, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:183>>;
    {_, 184, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:184>>;
    {_, 185, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:185>>;
    {_, 186, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:186>>;
    {_, 187, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:187>>;
    {_, 188, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:188>>;
    {_, 189, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:189>>;
    {_, 190, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:190>>;
    {_, 191, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:191>>;
    {_, 192, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:192>>;
    {_, 193, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:193>>;
    {_, 194, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:194>>;
    {_, 195, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:195>>;
    {_, 196, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:196>>;
    {_, 197, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:197>>;
    {_, 198, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:198>>;
    {_, 199, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:199>>;
    {_, 200, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:200>>;
    {_, 201, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:201>>;
    {_, 202, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:202>>;
    {_, 203, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:203>>;
    {_, 204, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:204>>;
    {_, 205, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:205>>;
    {_, 206, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:206>>;
    {_, 207, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:207>>;
    {_, 208, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:208>>;
    {_, 209, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:209>>;
    {_, 210, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:210>>;
    {_, 211, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:211>>;
    {_, 212, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:212>>;
    {_, 213, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:213>>;
    {_, 214, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:214>>;
    {_, 215, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:215>>;
    {_, 216, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:216>>;
    {_, 217, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:217>>;
    {_, 218, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:218>>;
    {_, 219, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:219>>;
    {_, 220, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:220>>;
    {_, 221, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:221>>;
    {_, 222, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:222>>;
    {_, 223, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:223>>;
    {_, 224, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:224>>;
    {_, 225, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:225>>;
    {_, 226, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:226>>;
    {_, 227, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:227>>;
    {_, 228, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:228>>;
    {_, 229, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:229>>;
    {_, 230, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:230>>;
    {_, 231, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:231>>;
    {_, 232, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:232>>;
    {_, 233, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:233>>;
    {_, 234, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:234>>;
    {_, 235, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:235>>;
    {_, 236, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:236>>;
    {_, 237, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:237>>;
    {_, 238, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:238>>;
    {_, 239, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:239>>;
    {_, 240, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:240>>;
    {_, 241, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:241>>;
    {_, 242, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:242>>;
    {_, 243, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:243>>;
    {_, 244, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:244>>;
    {_, 245, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:245>>;
    {_, 246, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:246>>;
    {_, 247, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:247>>;
    {_, 248, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:248>>;
    {_, 249, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:249>>;
    {_, 250, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:250>>;
    {_, 251, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:251>>;
    {_, 252, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:252>>;
    {_, 253, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:253>>;
    {_, 254, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:254>>;
    {_, 255, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:255>>;
    {_, 256, binary, signed, native} ->
      <<Val:Size/signed-native-binary-unit:256>>;
    {_, 1, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:1>>;
    {_, 2, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:2>>;
    {_, 3, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:3>>;
    {_, 4, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:4>>;
    {_, 5, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:5>>;
    {_, 6, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:6>>;
    {_, 7, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:7>>;
    {_, 8, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:8>>;
    {_, 9, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:9>>;
    {_, 10, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:10>>;
    {_, 11, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:11>>;
    {_, 12, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:12>>;
    {_, 13, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:13>>;
    {_, 14, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:14>>;
    {_, 15, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:15>>;
    {_, 16, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:16>>;
    {_, 17, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:17>>;
    {_, 18, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:18>>;
    {_, 19, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:19>>;
    {_, 20, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:20>>;
    {_, 21, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:21>>;
    {_, 22, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:22>>;
    {_, 23, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:23>>;
    {_, 24, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:24>>;
    {_, 25, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:25>>;
    {_, 26, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:26>>;
    {_, 27, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:27>>;
    {_, 28, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:28>>;
    {_, 29, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:29>>;
    {_, 30, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:30>>;
    {_, 31, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:31>>;
    {_, 32, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:32>>;
    {_, 33, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:33>>;
    {_, 34, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:34>>;
    {_, 35, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:35>>;
    {_, 36, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:36>>;
    {_, 37, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:37>>;
    {_, 38, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:38>>;
    {_, 39, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:39>>;
    {_, 40, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:40>>;
    {_, 41, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:41>>;
    {_, 42, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:42>>;
    {_, 43, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:43>>;
    {_, 44, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:44>>;
    {_, 45, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:45>>;
    {_, 46, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:46>>;
    {_, 47, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:47>>;
    {_, 48, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:48>>;
    {_, 49, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:49>>;
    {_, 50, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:50>>;
    {_, 51, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:51>>;
    {_, 52, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:52>>;
    {_, 53, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:53>>;
    {_, 54, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:54>>;
    {_, 55, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:55>>;
    {_, 56, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:56>>;
    {_, 57, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:57>>;
    {_, 58, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:58>>;
    {_, 59, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:59>>;
    {_, 60, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:60>>;
    {_, 61, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:61>>;
    {_, 62, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:62>>;
    {_, 63, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:63>>;
    {_, 64, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:64>>;
    {_, 65, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:65>>;
    {_, 66, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:66>>;
    {_, 67, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:67>>;
    {_, 68, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:68>>;
    {_, 69, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:69>>;
    {_, 70, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:70>>;
    {_, 71, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:71>>;
    {_, 72, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:72>>;
    {_, 73, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:73>>;
    {_, 74, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:74>>;
    {_, 75, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:75>>;
    {_, 76, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:76>>;
    {_, 77, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:77>>;
    {_, 78, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:78>>;
    {_, 79, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:79>>;
    {_, 80, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:80>>;
    {_, 81, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:81>>;
    {_, 82, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:82>>;
    {_, 83, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:83>>;
    {_, 84, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:84>>;
    {_, 85, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:85>>;
    {_, 86, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:86>>;
    {_, 87, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:87>>;
    {_, 88, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:88>>;
    {_, 89, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:89>>;
    {_, 90, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:90>>;
    {_, 91, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:91>>;
    {_, 92, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:92>>;
    {_, 93, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:93>>;
    {_, 94, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:94>>;
    {_, 95, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:95>>;
    {_, 96, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:96>>;
    {_, 97, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:97>>;
    {_, 98, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:98>>;
    {_, 99, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:99>>;
    {_, 100, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:100>>;
    {_, 101, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:101>>;
    {_, 102, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:102>>;
    {_, 103, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:103>>;
    {_, 104, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:104>>;
    {_, 105, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:105>>;
    {_, 106, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:106>>;
    {_, 107, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:107>>;
    {_, 108, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:108>>;
    {_, 109, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:109>>;
    {_, 110, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:110>>;
    {_, 111, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:111>>;
    {_, 112, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:112>>;
    {_, 113, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:113>>;
    {_, 114, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:114>>;
    {_, 115, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:115>>;
    {_, 116, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:116>>;
    {_, 117, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:117>>;
    {_, 118, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:118>>;
    {_, 119, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:119>>;
    {_, 120, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:120>>;
    {_, 121, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:121>>;
    {_, 122, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:122>>;
    {_, 123, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:123>>;
    {_, 124, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:124>>;
    {_, 125, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:125>>;
    {_, 126, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:126>>;
    {_, 127, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:127>>;
    {_, 128, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:128>>;
    {_, 129, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:129>>;
    {_, 130, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:130>>;
    {_, 131, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:131>>;
    {_, 132, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:132>>;
    {_, 133, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:133>>;
    {_, 134, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:134>>;
    {_, 135, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:135>>;
    {_, 136, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:136>>;
    {_, 137, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:137>>;
    {_, 138, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:138>>;
    {_, 139, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:139>>;
    {_, 140, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:140>>;
    {_, 141, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:141>>;
    {_, 142, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:142>>;
    {_, 143, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:143>>;
    {_, 144, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:144>>;
    {_, 145, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:145>>;
    {_, 146, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:146>>;
    {_, 147, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:147>>;
    {_, 148, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:148>>;
    {_, 149, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:149>>;
    {_, 150, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:150>>;
    {_, 151, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:151>>;
    {_, 152, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:152>>;
    {_, 153, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:153>>;
    {_, 154, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:154>>;
    {_, 155, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:155>>;
    {_, 156, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:156>>;
    {_, 157, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:157>>;
    {_, 158, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:158>>;
    {_, 159, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:159>>;
    {_, 160, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:160>>;
    {_, 161, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:161>>;
    {_, 162, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:162>>;
    {_, 163, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:163>>;
    {_, 164, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:164>>;
    {_, 165, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:165>>;
    {_, 166, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:166>>;
    {_, 167, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:167>>;
    {_, 168, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:168>>;
    {_, 169, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:169>>;
    {_, 170, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:170>>;
    {_, 171, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:171>>;
    {_, 172, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:172>>;
    {_, 173, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:173>>;
    {_, 174, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:174>>;
    {_, 175, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:175>>;
    {_, 176, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:176>>;
    {_, 177, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:177>>;
    {_, 178, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:178>>;
    {_, 179, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:179>>;
    {_, 180, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:180>>;
    {_, 181, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:181>>;
    {_, 182, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:182>>;
    {_, 183, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:183>>;
    {_, 184, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:184>>;
    {_, 185, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:185>>;
    {_, 186, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:186>>;
    {_, 187, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:187>>;
    {_, 188, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:188>>;
    {_, 189, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:189>>;
    {_, 190, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:190>>;
    {_, 191, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:191>>;
    {_, 192, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:192>>;
    {_, 193, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:193>>;
    {_, 194, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:194>>;
    {_, 195, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:195>>;
    {_, 196, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:196>>;
    {_, 197, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:197>>;
    {_, 198, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:198>>;
    {_, 199, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:199>>;
    {_, 200, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:200>>;
    {_, 201, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:201>>;
    {_, 202, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:202>>;
    {_, 203, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:203>>;
    {_, 204, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:204>>;
    {_, 205, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:205>>;
    {_, 206, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:206>>;
    {_, 207, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:207>>;
    {_, 208, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:208>>;
    {_, 209, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:209>>;
    {_, 210, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:210>>;
    {_, 211, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:211>>;
    {_, 212, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:212>>;
    {_, 213, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:213>>;
    {_, 214, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:214>>;
    {_, 215, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:215>>;
    {_, 216, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:216>>;
    {_, 217, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:217>>;
    {_, 218, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:218>>;
    {_, 219, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:219>>;
    {_, 220, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:220>>;
    {_, 221, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:221>>;
    {_, 222, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:222>>;
    {_, 223, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:223>>;
    {_, 224, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:224>>;
    {_, 225, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:225>>;
    {_, 226, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:226>>;
    {_, 227, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:227>>;
    {_, 228, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:228>>;
    {_, 229, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:229>>;
    {_, 230, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:230>>;
    {_, 231, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:231>>;
    {_, 232, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:232>>;
    {_, 233, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:233>>;
    {_, 234, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:234>>;
    {_, 235, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:235>>;
    {_, 236, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:236>>;
    {_, 237, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:237>>;
    {_, 238, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:238>>;
    {_, 239, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:239>>;
    {_, 240, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:240>>;
    {_, 241, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:241>>;
    {_, 242, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:242>>;
    {_, 243, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:243>>;
    {_, 244, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:244>>;
    {_, 245, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:245>>;
    {_, 246, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:246>>;
    {_, 247, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:247>>;
    {_, 248, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:248>>;
    {_, 249, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:249>>;
    {_, 250, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:250>>;
    {_, 251, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:251>>;
    {_, 252, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:252>>;
    {_, 253, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:253>>;
    {_, 254, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:254>>;
    {_, 255, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:255>>;
    {_, 256, integer, unsigned, native} ->
      <<Val:Size/unsigned-native-integer-unit:256>>;
    {_, 1, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:1>>;
    {_, 2, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:2>>;
    {_, 3, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:3>>;
    {_, 4, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:4>>;
    {_, 5, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:5>>;
    {_, 6, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:6>>;
    {_, 7, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:7>>;
    {_, 8, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:8>>;
    {_, 9, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:9>>;
    {_, 10, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:10>>;
    {_, 11, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:11>>;
    {_, 12, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:12>>;
    {_, 13, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:13>>;
    {_, 14, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:14>>;
    {_, 15, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:15>>;
    {_, 16, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:16>>;
    {_, 17, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:17>>;
    {_, 18, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:18>>;
    {_, 19, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:19>>;
    {_, 20, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:20>>;
    {_, 21, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:21>>;
    {_, 22, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:22>>;
    {_, 23, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:23>>;
    {_, 24, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:24>>;
    {_, 25, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:25>>;
    {_, 26, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:26>>;
    {_, 27, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:27>>;
    {_, 28, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:28>>;
    {_, 29, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:29>>;
    {_, 30, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:30>>;
    {_, 31, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:31>>;
    {_, 32, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:32>>;
    {_, 33, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:33>>;
    {_, 34, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:34>>;
    {_, 35, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:35>>;
    {_, 36, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:36>>;
    {_, 37, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:37>>;
    {_, 38, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:38>>;
    {_, 39, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:39>>;
    {_, 40, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:40>>;
    {_, 41, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:41>>;
    {_, 42, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:42>>;
    {_, 43, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:43>>;
    {_, 44, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:44>>;
    {_, 45, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:45>>;
    {_, 46, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:46>>;
    {_, 47, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:47>>;
    {_, 48, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:48>>;
    {_, 49, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:49>>;
    {_, 50, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:50>>;
    {_, 51, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:51>>;
    {_, 52, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:52>>;
    {_, 53, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:53>>;
    {_, 54, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:54>>;
    {_, 55, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:55>>;
    {_, 56, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:56>>;
    {_, 57, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:57>>;
    {_, 58, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:58>>;
    {_, 59, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:59>>;
    {_, 60, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:60>>;
    {_, 61, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:61>>;
    {_, 62, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:62>>;
    {_, 63, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:63>>;
    {_, 64, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:64>>;
    {_, 65, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:65>>;
    {_, 66, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:66>>;
    {_, 67, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:67>>;
    {_, 68, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:68>>;
    {_, 69, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:69>>;
    {_, 70, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:70>>;
    {_, 71, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:71>>;
    {_, 72, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:72>>;
    {_, 73, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:73>>;
    {_, 74, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:74>>;
    {_, 75, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:75>>;
    {_, 76, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:76>>;
    {_, 77, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:77>>;
    {_, 78, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:78>>;
    {_, 79, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:79>>;
    {_, 80, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:80>>;
    {_, 81, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:81>>;
    {_, 82, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:82>>;
    {_, 83, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:83>>;
    {_, 84, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:84>>;
    {_, 85, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:85>>;
    {_, 86, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:86>>;
    {_, 87, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:87>>;
    {_, 88, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:88>>;
    {_, 89, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:89>>;
    {_, 90, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:90>>;
    {_, 91, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:91>>;
    {_, 92, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:92>>;
    {_, 93, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:93>>;
    {_, 94, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:94>>;
    {_, 95, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:95>>;
    {_, 96, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:96>>;
    {_, 97, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:97>>;
    {_, 98, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:98>>;
    {_, 99, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:99>>;
    {_, 100, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:100>>;
    {_, 101, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:101>>;
    {_, 102, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:102>>;
    {_, 103, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:103>>;
    {_, 104, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:104>>;
    {_, 105, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:105>>;
    {_, 106, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:106>>;
    {_, 107, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:107>>;
    {_, 108, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:108>>;
    {_, 109, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:109>>;
    {_, 110, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:110>>;
    {_, 111, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:111>>;
    {_, 112, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:112>>;
    {_, 113, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:113>>;
    {_, 114, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:114>>;
    {_, 115, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:115>>;
    {_, 116, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:116>>;
    {_, 117, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:117>>;
    {_, 118, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:118>>;
    {_, 119, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:119>>;
    {_, 120, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:120>>;
    {_, 121, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:121>>;
    {_, 122, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:122>>;
    {_, 123, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:123>>;
    {_, 124, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:124>>;
    {_, 125, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:125>>;
    {_, 126, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:126>>;
    {_, 127, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:127>>;
    {_, 128, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:128>>;
    {_, 129, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:129>>;
    {_, 130, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:130>>;
    {_, 131, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:131>>;
    {_, 132, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:132>>;
    {_, 133, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:133>>;
    {_, 134, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:134>>;
    {_, 135, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:135>>;
    {_, 136, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:136>>;
    {_, 137, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:137>>;
    {_, 138, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:138>>;
    {_, 139, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:139>>;
    {_, 140, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:140>>;
    {_, 141, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:141>>;
    {_, 142, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:142>>;
    {_, 143, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:143>>;
    {_, 144, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:144>>;
    {_, 145, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:145>>;
    {_, 146, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:146>>;
    {_, 147, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:147>>;
    {_, 148, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:148>>;
    {_, 149, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:149>>;
    {_, 150, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:150>>;
    {_, 151, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:151>>;
    {_, 152, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:152>>;
    {_, 153, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:153>>;
    {_, 154, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:154>>;
    {_, 155, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:155>>;
    {_, 156, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:156>>;
    {_, 157, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:157>>;
    {_, 158, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:158>>;
    {_, 159, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:159>>;
    {_, 160, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:160>>;
    {_, 161, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:161>>;
    {_, 162, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:162>>;
    {_, 163, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:163>>;
    {_, 164, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:164>>;
    {_, 165, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:165>>;
    {_, 166, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:166>>;
    {_, 167, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:167>>;
    {_, 168, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:168>>;
    {_, 169, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:169>>;
    {_, 170, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:170>>;
    {_, 171, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:171>>;
    {_, 172, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:172>>;
    {_, 173, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:173>>;
    {_, 174, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:174>>;
    {_, 175, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:175>>;
    {_, 176, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:176>>;
    {_, 177, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:177>>;
    {_, 178, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:178>>;
    {_, 179, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:179>>;
    {_, 180, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:180>>;
    {_, 181, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:181>>;
    {_, 182, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:182>>;
    {_, 183, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:183>>;
    {_, 184, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:184>>;
    {_, 185, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:185>>;
    {_, 186, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:186>>;
    {_, 187, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:187>>;
    {_, 188, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:188>>;
    {_, 189, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:189>>;
    {_, 190, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:190>>;
    {_, 191, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:191>>;
    {_, 192, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:192>>;
    {_, 193, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:193>>;
    {_, 194, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:194>>;
    {_, 195, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:195>>;
    {_, 196, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:196>>;
    {_, 197, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:197>>;
    {_, 198, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:198>>;
    {_, 199, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:199>>;
    {_, 200, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:200>>;
    {_, 201, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:201>>;
    {_, 202, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:202>>;
    {_, 203, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:203>>;
    {_, 204, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:204>>;
    {_, 205, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:205>>;
    {_, 206, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:206>>;
    {_, 207, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:207>>;
    {_, 208, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:208>>;
    {_, 209, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:209>>;
    {_, 210, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:210>>;
    {_, 211, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:211>>;
    {_, 212, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:212>>;
    {_, 213, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:213>>;
    {_, 214, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:214>>;
    {_, 215, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:215>>;
    {_, 216, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:216>>;
    {_, 217, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:217>>;
    {_, 218, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:218>>;
    {_, 219, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:219>>;
    {_, 220, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:220>>;
    {_, 221, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:221>>;
    {_, 222, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:222>>;
    {_, 223, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:223>>;
    {_, 224, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:224>>;
    {_, 225, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:225>>;
    {_, 226, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:226>>;
    {_, 227, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:227>>;
    {_, 228, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:228>>;
    {_, 229, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:229>>;
    {_, 230, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:230>>;
    {_, 231, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:231>>;
    {_, 232, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:232>>;
    {_, 233, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:233>>;
    {_, 234, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:234>>;
    {_, 235, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:235>>;
    {_, 236, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:236>>;
    {_, 237, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:237>>;
    {_, 238, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:238>>;
    {_, 239, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:239>>;
    {_, 240, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:240>>;
    {_, 241, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:241>>;
    {_, 242, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:242>>;
    {_, 243, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:243>>;
    {_, 244, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:244>>;
    {_, 245, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:245>>;
    {_, 246, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:246>>;
    {_, 247, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:247>>;
    {_, 248, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:248>>;
    {_, 249, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:249>>;
    {_, 250, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:250>>;
    {_, 251, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:251>>;
    {_, 252, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:252>>;
    {_, 253, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:253>>;
    {_, 254, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:254>>;
    {_, 255, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:255>>;
    {_, 256, float, unsigned, native} ->
      <<Val:Size/unsigned-native-float-unit:256>>;
    {all, 1, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:1>>;
    {all, 2, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:2>>;
    {all, 3, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:3>>;
    {all, 4, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:4>>;
    {all, 5, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:5>>;
    {all, 6, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:6>>;
    {all, 7, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:7>>;
    {all, 8, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:8>>;
    {all, 9, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:9>>;
    {all, 10, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:10>>;
    {all, 11, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:11>>;
    {all, 12, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:12>>;
    {all, 13, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:13>>;
    {all, 14, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:14>>;
    {all, 15, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:15>>;
    {all, 16, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:16>>;
    {all, 17, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:17>>;
    {all, 18, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:18>>;
    {all, 19, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:19>>;
    {all, 20, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:20>>;
    {all, 21, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:21>>;
    {all, 22, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:22>>;
    {all, 23, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:23>>;
    {all, 24, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:24>>;
    {all, 25, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:25>>;
    {all, 26, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:26>>;
    {all, 27, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:27>>;
    {all, 28, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:28>>;
    {all, 29, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:29>>;
    {all, 30, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:30>>;
    {all, 31, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:31>>;
    {all, 32, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:32>>;
    {all, 33, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:33>>;
    {all, 34, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:34>>;
    {all, 35, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:35>>;
    {all, 36, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:36>>;
    {all, 37, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:37>>;
    {all, 38, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:38>>;
    {all, 39, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:39>>;
    {all, 40, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:40>>;
    {all, 41, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:41>>;
    {all, 42, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:42>>;
    {all, 43, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:43>>;
    {all, 44, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:44>>;
    {all, 45, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:45>>;
    {all, 46, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:46>>;
    {all, 47, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:47>>;
    {all, 48, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:48>>;
    {all, 49, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:49>>;
    {all, 50, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:50>>;
    {all, 51, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:51>>;
    {all, 52, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:52>>;
    {all, 53, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:53>>;
    {all, 54, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:54>>;
    {all, 55, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:55>>;
    {all, 56, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:56>>;
    {all, 57, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:57>>;
    {all, 58, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:58>>;
    {all, 59, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:59>>;
    {all, 60, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:60>>;
    {all, 61, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:61>>;
    {all, 62, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:62>>;
    {all, 63, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:63>>;
    {all, 64, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:64>>;
    {all, 65, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:65>>;
    {all, 66, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:66>>;
    {all, 67, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:67>>;
    {all, 68, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:68>>;
    {all, 69, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:69>>;
    {all, 70, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:70>>;
    {all, 71, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:71>>;
    {all, 72, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:72>>;
    {all, 73, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:73>>;
    {all, 74, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:74>>;
    {all, 75, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:75>>;
    {all, 76, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:76>>;
    {all, 77, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:77>>;
    {all, 78, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:78>>;
    {all, 79, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:79>>;
    {all, 80, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:80>>;
    {all, 81, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:81>>;
    {all, 82, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:82>>;
    {all, 83, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:83>>;
    {all, 84, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:84>>;
    {all, 85, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:85>>;
    {all, 86, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:86>>;
    {all, 87, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:87>>;
    {all, 88, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:88>>;
    {all, 89, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:89>>;
    {all, 90, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:90>>;
    {all, 91, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:91>>;
    {all, 92, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:92>>;
    {all, 93, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:93>>;
    {all, 94, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:94>>;
    {all, 95, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:95>>;
    {all, 96, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:96>>;
    {all, 97, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:97>>;
    {all, 98, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:98>>;
    {all, 99, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:99>>;
    {all, 100, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:100>>;
    {all, 101, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:101>>;
    {all, 102, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:102>>;
    {all, 103, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:103>>;
    {all, 104, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:104>>;
    {all, 105, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:105>>;
    {all, 106, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:106>>;
    {all, 107, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:107>>;
    {all, 108, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:108>>;
    {all, 109, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:109>>;
    {all, 110, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:110>>;
    {all, 111, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:111>>;
    {all, 112, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:112>>;
    {all, 113, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:113>>;
    {all, 114, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:114>>;
    {all, 115, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:115>>;
    {all, 116, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:116>>;
    {all, 117, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:117>>;
    {all, 118, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:118>>;
    {all, 119, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:119>>;
    {all, 120, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:120>>;
    {all, 121, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:121>>;
    {all, 122, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:122>>;
    {all, 123, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:123>>;
    {all, 124, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:124>>;
    {all, 125, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:125>>;
    {all, 126, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:126>>;
    {all, 127, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:127>>;
    {all, 128, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:128>>;
    {all, 129, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:129>>;
    {all, 130, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:130>>;
    {all, 131, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:131>>;
    {all, 132, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:132>>;
    {all, 133, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:133>>;
    {all, 134, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:134>>;
    {all, 135, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:135>>;
    {all, 136, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:136>>;
    {all, 137, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:137>>;
    {all, 138, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:138>>;
    {all, 139, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:139>>;
    {all, 140, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:140>>;
    {all, 141, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:141>>;
    {all, 142, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:142>>;
    {all, 143, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:143>>;
    {all, 144, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:144>>;
    {all, 145, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:145>>;
    {all, 146, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:146>>;
    {all, 147, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:147>>;
    {all, 148, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:148>>;
    {all, 149, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:149>>;
    {all, 150, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:150>>;
    {all, 151, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:151>>;
    {all, 152, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:152>>;
    {all, 153, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:153>>;
    {all, 154, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:154>>;
    {all, 155, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:155>>;
    {all, 156, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:156>>;
    {all, 157, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:157>>;
    {all, 158, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:158>>;
    {all, 159, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:159>>;
    {all, 160, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:160>>;
    {all, 161, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:161>>;
    {all, 162, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:162>>;
    {all, 163, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:163>>;
    {all, 164, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:164>>;
    {all, 165, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:165>>;
    {all, 166, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:166>>;
    {all, 167, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:167>>;
    {all, 168, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:168>>;
    {all, 169, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:169>>;
    {all, 170, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:170>>;
    {all, 171, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:171>>;
    {all, 172, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:172>>;
    {all, 173, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:173>>;
    {all, 174, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:174>>;
    {all, 175, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:175>>;
    {all, 176, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:176>>;
    {all, 177, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:177>>;
    {all, 178, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:178>>;
    {all, 179, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:179>>;
    {all, 180, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:180>>;
    {all, 181, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:181>>;
    {all, 182, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:182>>;
    {all, 183, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:183>>;
    {all, 184, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:184>>;
    {all, 185, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:185>>;
    {all, 186, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:186>>;
    {all, 187, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:187>>;
    {all, 188, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:188>>;
    {all, 189, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:189>>;
    {all, 190, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:190>>;
    {all, 191, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:191>>;
    {all, 192, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:192>>;
    {all, 193, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:193>>;
    {all, 194, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:194>>;
    {all, 195, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:195>>;
    {all, 196, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:196>>;
    {all, 197, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:197>>;
    {all, 198, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:198>>;
    {all, 199, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:199>>;
    {all, 200, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:200>>;
    {all, 201, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:201>>;
    {all, 202, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:202>>;
    {all, 203, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:203>>;
    {all, 204, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:204>>;
    {all, 205, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:205>>;
    {all, 206, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:206>>;
    {all, 207, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:207>>;
    {all, 208, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:208>>;
    {all, 209, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:209>>;
    {all, 210, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:210>>;
    {all, 211, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:211>>;
    {all, 212, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:212>>;
    {all, 213, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:213>>;
    {all, 214, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:214>>;
    {all, 215, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:215>>;
    {all, 216, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:216>>;
    {all, 217, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:217>>;
    {all, 218, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:218>>;
    {all, 219, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:219>>;
    {all, 220, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:220>>;
    {all, 221, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:221>>;
    {all, 222, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:222>>;
    {all, 223, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:223>>;
    {all, 224, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:224>>;
    {all, 225, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:225>>;
    {all, 226, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:226>>;
    {all, 227, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:227>>;
    {all, 228, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:228>>;
    {all, 229, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:229>>;
    {all, 230, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:230>>;
    {all, 231, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:231>>;
    {all, 232, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:232>>;
    {all, 233, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:233>>;
    {all, 234, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:234>>;
    {all, 235, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:235>>;
    {all, 236, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:236>>;
    {all, 237, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:237>>;
    {all, 238, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:238>>;
    {all, 239, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:239>>;
    {all, 240, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:240>>;
    {all, 241, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:241>>;
    {all, 242, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:242>>;
    {all, 243, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:243>>;
    {all, 244, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:244>>;
    {all, 245, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:245>>;
    {all, 246, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:246>>;
    {all, 247, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:247>>;
    {all, 248, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:248>>;
    {all, 249, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:249>>;
    {all, 250, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:250>>;
    {all, 251, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:251>>;
    {all, 252, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:252>>;
    {all, 253, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:253>>;
    {all, 254, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:254>>;
    {all, 255, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:255>>;
    {all, 256, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:256>>;
    {_, 1, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:1>>;
    {_, 2, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:2>>;
    {_, 3, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:3>>;
    {_, 4, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:4>>;
    {_, 5, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:5>>;
    {_, 6, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:6>>;
    {_, 7, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:7>>;
    {_, 8, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:8>>;
    {_, 9, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:9>>;
    {_, 10, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:10>>;
    {_, 11, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:11>>;
    {_, 12, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:12>>;
    {_, 13, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:13>>;
    {_, 14, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:14>>;
    {_, 15, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:15>>;
    {_, 16, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:16>>;
    {_, 17, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:17>>;
    {_, 18, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:18>>;
    {_, 19, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:19>>;
    {_, 20, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:20>>;
    {_, 21, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:21>>;
    {_, 22, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:22>>;
    {_, 23, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:23>>;
    {_, 24, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:24>>;
    {_, 25, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:25>>;
    {_, 26, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:26>>;
    {_, 27, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:27>>;
    {_, 28, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:28>>;
    {_, 29, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:29>>;
    {_, 30, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:30>>;
    {_, 31, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:31>>;
    {_, 32, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:32>>;
    {_, 33, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:33>>;
    {_, 34, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:34>>;
    {_, 35, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:35>>;
    {_, 36, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:36>>;
    {_, 37, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:37>>;
    {_, 38, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:38>>;
    {_, 39, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:39>>;
    {_, 40, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:40>>;
    {_, 41, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:41>>;
    {_, 42, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:42>>;
    {_, 43, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:43>>;
    {_, 44, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:44>>;
    {_, 45, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:45>>;
    {_, 46, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:46>>;
    {_, 47, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:47>>;
    {_, 48, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:48>>;
    {_, 49, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:49>>;
    {_, 50, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:50>>;
    {_, 51, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:51>>;
    {_, 52, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:52>>;
    {_, 53, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:53>>;
    {_, 54, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:54>>;
    {_, 55, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:55>>;
    {_, 56, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:56>>;
    {_, 57, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:57>>;
    {_, 58, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:58>>;
    {_, 59, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:59>>;
    {_, 60, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:60>>;
    {_, 61, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:61>>;
    {_, 62, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:62>>;
    {_, 63, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:63>>;
    {_, 64, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:64>>;
    {_, 65, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:65>>;
    {_, 66, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:66>>;
    {_, 67, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:67>>;
    {_, 68, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:68>>;
    {_, 69, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:69>>;
    {_, 70, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:70>>;
    {_, 71, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:71>>;
    {_, 72, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:72>>;
    {_, 73, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:73>>;
    {_, 74, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:74>>;
    {_, 75, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:75>>;
    {_, 76, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:76>>;
    {_, 77, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:77>>;
    {_, 78, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:78>>;
    {_, 79, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:79>>;
    {_, 80, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:80>>;
    {_, 81, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:81>>;
    {_, 82, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:82>>;
    {_, 83, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:83>>;
    {_, 84, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:84>>;
    {_, 85, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:85>>;
    {_, 86, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:86>>;
    {_, 87, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:87>>;
    {_, 88, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:88>>;
    {_, 89, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:89>>;
    {_, 90, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:90>>;
    {_, 91, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:91>>;
    {_, 92, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:92>>;
    {_, 93, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:93>>;
    {_, 94, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:94>>;
    {_, 95, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:95>>;
    {_, 96, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:96>>;
    {_, 97, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:97>>;
    {_, 98, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:98>>;
    {_, 99, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:99>>;
    {_, 100, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:100>>;
    {_, 101, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:101>>;
    {_, 102, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:102>>;
    {_, 103, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:103>>;
    {_, 104, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:104>>;
    {_, 105, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:105>>;
    {_, 106, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:106>>;
    {_, 107, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:107>>;
    {_, 108, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:108>>;
    {_, 109, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:109>>;
    {_, 110, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:110>>;
    {_, 111, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:111>>;
    {_, 112, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:112>>;
    {_, 113, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:113>>;
    {_, 114, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:114>>;
    {_, 115, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:115>>;
    {_, 116, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:116>>;
    {_, 117, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:117>>;
    {_, 118, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:118>>;
    {_, 119, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:119>>;
    {_, 120, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:120>>;
    {_, 121, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:121>>;
    {_, 122, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:122>>;
    {_, 123, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:123>>;
    {_, 124, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:124>>;
    {_, 125, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:125>>;
    {_, 126, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:126>>;
    {_, 127, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:127>>;
    {_, 128, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:128>>;
    {_, 129, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:129>>;
    {_, 130, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:130>>;
    {_, 131, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:131>>;
    {_, 132, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:132>>;
    {_, 133, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:133>>;
    {_, 134, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:134>>;
    {_, 135, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:135>>;
    {_, 136, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:136>>;
    {_, 137, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:137>>;
    {_, 138, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:138>>;
    {_, 139, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:139>>;
    {_, 140, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:140>>;
    {_, 141, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:141>>;
    {_, 142, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:142>>;
    {_, 143, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:143>>;
    {_, 144, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:144>>;
    {_, 145, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:145>>;
    {_, 146, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:146>>;
    {_, 147, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:147>>;
    {_, 148, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:148>>;
    {_, 149, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:149>>;
    {_, 150, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:150>>;
    {_, 151, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:151>>;
    {_, 152, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:152>>;
    {_, 153, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:153>>;
    {_, 154, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:154>>;
    {_, 155, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:155>>;
    {_, 156, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:156>>;
    {_, 157, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:157>>;
    {_, 158, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:158>>;
    {_, 159, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:159>>;
    {_, 160, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:160>>;
    {_, 161, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:161>>;
    {_, 162, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:162>>;
    {_, 163, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:163>>;
    {_, 164, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:164>>;
    {_, 165, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:165>>;
    {_, 166, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:166>>;
    {_, 167, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:167>>;
    {_, 168, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:168>>;
    {_, 169, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:169>>;
    {_, 170, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:170>>;
    {_, 171, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:171>>;
    {_, 172, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:172>>;
    {_, 173, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:173>>;
    {_, 174, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:174>>;
    {_, 175, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:175>>;
    {_, 176, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:176>>;
    {_, 177, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:177>>;
    {_, 178, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:178>>;
    {_, 179, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:179>>;
    {_, 180, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:180>>;
    {_, 181, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:181>>;
    {_, 182, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:182>>;
    {_, 183, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:183>>;
    {_, 184, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:184>>;
    {_, 185, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:185>>;
    {_, 186, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:186>>;
    {_, 187, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:187>>;
    {_, 188, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:188>>;
    {_, 189, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:189>>;
    {_, 190, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:190>>;
    {_, 191, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:191>>;
    {_, 192, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:192>>;
    {_, 193, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:193>>;
    {_, 194, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:194>>;
    {_, 195, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:195>>;
    {_, 196, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:196>>;
    {_, 197, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:197>>;
    {_, 198, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:198>>;
    {_, 199, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:199>>;
    {_, 200, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:200>>;
    {_, 201, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:201>>;
    {_, 202, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:202>>;
    {_, 203, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:203>>;
    {_, 204, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:204>>;
    {_, 205, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:205>>;
    {_, 206, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:206>>;
    {_, 207, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:207>>;
    {_, 208, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:208>>;
    {_, 209, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:209>>;
    {_, 210, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:210>>;
    {_, 211, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:211>>;
    {_, 212, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:212>>;
    {_, 213, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:213>>;
    {_, 214, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:214>>;
    {_, 215, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:215>>;
    {_, 216, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:216>>;
    {_, 217, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:217>>;
    {_, 218, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:218>>;
    {_, 219, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:219>>;
    {_, 220, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:220>>;
    {_, 221, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:221>>;
    {_, 222, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:222>>;
    {_, 223, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:223>>;
    {_, 224, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:224>>;
    {_, 225, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:225>>;
    {_, 226, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:226>>;
    {_, 227, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:227>>;
    {_, 228, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:228>>;
    {_, 229, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:229>>;
    {_, 230, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:230>>;
    {_, 231, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:231>>;
    {_, 232, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:232>>;
    {_, 233, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:233>>;
    {_, 234, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:234>>;
    {_, 235, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:235>>;
    {_, 236, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:236>>;
    {_, 237, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:237>>;
    {_, 238, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:238>>;
    {_, 239, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:239>>;
    {_, 240, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:240>>;
    {_, 241, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:241>>;
    {_, 242, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:242>>;
    {_, 243, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:243>>;
    {_, 244, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:244>>;
    {_, 245, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:245>>;
    {_, 246, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:246>>;
    {_, 247, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:247>>;
    {_, 248, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:248>>;
    {_, 249, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:249>>;
    {_, 250, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:250>>;
    {_, 251, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:251>>;
    {_, 252, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:252>>;
    {_, 253, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:253>>;
    {_, 254, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:254>>;
    {_, 255, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:255>>;
    {_, 256, binary, unsigned, native} ->
      <<Val:Size/unsigned-native-binary-unit:256>>;
  end.

