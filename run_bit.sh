#!/usr/bin/env bash

SEP="$(printf '%.0s-' {1..42}; echo)"
LINE="$(printf '%.0s=' {1..80}; echo)"
MOD=bit
FLAGS="-d -p=4 -s=4"

erlc +debug_info bit.erl

echo ${LINE}
echo "CONSTRUCTING BITSTRINGS"
echo ${LINE}
./cuter ${MOD} f11 '[0]' 2 ${FLAGS}
echo ${SEP}
./cuter ${MOD} f12 '[0,0,0]' 6 ${FLAGS}
echo ${SEP}
./cuter ${MOD} f13 '[0,0,0]' 15 ${FLAGS}
echo ${LINE}
echo "MATCHING AGAINST A CONSTANT VALUE"
echo ${LINE}
./cuter ${MOD} f21 '[<<>>]' 6 ${FLAGS}
echo ${SEP}
./cuter ${MOD} f22 '[<<>>,42]' 8 ${FLAGS}
echo ${SEP}
./cuter ${MOD} f23 '[0,0]' 7 ${FLAGS}
echo ${LINE}
echo "MATCHING AND BINDING VARIABLES"
echo ${LINE}
./cuter ${MOD} f31 '[<<>>,0]' 6 ${FLAGS}
echo ${SEP}
./cuter ${MOD} f32 '[<<>>,0,3]' 3 ${FLAGS}
echo ${SEP}
./cuter ${MOD} f33 '[<<>>,0,0,<<>>]' 5 ${FLAGS}
