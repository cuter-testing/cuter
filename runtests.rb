#! /usr/bin/env ruby

ebin = "ebin"
suite = "testsuite/ebin"
tests = ["cuter"]
tests.each do |t|
  puts "Testing #{t} ..."
  puts `erl -noshell -pa #{ebin} #{suite} -eval "eunit:test(#{t}, [verbose])" -s init stop`
end

