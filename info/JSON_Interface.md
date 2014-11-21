A term is encoded as follows:
{"d":dictionary-of-shared-subterms, "t":term-type, "v":term-value}

For example the terms {[1,2], [1,2]} is encoded as:

{
  "d": {
    "0.0.0.29": {
      "t": 4, // List type
      "v": [
        {"t": 1, "v": 1},
        {"t": 1, "v": 2}
      ]
    }
  },
  "t": 5, // Tuple type
  "v": [
    {"l": "0.0.0.29"},
    {"l": "0.0.0.29"}
  ]
}

Encoding of different Erlang types
----------------------------------

* Integer            =>  {"t" : 1, "v" : int-value}      e.g. {"t":1, "v": 42}
* Real               =>  {"t" : 2, "v" : float-value}    e.g. {"t":2, "v": 3.14}
* Atom               =>  {"t" : 3, "v" : [int-value+]}   e.g. {"t":3, "v":[111,107]}
* List               =>  {"t" : 4, "v" : [Term*]}        e.g. {"t":4, "v":[{"t":1, "v":42}]}
* Tuple              =>  {"t" : 5, "v" : [Term*]}        e.g. {"t":5, "v":[{"t":1, "v":42}]}
* Symbolic Variable  =>  {"s" : unicode-value}           e.g. {"s":"0.0.0.37"}

Encoding of constraints
-----------------------

* Guard True       =>  {"c" : 3, "a" : [Term]}  i.e. Term is the atom true
* Guard False      =>  {"c" : 4, "a" : [Term]}  i.e. Term is the atom false

* Match Equal      =>  {"c" : 5, "a" : [Term1, Term2]}  i.e. Term1 =:= Term2
* Match Not Equal  =>  {"c" : 6, "a" : [Term1, Term2]}  i.e. Term1 =/= Term2

* NonEmpty List    =>  {"c" : 10, "a" : [Term]}  i.e. Term is a nonempty list
* Empty List       =>  {"c" : 11, "a" : [Term]}  i.e. Term is an empty list
* Not a List       =>  {"c" : 12, "a" : [Term]}  i.e. Term is not a list

* Tuple of Size N      =>  {"c" : 7, "a" : [Term, N]}  i.e. Term is a tuple of size N
* Tuple of Not Size N  =>  {"c" : 8, "a" : [Term, N]}  i.e. Term is a tuple of not size N
* Not a Tuple          =>  {"c" : 9, "a" : [Term, N]}  i.e. Term is not a tuple


Encoding of important events
----------------------------

* Entry Point MFA's symbolic parameters  =>  {"c" : 1, "a" : [Symbolic-Params*]}

* Spawn a process       =>  {"c" : 13, "a" : [Child-Node, Child-Pid, Spawn-Ref]}
* Spawned by a process  =>  {"c" : 14, "a" : [Parent-Node, Parent-Pid, Spawn-Ref]}

* Send a message     =>  {"c" : 15, "a" : [Dest-Node, Dest-Pid, Msg-Ref]}
* Receive a message  =>  {"c" : 16, "a" : [From-Node, From-Pid, Msg-Ref]}
* Consume a message  =>  {"c" : 17, "a" : [From-Node, From-Pid, Msg-Ref]}

* Unfold a symbolic tuple  =>  {"c" : 18, "a" : [Term, Terms+]}  i.e. Term = {Term1, Term2, ..., TermN}
* Unfold a symbolic list   =>  {"c" : 19, "a" : [Term, Terms+]}  i.e. Term = [Term1, Term2, ..., TermN]


Encoding of Erlang BIFs
-----------------------

* erlang:hd/1  => {"c" : 25, "a" : [Term1, Term2]}  i.e. Term1 = erlang:hd(Term2)
* erlang:tl/1  => {"c" : 26, "a" : [Term1, Term2]}  i.e. Term1 = erlang:tl(Term2)
* erlang:is_integer/1  => {"c" : 27, "a" : [Term1, Term2]}  i.e. Term1 = erlang:is_integer(Term2)
* erlang:is_atom/1  => {"c" : 28, "a" : [Term1, Term2]}  i.e. Term1 = erlang:is_atom(Term2)
* erlang:is_float/1  => {"c" : 29, "a" : [Term1, Term2]}  i.e. Term1 = erlang:is_float(Term2)
* erlang:is_list/1  => {"c" : 30, "a" : [Term1, Term2]}  i.e. Term1 = erlang:is_list(Term2)
* erlang:is_tuple/1  => {"c" : 31, "a" : [Term1, Term2]}  i.e. Term1 = erlang:is_tuple(Term2)
* erlang:is_boolean/1  => {"c" : 32, "a" : [Term1, Term2]}  i.e. Term1 = erlang:is_boolean(Term2)
* erlang:is_number/1  => {"c" : 33, "a" : [Term1, Term2]}  i.e. Term1 = erlang:is_number(Term2)
* erlang:'+'/2  => {"c" : 34, "a" : [Term, Term2, Term3]}  i.e. Term = Term2 + Term3
* erlang:'-'/2  => {"c" : 35, "a" : [Term, Term2, Term3]}  i.e. Term = Term2 - Term3
* erlang:'*'/2  => {"c" : 36, "a" : [Term, Term2, Term3]}  i.e. Term = Term2 * Term3
* erlang:'/'/2  => {"c" : 37, "a" : [Term, Term2, Term3]}  i.e. Term = Term2 / Term3
