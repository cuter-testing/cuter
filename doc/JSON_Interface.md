# JSON Interface


A term is encoded as follows:

```javascript
{"d":dictionary-of-shared-subterms, "t":term-type, "v":term-value}
```

For example, the term

  ```erlang
  {[1,2], [1,2]}
  ```

is encoded as:

```javascript
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
```


### Encoding of different Erlang types


* **Integer**  
    
    ```javascript
    {"t" : 1, "v" : int-value}
    ```
    
    e.g.  
    ```javascript
    {"t":1, "v": 42}
    ```

* **Real**
    
    ```javascript
    {"t" : 2, "v" : float-value}
    ```
    
    e.g.  
    ```javascript
    {"t":2, "v": 3.14}
    ```

* **Atom**
    
    ```javascript
    {"t" : 3, "v" : [int-value+]}
    ```
    
    e.g.  
    ```javascript
    {"t":3, "v":[111,107]}
    ```

* **List**
    
    ```javascript
    {"t" : 4, "v" : [Term*]}`
    ```
    
    e.g.  
    ```javascript
    {"t":4, "v":[{"t":1, "v":42}]}
    ```

* **Tuple**
    
    ```javascript
    {"t" : 5, "v" : [Term*]}`
    ```
    
    e.g.  
    ```javascript  
    {"t":5, "v":[{"t":1, "v":42}]}
    ```

* **Symbolic Variable**
    
    ```javascript
    {"s" : string-value}
    ```
    
    e.g.  
    ```javascript
    {"s":"0.0.0.37"}
    ```


### Encoding of constraints


* **True Guard**
    
    ```javascript
    {"c" : 3, "a" : [T]}
    ```
    
    i.e.
    ```erlang
    T =:= true.
    ```

* **False Guard**
    
    ```javascript
    {"c" : 4, "a" : [T]}
    ```
    
    i.e.  
    ```erlang
    T =:= false.
    ```

* **Match Equal**
    
    ```javascript
    {"c" : 5, "a" : [T1, T2]}
    ```
    
    i.e.  
    ```erlang
    T1 =:= T2.
    ```

* **Match Not Equal**
    
    ```javascript
    {"c" : 6, "a" : [T1, T2]}
    ```
    
    i.e.  
    ```erlang
    T1 =/= T2.
    ```

* **Tuple of Size N**
    
    ```javascript
    {"c" : 7, "a" : [T, N]}
    ```
    
    i.e.  
    ```erlang
    T when is_tuple(T) andalso tuple_size(T) =:= N.
    ```

* **Tuple of Not Size N**
    
    ```javascript
    {"c" : 8, "a" : [T, N]}
    ```
    
    i.e.  
    ```erlang
    T when is_tuple(T) andalso tuple_size(T) =/= N.
    ```

* **Not a Tuple**
    
    ```javascript
    {"c" : 8, "a" : [T, N]}
    ```
    
    i.e.  
    ```erlang
    T when not is_tuple(T).
    ```

* **Nonempty List**
    
    ```javascript
    {"c" : 10, "a" : [T]}
    ```
    
    i.e.  
    ```erlang
    T when is_list(T) andalso T =/= [].
    ```

* **Empty List**
    
    ```javascript
    {"c" : 11, "a" : [T]}
    ```
    
    i.e.  
    ```erlang
    T =:= [].
    ```

* **Not a List**
    
    ```javascript
    {"c" : 12, "a" : [T]}
    ```
    
    i.e.  
    ```erlang
    T when not is_list(T).
    ```


### Encoding of important events


* Entry Point MFA's symbolic parameters  
    
    ```javascript
    {"c" : 1, "a" : [Symbolic-Params*]}
    ```

* Spawn a process  
    
    ```javascript
    {"c" : 13, "a" : [Child-Node, Child-Pid, Spawn-Ref]}
    ```

* Spawned by a process  
    
    ```javascript
    {"c" : 14, "a" : [Parent-Node, Parent-Pid, Spawn-Ref]}
    ```

* Send a message  
    
    ```javascript
    {"c" : 15, "a" : [Dest-Node, Dest-Pid, Msg-Ref]}
    ```

* Receive a message  
    
    ```javascript
    {"c" : 16, "a" : [From-Node, From-Pid, Msg-Ref]}
    ```

* Consume a message  
    
    ```javascript
    {"c" : 17, "a" : [From-Node, From-Pid, Msg-Ref]}
    ```

* Unfold a symbolic tuple  
    
    ```javascript
    {"c" : 18, "a" : [T, T1, ..., TN]}`
    ```
    
    i.e.  
    ```erlang
    T = {T1, ..., TN}.
    ```

* Unfold a symbolic list  
    
    ```javascript
    {"c" : 19, "a" : [T, T1, ..., TN]}
    ```
    
    i.e.  
    ```erlang
    T = [T1, ..., TN].
    ```


### Encoding of BIF Operations


* Get the head of a list  
    
    ```javascript
    {"c" : 25, "a" : [T1, T2]}
    ```
    
    i.e.  
    ```erlang
    T1 = hd(T2).
    ```

* Get the tail of a list  
    
    ```javascript
    {"c" : 26, "a" : [T1, T2]}
    ```
    
    i.e.  
    ```erlang
    T1 = tl(T2).
    ```

* Is a term is an integer  
    
    ```javascript
    {"c" : 27, "a" : [T1, T2]}
    ```
    
    i.e.  
    ```erlang
    T1 = is_integer(T2).
    ```

* Is a term is an atom  
    
    ```javascript
    {"c" : 28, "a" : [T1, T2]}
    ```
    
    i.e.  
    ```erlang
    T1 = is_atom(T2).
    ```

* Is a term is a float  
    
    ```javascript
    {"c" : 29, "a" : [T1, T2]}
    ```
    
    i.e.  
    ```erlang
    T1 = is_float(T2).
    ```

* Is a term is a list  
    
    ```javascript
    {"c" : 30, "a" : [T1, T2]}
    ```
    
    i.e.  
    ```erlang
    T1 = is_list(T2).
    ```

* Is a term is a tuple  
    
    ```javascript
    {"c" : 31, "a" : [T1, T2]}
    ```
    
    i.e.  
    ```erlang
    T1 = is_tuple(T2).
    ```

* Is a term is a boolean  
    
    ```javascript
    {"c" : 32, "a" : [T1, T2]}
    ```
    
    i.e.  
    ```erlang
    T1 = is_boolean(T2).
    ```

* Is a term is a number  
    
    ```javascript
    {"c" : 33, "a" : [T1, T2]}
    ```
    
    i.e.  
    ```erlang
    T1 = is_number(T2).
    ```

* Add two numbers  
    
    ```javascript
    {"c" : 34, "a" : [T, T2, T3]}
    ```
    
    i.e.  
    ```erlang
    T = T1 + T2.
    ```

* Subtract two numbers  
    
    ```javascript
    {"c" : 35, "a" : [T, T2, T3]}
    ```
    
    i.e.  
    ```erlang
    T = T1 - T2.
    ```

* Multiply two numbers  
    
    ```javascript
    {"c" : 36, "a" : [T, T2, T3]}
    ```
    
    i.e.  
    ```erlang
    T = T1 * T2.
    ```

* Divide two numbers  
    
    ```javascript
    {"c" : 37, "a" : [T, T2, T3]}
    ```
    
    i.e.  
    ```erlang
    T = T1 / T2.
    ```

* Integer division of natural numbers  
    
    ```javascript
    {"c" : 38, "a" : [T, T2, T3]}
    ```
    
    i.e.  
    ```erlang
    T = T1 div T2.  % when T2 >= 0 andalso T3 > 0
    ```

* Remainder of integer division of natural numbers  
    
    ```javascript
    {"c" : 39, "a" : [T, T2, T3]}
    ```
    
    i.e.  
    ```erlang
    T = T1 rem T2.  % when T2 >= 0 andalso T3 > 0
    ```

* Unary operation  
    
    ```javascript
    {"c" : 40, "a" : [T, T1]}
    ```
    
    i.e.  
    ```erlang
    T = - T1.
    ```

* Equality of terms  
    
    ```javascript
    {"c" : 41, "a" : [T, T1, T2]}
    ```
    
    i.e.  
    ```erlang
    T = T1 =:= T2.
    ```

* Inequality of terms  
    
    ```javascript
    {"c" : 42, "a" : [T, T1, T2]}
    ```
    
    i.e.  
    ```erlang
    T = T1 =/= T2.
    ```

* Convert a number to float  
    
    ```javascript
    {"c" : 47, "a" : [T, T1]}
    ```
    
    i.e.  
    ```erlang
    T = float(T1).
    ```
