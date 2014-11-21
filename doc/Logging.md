# Logging


The module responsible for logging is **cuter_log**.


### Logging Constraints


* `log_guard/3`  
    
    Constraints with guards.

* `log_equal/4`  
    
    Constraints with matching literals.

* `log_tuple/4`  
    
    Constratins with matching tuples.

* `log_list/3`  
    
    Constraints with matching lists.


### Logging Process Spawns


* `log_spawn/3`  
    
    Spawn a process.

* `log_spawned/3`  
    
    Spawned by a process.


### Logging Message Passing


* `log_message_sent/3`  
    
    Send a message.

* `log_message_received/3`  
    
    Receive a message (but not consume).

* `log_message_consumed/3`  
    
    Consume a message.


### Log Entry Point MFA Details


* `log_symb_params/2`  
    
    The symbolic parameters.


### Log symbolic operations


* `log_unfold_symbolic/4`  
    
    Unfolding of a symbolic variable that represents a list or a tuple of values.

* `log_mfa/4`  
    
    Symbolic operation of a BIF operation.
