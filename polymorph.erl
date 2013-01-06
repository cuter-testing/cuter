-module(polymorph).
-author('ChrisRathman').

-export([tryme/0]).

% test polymorphism in Erlang
tryme() ->
  % create a list containing various shape process instances
  Scribble = [
     rectangle:new(10,20,5,6),
     circle:new(15,25,8)],

  % iterate through the list and handle shapes polymorphically
  drawloop(Scribble),

  % dispose of the processes
  disposeloop(Scribble),

  % call a rectangle specific function
  ARectangle = rectangle:new(0,0,15,15),
  ARectangle!{setwidth, 30},
  ARectangle!{self(), draw},
  retrieve(),
  ARectangle!dispose,
  true.

% iterate through the list of shapes
drawloop([]) -> true;
drawloop([Shape|Tail]) ->
  Shape!{self(), draw},
  retrieve(),
  Shape!{rmoveto, 100, 100},
  Shape!{self(), draw},
  retrieve(),
  drawloop(Tail).

% close out the object processes
disposeloop([]) -> true;
disposeloop([Shape|Tail]) ->
  Shape!dispose,
  disposeloop(Tail).

% wait for process to return result
retrieve() ->
  receive
     {retval, Any} -> Any
  end.
