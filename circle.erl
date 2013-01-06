-module(circle).
-author('ChrisRathman').

-export([new/3,slots/3,dispatch/1]).

% declare record to hold the slots for the class
-record(
  circle, {
     super,
     x,
     y,
     radius
  }).

% populate the slots of the object record
slots(X, Y, Radius) ->
  #circle {
     x = X,
     y = Y,
     radius = Radius}.

% create a process for the circle instance
new(X, Y, Radius) ->
  This = slots(X, Y, Radius),
  spawn(circle, dispatch, [This]).

% dispatch the messages for the process as they are received
dispatch(This) ->
  receive
     {Pid, getx} ->
        Pid!{retval, getx(This)},
        dispatch(This);
     {Pid, gety} ->
        Pid!{retval, gety(This)},
        dispatch(This);

     {setx, X} ->
        dispatch(setx(This, X));
     {sety, Y} ->
        dispatch(sety(This, Y));

     {moveto, X, Y} ->
        dispatch(moveto(This, X, Y));
     {rmoveto, X, Y} ->
        dispatch(rmoveto(This, X, Y));

     {Pid, getradius} ->
        Pid!{retval, getradius(This)},
        dispatch(This);

     {setradius, Radius} ->
        dispatch(setradius(This,Radius));

     {Pid, draw} ->
        draw(This),
        Pid!{retval, true},
        dispatch(This);

     dispose ->
        true
  end.

% get the x & y coordinates for the object
getx(This) ->
  This#circle.x.
gety(This) ->
  This#circle.y.

% set the x & y coordinates for the object
setx(This, X) ->
  This#circle{x = X}.
sety(This, Y) ->
  This#circle{y = Y}.

% move the x & y position of the object
moveto(This, X, Y) ->
  setx(sety(This, Y), X).
rmoveto(This, DeltaX, DeltaY) ->
  moveto(This, getx(This) + DeltaX, gety(This) + DeltaY).

% get the radius of the object
getradius(This) ->
  This#circle.radius.

% set the radius of the object
setradius(This, Radius) ->
  This#circle{radius = Radius}.

% draw the circle
draw(This) ->
  io:format('Drawing a Circle at:('),
  io:write(getx(This)),
  io:format(','),
  io:write(gety(This)),
  io:format('), radius '),
  io:write(getradius(This)),
  io:format("~n").
