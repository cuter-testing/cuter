-module(rectangle).
-author('ChrisRathman').

-export([new/4,slots/4,dispatch/1]).

% declare record to hold the slots for the class
-record(
  rectangle, {
     x,
     y,
     width,
     height
  }).

% populate the slots of the object record
slots(X, Y, Width, Height) ->
  #rectangle {
     x = X,
     y = Y,
     width = Width,
     height = Height}.

% create a process for the rectangle instance
new(X, Y, Width, Height) ->
  This = slots(X, Y, Width, Height),
  spawn(rectangle, dispatch, [This]).

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

     {Pid, getwidth} ->
        Pid!{retval, getwidth(This)},
        dispatch(This);
     {Pid, getheight} ->
        Pid!{retval, getheight(This)},
        dispatch(This);

     {setwidth, Width} ->
        dispatch(setwidth(This, Width));
     {setheight, Height} ->
        dispatch(setheight(This, Height));

     {Pid, draw} ->
        draw(This),
        Pid!{retval, true},
        dispatch(This);

     dispose ->
        true
  end.

% get the x & y coordinates for the object
getx(This) ->
  This#rectangle.x.
gety(This) ->
  This#rectangle.y.

% set the x & y coordinates for the object
setx(This, X) ->
  This#rectangle{x = X}.
sety(This, Y) ->
  This#rectangle{y = Y}.

% move the x & y position of the object
moveto(This, X, Y) ->
  setx(sety(This, Y), X).
rmoveto(This, DeltaX, DeltaY) ->
  moveto(This, getx(This) + DeltaX, gety(This) + DeltaY).

% get the width & height of the object
getwidth(This) ->
  This#rectangle.width.
getheight(This) ->
  This#rectangle.height.

% set the width and height of the object
setwidth(This, Width) ->
  This#rectangle{width = Width}.
setheight(This, Height) ->
  This#rectangle{height = Height}.

% draw the rectangle
draw(This) ->
  io:format('Drawing a Rectangle at:('),
  io:write(getx(This)),
  io:format(','),
  io:write(gety(This)),
  io:format('), width '),
  io:write(getwidth(This)),
  io:format(', height '),
  io:write(getheight(This)),
  io:format("~n").
