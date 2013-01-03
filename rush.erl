-module(rush).
-export([solve/4, statistics/4]).

%% Contain the input validation, execute the A* algorithm and return the optimal path
solve(W, H, {XE,YE}, L) ->
	case validate(W, H, {XE,YE}, L) of
		false -> -1;
		true ->
			StartState = lists:map(fun orientation/1,normalize(L, [])),
			aStar (W, H, {XE,YE}, StartState)
	end.

%% Solve the problem and display statistics
statistics(W, H, {XE,YE}, L) ->
	{Time,Val} = timer:tc(rush,solve,[W, H, {XE,YE}, L]),
	io:format("Solution is ~w moves.\nExecution time = ~w sec.\n",[Val,Time/1000000]).


%%%%%%%%%%%%%%  Input validation and preparation  %%%%%%%%%%%%%%%%%%%%%

%% Validate the starting State input
%% W, H >= 0
%% The red car must be able to exit if it's not blocked
validate (_, _, _, []) -> false;
validate (W, H, {X,_}, [{0, {X,_}, {X,_}}|_]) ->
	case ((W =< 0) or (H =< 0)) of
		true -> false;
		false -> true
	end;
validate (W, H, {_,Y}, [{0, {_,Y}, {_,Y}}|_]) ->
	case ((W =< 0) or (H =< 0)) of
		true -> false;
		false -> true
	end;
validate (W, H, Exit, [_|T]) ->
	validate (W, H, Exit, T);
validate (_,_,_,_) -> false.

%% Get the red car first on the State list
normalize ([{0, C1, C2}|T], Acc) ->
	[{0, C1, C2}|lists:reverse(Acc) ++ T];
normalize ([{Num, C1, C2}|T], Acc) ->
	normalize (T, [{Num, C1, C2}|Acc]).

%% Rearranges car cell coordinates so the left cell on the tuple is the uppermost on vertical or leftmost on horizontal orientations
orientation ({Car, {X,Y1}, {X,Y2}}) -> 
	case Y1 > Y2 of
		true ->
			{Car, {X,Y2}, {X,Y1}};
		false ->
			{Car, {X,Y1}, {X,Y2}}
	end;
orientation ({Car, {X1,Y}, {X2,Y}}) -> 
	case X1 > X2 of
		true ->
			{Car, {X2,Y}, {X1,Y}};
		false ->
			{Car, {X1,Y}, {X2,Y}}
	end;
orientation (_) -> false.

%%%%%%%%%%%%%%%%%%%  Auxiliary functions %%%%%%%%%%%%%%%%%%%%%%%%

%% Finds the occupied cells on the board
carcells ({_, {X,Y1}, {X,Y2}}) ->
	F = fun(N) -> {X, N} end,
	lists:map(F, lists:seq(Y1, Y2));
carcells ({_, {X1,Y}, {X2,Y}}) ->
	F = fun(N) -> {N, Y} end,
	lists:map(F, lists:seq(X1, X2)).

%% Finds the free cells on the board
freecells (W, H, L) ->
	[{X,Y} || X <- lists:seq(0,W-1), Y <- lists:seq(0,H-1), not lists:member({X,Y},L)].

%% Find available moves for a car
freecarcells ({Car, {X,Y1}, {X,Y2}}, _, H, FreeCells) -> 
	case Y1 == 0 of
		true -> 
			UpMoves = [];
		false ->
			UpMoves = freecarcells (Car, {X,Y1-1}, {X,Y2-1}, FreeCells, H, up, [])
	end,
	case Y2 == H-1 of
		true ->
			DownMoves = [];
		false ->
			DownMoves = freecarcells (Car, {X,Y1+1}, {X,Y2+1}, FreeCells, H, down, [])
	end,
	UpMoves ++ DownMoves;
freecarcells ({Car, {X1,Y}, {X2,Y}}, W, _, FreeCells) ->
	case X1 == 0 of
		true ->
			LeftMoves = [];
		false ->
			LeftMoves = freecarcells (Car, {X1-1,Y}, {X2-1,Y}, FreeCells, W, left, [])
	end,
	case X2 == W-1 of
		true ->
			RightMoves = [];
		false ->
			RightMoves = freecarcells (Car, {X1+1,Y}, {X2+1,Y}, FreeCells, W, right, [])
	end,
	LeftMoves ++ RightMoves.

freecarcells (Car, {X,Y1}, {X,Y2}, FreeCells, H, down, Acc) ->
	case Y2 == H of
		true ->
			lists:reverse(Acc);
		false ->
			case lists:member({X,Y2}, FreeCells) of
				true->
					freecarcells (Car, {X,Y1+1}, {X,Y2+1}, FreeCells, H, down, [{Car,{X,Y1},{X,Y2}}|Acc]);
				false ->
					lists:reverse(Acc)
			end
	end;
freecarcells (Car, {X,Y1}, {X,Y2}, FreeCells, H, up, Acc) ->
	case Y1 == -1 of
		true ->
			lists:reverse(Acc);
		false ->
			case lists:member({X,Y1}, FreeCells) of
				true->
					freecarcells (Car, {X,Y1-1}, {X,Y2-1}, FreeCells, H, up, [{Car,{X,Y1},{X,Y2}}|Acc]);
				false ->
					lists:reverse(Acc)
			end
	end;
freecarcells (Car, {X1,Y}, {X2,Y}, FreeCells, W, left, Acc) ->
	case X1 == -1 of
		true ->
			lists:reverse(Acc);
		false ->
			case lists:member({X1,Y}, FreeCells) of
				true ->
					freecarcells (Car, {X1-1,Y}, {X2-1, Y}, FreeCells, W, left, [{Car,{X1,Y},{X2,Y}}|Acc]);
				false ->
					lists:reverse(Acc)
			end
	end;
freecarcells (Car, {X1,Y}, {X2,Y}, FreeCells, W, right, Acc) ->
	case X2 == W of
		true ->
			lists:reverse(Acc);
		false ->
			case lists:member({X2,Y}, FreeCells) of
				true ->
					freecarcells (Car, {X1+1,Y}, {X2+1, Y}, FreeCells, W, right, [{Car,{X1,Y},{X2,Y}}|Acc]);
				false ->
					lists:reverse(Acc)
			end
	end.

%% Create new state from move
createnewstate ({MCar, {NewX1,NewY1}, {NewX2,NewY2}}, [{Car,{OldX1,OldY1}, {OldX2,OldY2}}|T], Acc) ->
	case MCar == Car of
		true ->
			lists:reverse([{MCar, {NewX1,NewY1}, {NewX2,NewY2}}|Acc]) ++ T;
		false ->
			createnewstate ({MCar, {NewX1,NewY1}, {NewX2,NewY2}}, T, [{Car,{OldX1,OldY1}, {OldX2,OldY2}}|Acc])
	end.

%% Create all new states
createallnewstates ([], _, Acc) ->
	lists:reverse(Acc);
createallnewstates ([Move|T], CurrentState, Acc) ->
	NewState = createnewstate (Move, CurrentState, []),
	createallnewstates (T, CurrentState, [NewState|Acc]).

%% Find all the Child States of the CurrentState
findChilds (CurrentState, W, H) ->
	OccupiedCells = lists:merge(lists:map(fun carcells/1, CurrentState)),
	FreeCells = freecells (W, H, OccupiedCells),
	FindAllMoves = fun(N) -> freecarcells (N, W, H, FreeCells) end,
	AvailableMoves = lists:flatten(lists:map(FindAllMoves, CurrentState)),
	createallnewstates (AvailableMoves, CurrentState, []).

%% Check if the current state is the Goal state
isGoal([{0, {X,Y1}, {X,Y2}}|_], {X, YE}) ->
	((Y1 == YE) or (Y2 == YE));
isGoal([{0, {X1,Y}, {X2,Y}}|_], {XE, Y}) ->
	((X1 == XE) or (X2 == XE)).

%% Find optimal path for red car as if there weren't any other cars
optimalPath([{0, {X1,Y}, {X2,Y}}|_], {XE,Y}) ->
	F = fun(N) -> {N, Y} end,
	case XE >= X2 of
		true ->
			lists:map(F, lists:seq(X2+1, XE));
		false ->
			lists:map(F, lists:seq(XE, X1-1))
	end;
optimalPath([{0, {X,Y1}, {X,Y2}}|_], {X,YE}) ->
	F = fun(N) -> {X, N} end,
	case YE >= Y2 of
		true ->
			lists:map(F, lists:seq(Y2+1, YE));
		false ->
			lists:map(F, lists:seq(YE, Y1-1))
	end.

%% Heurestic value of the state (how many cars are blocking the way)
heurestic(State, {XE,YE}) ->
	OccupiedCells = lists:merge(lists:map(fun carcells/1, State)),
	Path = optimalPath(State, {XE,YE}),
	length([X || X <- Path, lists:member(X, OccupiedCells)]).


%%%%%%%%%%%%%%%%% Implementation of A* Algorithm %%%%%%%%%%%%%%%%%%%%%%%%

%% Initialization of the A* Algorithm
aStar (W, H, {XE,YE}, StartState) ->
	OpenSet = sets:add_element(StartState, sets:new()),
	ClosedSet = sets:new(),
	Heurestic = heurestic(StartState, {XE,YE}),
	Gvalue = dict:append(StartState, 0, dict:new()),
	Hvalue = dict:append(StartState, Heurestic, dict:new()),
	Fvalue = dict:append(StartState, Heurestic, dict:new()),
	ParentNode = dict:append(StartState, root, dict:new()),
	aStarLoop (W, H, {XE,YE}, OpenSet, ClosedSet, Fvalue, Gvalue, Hvalue, ParentNode).

%% Main A* Loop
aStarLoop (W, H, {XE,YE}, OpenSet, ClosedSet, Fvalue, Gvalue, Hvalue, ParentNode) ->
	case sets:size(OpenSet) of
		0 ->
			-1;
		_ ->
			NewState = reorderOpenSet(OpenSet, Fvalue, init, init),
			case isGoal(NewState, {XE,YE}) of
				true -> 
%%					reconstructOptimalPath(NewState, ParentNode, []);
					reconstructOptimalPath(NewState, ParentNode, 0);
				false -> 
					NewOpenSet = sets:del_element(NewState, OpenSet),
					NewClosedSet = sets:add_element(NewState, ClosedSet),
					Childs = findChilds (NewState, W, H),
					{NextOpenSet,NextFvalue,NextGValue,NextHvalue,NextParentNode} = insertChildstoOpenSet({XE,YE}, Childs, NewOpenSet, NewClosedSet, Fvalue, Gvalue, Hvalue, ParentNode, NewState),	
					aStarLoop(W, H, {XE,YE}, NextOpenSet, NewClosedSet, NextFvalue, NextGValue, NextHvalue, NextParentNode)
			end
	end.

%% Reconstruct the optimal path when A* finds one
reconstructOptimalPath(State, ParentNode, Acc) ->
	[Parent] = dict:fetch(State, ParentNode),
	case Parent of
		root ->
%%			[State|Acc];
			Acc;
		ParentState ->
%%			reconstructOptimalPath(ParentState, ParentNode, [State|Acc])
			reconstructOptimalPath(ParentState, ParentNode, Acc+1)
	end.

%% Insert a node's children into the open set and updating the OpenList and ClosedList if necessary
insertChildstoOpenSet(_, [], OpenSet, _, Fvalue, Gvalue, Hvalue, ParentNode, _) -> 
	{OpenSet, Fvalue, Gvalue, Hvalue, ParentNode};
insertChildstoOpenSet({XE,YE}, [Hd|T], OpenSet, ClosedSet, Fvalue, Gvalue, Hvalue, ParentNode, ParentState) ->
	[PStateG] = dict:fetch(ParentState, Gvalue),
	HdG = PStateG + 1,
	case sets:is_element(Hd, ClosedSet) of
		true -> 
			[OldHdG] = dict:fetch(Hd, Gvalue),
			case HdG < OldHdG of
				true ->
					NewGvalue = dict:append(Hd,HdG, dict:erase(Hd,Gvalue)),
					[HdH] = dict:fetch(Hd, Hvalue),
					NewFvalue = dict:append(Hd,HdH+HdG, dict:erase(Hd,Fvalue)),
					NewParentNode = dict:append(Hd,ParentState, dict:erase(Hd,ParentNode)),
					insertChildstoOpenSet({XE,YE}, T, OpenSet, ClosedSet, NewFvalue, NewGvalue, Hvalue, NewParentNode, ParentState);
				false ->
					insertChildstoOpenSet({XE,YE}, T, OpenSet, ClosedSet, Fvalue, Gvalue, Hvalue, ParentNode, ParentState)
			end;
		false -> 
			case sets:is_element(Hd, OpenSet) of
				true -> 
					[OldHdG] = dict:fetch(Hd, Gvalue),
					case HdG < OldHdG of
						true ->
							NewGvalue = dict:append(Hd,HdG, dict:erase(Hd,Gvalue)),
							[HdH] = dict:fetch(Hd, Hvalue),
							NewFvalue = dict:append(Hd,HdH+HdG, dict:erase(Hd,Fvalue)),
							NewParentNode = dict:append(Hd,ParentState, dict:erase(Hd,ParentNode)),
							insertChildstoOpenSet({XE,YE}, T, OpenSet, ClosedSet, NewFvalue, NewGvalue, Hvalue, NewParentNode, ParentState);
						false ->
							insertChildstoOpenSet({XE,YE}, T, OpenSet, ClosedSet, Fvalue, Gvalue, Hvalue, ParentNode, ParentState)
					end;
				false -> 
					NewOpenSet = sets:add_element(Hd, OpenSet),
					Heurestic = heurestic(Hd, {XE,YE}),
					NewHvalue = dict:append(Hd, Heurestic, Hvalue),
					NewGvalue = dict:append(Hd, HdG, Gvalue),
					NewFvalue = dict:append(Hd, HdG+Heurestic, Fvalue),
					NewParentNode = dict:append(Hd, ParentState, ParentNode),
					insertChildstoOpenSet({XE,YE}, T, NewOpenSet, ClosedSet, NewFvalue, NewGvalue, NewHvalue, NewParentNode, ParentState)
			end
	end.
	

%% Reorder the OpenList to find the State with the lowest F value
reorderOpenSet(OpenSet, Fvalue, init, init) ->
	[H|T] = sets:to_list(OpenSet),
	[F] = dict:fetch(H, Fvalue),
	reorderOpenSet(T, Fvalue, H, F);
reorderOpenSet([], _, BestState, _) ->
	BestState;
reorderOpenSet([H|T], Fvalue, CurrBestState, CurrBestValue) ->
	[F] = dict:fetch(H, Fvalue),
	case F < CurrBestValue of
		true ->
			reorderOpenSet(T, Fvalue, H, F);
		false ->
			reorderOpenSet(T, Fvalue, CurrBestState, CurrBestValue)
	end.
