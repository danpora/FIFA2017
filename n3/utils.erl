-module(utils).
-compile(export_all).
-define(GOAL_A_POS, {0, {250,350}}).
-define(GOAL_B_POS, {1000, {250,350}}).
-define(Ball_DECCELERATION, 0.5).
-define(RECEIVING_PASS_SPEED, 8).
-define(SHOOT_RANGE, 300).
-define(FIELD_HEIGHT, 600).
-define(FIELD_WIDTH, 1000).
-define(TRACE, false).

minInt(Num1, Num2) when Num1 =< Num2 -> Num1;
minInt(_, Num2) -> Num2.

angle({X1,Y1}, {X2,Y2}) ->
	math:atan2(Y1-Y2, X2-X1).

%% gets the ID of the opponent's team
oppTeam('A') -> 'B';
oppTeam('B') -> 'A'.

getSign('A') -> -1;
getSign('B') -> 1.

%% determines weather X1 is in front of X2 in the field, while both X's are from team <TeamID>
isInFront(X1, X2, 'A') -> X1 > X2;
isInFront(X1, X2, 'B') -> X1 < X2.

trace(String, ArgList) ->
	trace(String, ArgList, ?TRACE).

trace(_, _, false) -> notrace;
trace(String, ArgList, true) ->
	io:fwrite(String, ArgList).

distPoints({X1,Y1},{X2,Y2}) ->
	math:sqrt(math:pow(X2-X1,2) + math:pow(Y2-Y1,2)).

getGoalPos('A') -> ?GOAL_A_POS;
getGoalPos('B') -> ?GOAL_B_POS.

getFieldDimentions() -> {?FIELD_WIDTH, ?FIELD_HEIGHT}.

isPlayersInDangerZone(PlayersInDangerZone) ->
	PlayersInDangerZone =/= [].

getPizzaArea('A') -> {-1*math:pi()/6, math:pi()/6};
getPizzaArea('B') -> {2*math:pi()/3, -2*math:pi()/3}.

getForwardHalfCircle('A') -> {-1*math:pi()/2, math:pi()/2};
getForwardHalfCircle('B') -> {math:pi()/2, -1*math:pi()/2}.

getKickInfo(_, _, OppTeamID) ->
	{GoalX, {TopPost,BottomPost}} = getGoalPos(OppTeamID),
	GoalLength = BottomPost - TopPost,
	RandomKick = random:uniform()*(GoalLength*4/3) + TopPost - GoalLength/6,
	{GoalX, RandomKick}.

% getKickInfo(GKPos, _, OppTeamID) ->
% 	{GoalX, {TopPost,BottomPost}} = getGoalPos(OppTeamID),
% 	{_, GK_Ypos} = GKPos,
% 	DistGKTopPost = distPoints(GKPos, {GoalX, TopPost}),
% 	DistGKBottomPost = distPoints(GKPos, {GoalX, BottomPost}),
% 	if DistGKTopPost =< DistGKBottomPost ->
% 		{GoalX, (TopPost + BottomPost)/2}; %%{GoalX, (BottomPost + GK_Ypos) / 2)};
% 	true ->
% 		{GoalX, (TopPost + BottomPost)/2} %%{GoalX, (GK_Ypos + TopPost) / 2)}
% 	end.

bestPassOption(_, []) -> {cannotPass, {0,0,0}};
bestPassOption(PlayerTabID, FreePlayers) ->
	PlayerID = ets:first(PlayerTabID),
	{My_X, _} = ets:lookup_element(PlayerTabID, PlayerID, 4),
	TeamID = ets:lookup_element(PlayerTabID, PlayerID, 2),
	trace("~p: best Pass option, FreePlayers: ~p~n", [PlayerID, FreePlayers]),		
	{DestPlayerID, X, Y, _} = hd(FreePlayers),
	B = isInFront(X, My_X, TeamID),	
	if B ->
		{forwardPass, {DestPlayerID, X, Y}};
	true ->
		{backwardPass, {DestPlayerID, X, Y}}
	end.

%% returns a tuple of data regarding opponents in NZ.
%% data represents as: {IsOppInNZ, HasBall, OppPosition}
oppInNatZoneInfo([]) -> {false, false, {0,0}};
oppInNatZoneInfo(OpponentsInNaturalZone) ->
	PlayerPosTup = lists:keyfind(true, 4, OpponentsInNaturalZone),
	if (PlayerPosTup =:= false) ->
			{_, Xpos, Ypos, _} = lists:last(OpponentsInNaturalZone),
			OpponentPosition = {Xpos, Ypos},
			{true, false, OpponentPosition};
	   true ->
			{_,Xpos,Ypos,_} = PlayerPosTup,
			{true, true ,{Xpos, Ypos}}
	end.

%% isInShootArea(PlayerPos, OpponentsTeamID) ->	 	  
isInShootArea(PlayerPos, OppTeamID) ->	 
	{GoalX, {GoalTopY, GoalBottomY}} = getGoalPos(OppTeamID),
	X = GoalX + getSign(OppTeamID)*math:sqrt(3)*(GoalBottomY - GoalTopY)/2,
	Y = (GoalBottomY + GoalTopY)/2,
	distPoints(PlayerPos, {X, Y}) =< ?SHOOT_RANGE.

%% isPointInReactanle(Point, Rectangle)
isPointInReactanle({X, Y}, {MiddleX, MiddleY, Width, Height}) ->
	{LeftX, RightX, TopY, BottomY} = {MiddleX - Width/2, MiddleX + Width/2, MiddleY - Height/2, MiddleY + Height/2},
	(X >= LeftX andalso X =< RightX andalso Y =< BottomY andalso Y >= TopY).

calcNextPosition({Current_X, Current_Y}, DestinationXY, Speed) ->	
	Dist = distPoints(DestinationXY, {Current_X, Current_Y}),
	Angle = angle({Current_X, Current_Y}, DestinationXY),
	if Dist =< Speed ->
		DestinationXY;
	true ->
		Pos = {Current_X + math:cos(Angle)*Speed, Current_Y - math:sin(Angle)*Speed},
		%io:fwrite("cal next position: ~p~n when I am at: ~p and dest is: ~p~n", [Pos, {Current_X, Current_Y}, DestinationXY]),
		Pos
	end.

calcPassSpeed(PlayerPos, DestPlayerPos) ->
	Dist = distPoints(PlayerPos, DestPlayerPos),
	math:sqrt(?RECEIVING_PASS_SPEED*?RECEIVING_PASS_SPEED + 2*?Ball_DECCELERATION*Dist). %% formula: v0^2 = v^2 - 2aX

calcTimeWaitForPass(PlayerPos, SourcePos) ->
	V0 = calcPassSpeed(PlayerPos, SourcePos),
	round(0.9*(V0 - ?RECEIVING_PASS_SPEED)/?Ball_DECCELERATION). %% formula = (v - v0)/a

%% case attacking goal with X=0 coordinate
%% getRandomPointToGo(OpponentTeamID, PlayerPos, Speed) ->
getRandomPointToGo(OpponentTeamID, {Curr_X, Curr_Y}, Speed, HasBall, Position) ->
	{GoalX, {GoalTopY, GoalBottomY}} = getGoalPos(OpponentTeamID),
	{OwnGoalX, {_, _}} = getGoalPos(oppTeam(OpponentTeamID)), %% get own goal position
	DistFromOwnGoal = distPoints({Curr_X, 0}, {OwnGoalX, 0}),
	DistFromGoal = distPoints({Curr_X, 0}, {GoalX, 0}),
	RandomAngle = (random:uniform()-0.5)*math:pi()/3, %% Picks a random angle between [-pi/6, pi/6]		
	MoveDistance = Speed*8,
	Sign = getSign(OpponentTeamID),
	AngleWithinBounds = rePickAngle(RandomAngle, Curr_Y - math:sin(RandomAngle)*MoveDistance),
	if 
		%% front players should advance far (not too far though). back players should advance not too far from half the field. Goalkeeper should stay close
		(Position =:= 1 andalso (DistFromOwnGoal =< 40 orelse HasBall)) orelse (Position > 6 andalso DistFromGoal > MoveDistance*4) orelse
			 (Position > 1 andalso Position =< 6 andalso DistFromGoal > MoveDistance*10) ->
			{Curr_X + Sign*math:cos(AngleWithinBounds)*MoveDistance, Curr_Y - math:sin(AngleWithinBounds)*MoveDistance};
		HasBall ->		
		 	{Curr_X, (GoalBottomY + GoalTopY)/2};
		true ->
		 	{Curr_X, Curr_Y}
	end.

%% rePickAngle(SuspisiousAngle, Y_PositionAfterCalculation)
%% re-pickes a new angle, in case the old one positions the player out of Y bounds
rePickAngle(_, Y) when Y < 0 ->
	-1*random:uniform()*math:pi()/6; %% Picks a random angle between [-pi/6, 0]
rePickAngle(_, Y) when Y > ?FIELD_HEIGHT ->
	random:uniform()*math:pi()/6; %% Picks a random angle between [0, pi/6]	
rePickAngle(RandomAngle, _) -> RandomAngle.

%% getNaturalZone(Position, TeamID) -> {MiddleX, MiddleY, Width, Height};
getNaturalZone(1, 'A') -> {20, ?FIELD_HEIGHT/2,30,100};
getNaturalZone(2, 'A') -> {100,100,200,200};
getNaturalZone(3, 'A') -> {120,?FIELD_HEIGHT/2 - 50,250,100};
getNaturalZone(4, 'A') -> {120,?FIELD_HEIGHT/2 + 50,250,100};
getNaturalZone(5, 'A') -> {100,?FIELD_HEIGHT - 100,200,200};
getNaturalZone(6, 'A') -> {?FIELD_WIDTH/4 - 20,50,300,300};
getNaturalZone(7, 'A') -> {?FIELD_WIDTH/4 - 20,?FIELD_HEIGHT - 50,300,300};
getNaturalZone(8, 'A') -> {?FIELD_WIDTH/4 + 50,?FIELD_HEIGHT/2 - 100,100,100};
getNaturalZone(9, 'A') -> {?FIELD_WIDTH/4 + 50,?FIELD_HEIGHT/2 + 100,100,100};
getNaturalZone(10, 'A') -> {?FIELD_WIDTH/2 - 100, 250,100,100};
getNaturalZone(11, 'A') -> {?FIELD_WIDTH/2 - 50, 350,100,100};

getNaturalZone(1, 'B') -> {?FIELD_WIDTH - 20,?FIELD_HEIGHT/2,30,100};
getNaturalZone(2, 'B') -> {?FIELD_WIDTH - 100,200,200,200};
getNaturalZone(3, 'B') -> {?FIELD_WIDTH - 120,?FIELD_HEIGHT/2 - 50,250,100};
getNaturalZone(4, 'B') -> {?FIELD_WIDTH - 120,?FIELD_HEIGHT/2 + 50,250,100};
getNaturalZone(5, 'B') -> {?FIELD_WIDTH - 100,?FIELD_HEIGHT - 100,200,200};
getNaturalZone(6, 'B') -> {3*?FIELD_WIDTH/4 + 20,50,300,300};
getNaturalZone(7, 'B') -> {3*?FIELD_WIDTH/4 + 20,?FIELD_HEIGHT - 50,300,300};
getNaturalZone(8, 'B') -> {3*?FIELD_WIDTH/4 - 50,?FIELD_HEIGHT/2 - 100,100,100};
getNaturalZone(9, 'B') -> {3*?FIELD_WIDTH/4 - 50,?FIELD_HEIGHT/2 + 100,100,100};
getNaturalZone(10, 'B') -> {?FIELD_WIDTH/2 + 100, 250,100,100};
getNaturalZone(11, 'B') -> {?FIELD_WIDTH/2 + 50, 350,100,100}.