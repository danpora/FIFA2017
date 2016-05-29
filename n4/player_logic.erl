-module(player_logic).
-define(DefenseBallRadius, 5).
-compile(export_all).
-define(MOVE_SPEED, 5).
-define(DRIBBLE_SPEED, 4).
-define(KICK_SPEED, 20).
-define(GUARD_DIST, 10).
-define(MAX_PASS_DIST, 210).
-define(MIN_PASS_DIST, 100).
-define(FIELD_HEIGHT, 600).
-define(FIELD_WIDTH, 1000).
-define(AttackFreeBallDist, 150).

%% {{BallPosition, IsBallFree, BallMotion}, OpponentsInNaturalZone} = DefGameData
%% {{BallPosition, IsBallFree, BallMotion}, FreePlayers,PlayersInDangerZone,GKPos} = OffGameData
%% Player ETS data: {PlayerID, TeamID, {Natural_X, Natural_Y, Natural_Width, Natural_Height}, {Current_X, Current_Y}, Angle, HasBall, OffensiveTeam}
%% DistFromBall, FreePlayers, OpponentsInNaturalZone, PlayersInDangerZone, GKPos

%% request data (offensive or defensive) from field managers
requestForData(PlayerTabID) ->
	PlayerID = ets:first(PlayerTabID),
	IsOffensive = ets:lookup_element(PlayerTabID, PlayerID, 7),
	TeamID = ets:lookup_element(PlayerTabID, PlayerID, 2),
	{PizzaAngle1, PizzaAngle2} = utils:getForwardHalfCircle(TeamID),
	if IsOffensive =:= TeamID ->		
		{X, _} = ets:lookup_element(PlayerTabID, PlayerID, 4),	

		%% {getAllOffensiveData,LeftX, RightX ,Angle1,Angle2}
		gen_server:cast(field_manager, {getAllOffensiveData, PlayerID, X-?MAX_PASS_DIST, X+?MAX_PASS_DIST, PizzaAngle1, PizzaAngle2}); 
	true ->
		NaturalZoneTup = ets:lookup_element(PlayerTabID, ets:first(PlayerTabID), 3),
		gen_server:cast(field_manager, {getAllDefensiveData, PlayerID, NaturalZoneTup})
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%% makeDecision/2 returns a state of action, the following one in case of attack

%% offensive case decision
makeDecision(PlayerTabID, {offensive, OffGameData}) ->
	HasBall = ets:lookup_element(PlayerTabID, ets:first(PlayerTabID), 6),
	offensiveTact(HasBall, PlayerTabID, OffGameData);

%% defensive case decision
makeDecision(PlayerTabID, {defensive, DefGameData}) ->
	defensiveTact(PlayerTabID, DefGameData).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% offensiveTact(HasBall, PlayerTabID, OffGameData)
offensiveTact(true, PlayerTabID, OffGameData) ->
	{_, IsAbleToKick, IsPlayersInDangerZone, {BestPassType, PassDest}} = offenceDecisionTup(PlayerTabID, OffGameData),
	if 
		IsAbleToKick -> %% step 1: kick if I can!
			utils:trace("KICKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKK",[]),
			kickCalc(PlayerTabID, OffGameData);
		BestPassType =:= cannotPass -> %% step 2: dribble if I can't pass
			utils:trace("~p:::::: cannot pass",[ets:first(PlayerTabID)]),
			dribbleCalc(danger, PlayerTabID, OffGameData);
	   	IsPlayersInDangerZone -> %% step 3: if I'm under pressure, pass it
	   		utils:trace("~p:::::: players in DZ, passing to: ~p",[ets:first(PlayerTabID), PassDest]),
			{pass, PassDest};
		BestPassType =:= backwardPass -> %% step 4: if there are only free players to the back - dribble
			utils:trace("~p:::::: NO players in DZ, only backward pass. Dribbling",[ets:first(PlayerTabID)]),
			dribbleCalc(noDanger, PlayerTabID, OffGameData);
		true ->	%% step 5: if there's a free player ahead:		
			{_, X, Y} = PassDest,
			MyPos = ets:lookup_element(PlayerTabID, ets:first(PlayerTabID), 4),			
			FarEnough = utils:distPoints(MyPos, {X, Y}) >= ?MIN_PASS_DIST,
			if FarEnough -> %% step 5.1: if there's a free player ahead and he's far enough from me - pass him
				utils:trace("~p:::::: NO players in DZ, forward passing to: ~p",[ets:first(PlayerTabID), PassDest]),
				{pass, PassDest};
			true -> %% step 5.2: if there's a free player ahead and he's too close - dribble
				utils:trace("~p:::::: NO players in DZ, NO forward pass. Dribbling",[ets:first(PlayerTabID)]),
				dribbleCalc(noDanger, PlayerTabID, OffGameData)
			end
	end;

%% case offence, doesnt have the ball
offensiveTact(false, PlayerTabID, OffGameData) ->
	{{BallLocation, IsBallFree, BallMotion}, _FreePlayers, _PlayersInDangerZone, _GKPos} = OffGameData,
	[{_, TeamID, _, CurrentLocation, Position, _, _}] = ets:lookup(PlayerTabID, ets:first(PlayerTabID)),
	ShouldAttackBall = shouldAttackBall(BallLocation, IsBallFree, CurrentLocation, false),
	if ShouldAttackBall ->
			attackBall(CurrentLocation, TeamID, {BallLocation, BallMotion}, Position);
		true ->
			LocationXY = utils:getRandomPointToGo(utils:oppTeam(TeamID), CurrentLocation, ?MOVE_SPEED, false, Position),
			{move, LocationXY}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% case defence, opponents in natural zone (true)
defensiveTact(PlayerTabID, DefGameData) ->
		{{IsOppInNZ, IsOppWithBall, OppLocation}, IsPlayerInNZ, ShouldAttackBall, {BallLocation, BallMotion}} = getDefenseTup(PlayerTabID, DefGameData),
		[{_, TeamID, _, CurrentLocation, Position, _, _}] = ets:lookup(PlayerTabID, ets:first(PlayerTabID)),
		if 	
			ShouldAttackBall ->	%% If I am close to the ball		
				attackBall(CurrentLocation, TeamID, {BallLocation, BallMotion}, Position); %% go take it
		   	IsPlayerInNZ =:= false -> %% else, If I am not in my Natural Zone
		   		goToDefaultLocation(PlayerTabID); %% go back to my Natural Zone	   		
		   	IsOppInNZ andalso IsOppWithBall -> %% else, If opponent with ball is in my NZ
				attackBall(CurrentLocation, TeamID, {OppLocation, noMotion}, Position); %% take the ball
			IsOppInNZ -> %% else, If opponent without the ball in my NZ
				guardOpponent(OppLocation); %% stay close to him
			true ->
				{move, CurrentLocation} %% do nothing
		end.

getDefenseTup(PlayerTabID, DefGameData) ->
		PlayerPos = ets:lookup_element(PlayerTabID, ets:first(PlayerTabID), 4),
		PlayerNaturalPos = ets:lookup_element(PlayerTabID, ets:first(PlayerTabID), 3),
		IsPlayerInNaturalZone = utils:isPointInReactanle(PlayerPos, PlayerNaturalPos),
		{{BallPosition, IsBallFree, BallMotion}, OpponentsInNaturalZone} = DefGameData,
		{IsOpponentInNaturalZone, IsOpponentWithBall, OpponentPosition}  = utils:oppInNatZoneInfo(OpponentsInNaturalZone),
		ShouldAttackBall = shouldAttackBall(BallPosition, IsBallFree, PlayerPos, true),
		{{IsOpponentInNaturalZone, IsOpponentWithBall, OpponentPosition}, IsPlayerInNaturalZone, ShouldAttackBall, {BallPosition, BallMotion}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Offensive functions 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

offenceDecisionTup(PlayerTabID, OffGameData) ->
	IsAbleToKick = isAbleToKick(PlayerTabID),
	{{BallPosition, IsBallFree, BallMotion},FreePlayers,PlayersInDangerZone, _GKPos} = OffGameData,
	IsPlayersInDangerZone = utils:isPlayersInDangerZone(PlayersInDangerZone),
	BestPassOption = utils:bestPassOption(PlayerTabID, FreePlayers),
	{{BallPosition, IsBallFree, BallMotion}, IsAbleToKick, IsPlayersInDangerZone, BestPassOption}.

%% calculates if a player is at a shoot rangeisInShootArea
isAbleToKick(PlayerTabID) -> 
	PlayerPos = ets:lookup_element(PlayerTabID, ets:first(PlayerTabID), 4),
	TeamID = ets:lookup_element(PlayerTabID, ets:first(PlayerTabID), 2),	
	utils:isInShootArea(PlayerPos, utils:oppTeam(TeamID)).

kickCalc(PlayerTabID, {{_,_,_},_,_,GKPos}) ->
	TeamID = ets:lookup_element(PlayerTabID, ets:first(PlayerTabID), 2),	
	KickPlayerPos = ets:lookup_element(PlayerTabID, ets:first(PlayerTabID), 4),
    KickToPos = utils:getKickInfo(GKPos, KickPlayerPos, utils:oppTeam(TeamID)),
    %utils:trace("Kicking~n",[]),
	{kick, KickToPos}.

%% decide which direction to move with the ball
dribbleCalc(noDanger, PlayerTabID, _OffGameData) ->
	[{_, TeamID, _, CurrentLocation, Position, _, _}] = ets:lookup(PlayerTabID, ets:first(PlayerTabID)),
	{NextX, NextY} = utils:getRandomPointToGo(utils:oppTeam(TeamID), CurrentLocation, ?DRIBBLE_SPEED, true, Position),
	{dribble, {NextX,NextY}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO: change this func to be danger sensitive
dribbleCalc(danger, PlayerTabID, _OffGameData) ->
	[{_, TeamID, _, CurrentLocation, Position, _, _}] = ets:lookup(PlayerTabID, ets:first(PlayerTabID)),
	{NextX, NextY} = utils:getRandomPointToGo(utils:oppTeam(TeamID), CurrentLocation, ?DRIBBLE_SPEED, true, Position),
	{dribble, {NextX,NextY}}.

guardOpponent({OppX, OppY}) ->
	%utils:trace("Guarding Opponent in Natural Zone~n", []),	
	GaurdPoint = {(random:uniform()-0.5)*?GUARD_DIST + OppX, (random:uniform()-0.5)*?GUARD_DIST + OppY}, %% go to a random point near the opponent
	{move, GaurdPoint}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Defensive functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% attackBall(CurrentLocation, TeamID {BallLocation, BallMotion}, Position) - going to get the ball. If the player is goal keeper, it should be a bit different
attackBall(CurrentLoc, _TeamID, BallVector, 1) ->
	JumpToPoint = calcGKJumpToPoint(BallVector, CurrentLoc),
	% {_, {TopPost, BottomPost}} = utils:getGoalPos(TeamID),	
	% Max = max(TopPost, BallY),
	% Min = min(BottomPost, BallY),	
	{move, JumpToPoint};
attackBall(CurrentPos, _TeamID, {BallLocation, _BallMotion}, _Position) ->
	NextPosition = utils:calcNextPosition(CurrentPos, BallLocation, 5*?MOVE_SPEED),
	{move, NextPosition}.

%% calcGKJumpToPoint({BallLoction, BallMotion}, CurrentGKLocation)
calcGKJumpToPoint({{_BallX, BallY}, {0, _Speed}}, {CurrX, _CurrY}) -> {CurrX, BallY};
calcGKJumpToPoint({{BallX, BallY}, {Angle, _Speed}}, {CurrX, CurrY}) ->
	M = -1*math:tan(Angle),
	N = BallY - M*BallX,
	DestX = (CurrY + M*CurrX - N)/(2*M),
	DestY = (CurrY + M*CurrX + N)/2,
	Min = min(DestX, ?FIELD_WIDTH),
	Max = max(DestX, 0),
	{determinMin(Min, Max, DestX), DestY}.

determinMin(Val, Val, Val) -> Val;
determinMin(Min, Val, Val) -> Min;
determinMin(Val, Max, Val) -> Max.

goToDefaultLocation(PlayerTabID) ->
	{NatX, NatY, _, _} = ets:lookup_element(PlayerTabID, ets:first(PlayerTabID), 3),
	%utils:trace("Moving to default location ~n"),
	{move, {NatX,NatY}}.
 
shouldAttackBall(BallPosition, IsBallFree, PlayerPos, _IsDenfense) ->
	%io:fwrite("calc shouldAttackBall: ballPos: ~p, PlayerPos: ~p~n", [BallPosition, PlayerPos]),	
	DistFromBall = utils:distPoints(BallPosition, PlayerPos),

	(DistFromBall =< ?AttackFreeBallDist andalso IsBallFree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


runToPoint(PlayerTabID, DestinationXY, false) ->
	CurrentPos = ets:lookup_element(PlayerTabID, ets:first(PlayerTabID), 4),
	NextPosition = utils:calcNextPosition(CurrentPos, DestinationXY, ?MOVE_SPEED),
	ets:update_element(PlayerTabID, ets:first(PlayerTabID), {4, NextPosition}),
	{X, Y} = NextPosition,
	gen_server:cast(field_manager, {playerPos, ets:first(PlayerTabID), X, Y}),
	NextPosition;

runToPoint(PlayerTabID, DestinationXY, true) ->
	CurrentPos = ets:lookup_element(PlayerTabID, ets:first(PlayerTabID), 4),
	NextPosition = utils:calcNextPosition(CurrentPos, DestinationXY, ?DRIBBLE_SPEED),
	ets:update_element(PlayerTabID, ets:first(PlayerTabID), {4, NextPosition}),
	{X, Y} = NextPosition,
	%ball_fsm:sendEvent(ball_1, {dribble, NextPosition}),
	gen_server:cast(field_manager, {playerPos, ets:first(PlayerTabID), X, Y}),
	NextPosition.

pass(PlayerTabID, {DestPlayerID, Dest_X, Dest_Y}) ->
	PlayerID = ets:first(PlayerTabID),
	PlayerPos = ets:lookup_element(PlayerTabID, PlayerID, 4),
	Angle = utils:angle(PlayerPos, {Dest_X, Dest_Y}),
	Speed = utils:calcPassSpeed(PlayerPos, {Dest_X, Dest_Y}),
	utils:trace("~p Pass: calc pass ~p. from ~p to ~p~n",[PlayerID, Speed, PlayerPos, {Dest_X, Dest_Y}]),
	ets:update_element(PlayerTabID, PlayerID, {6, false}),
	gen_server:cast(field_manager, {playerPass, PlayerID, {Angle, Speed}, DestPlayerID, PlayerPos, Dest_X}),
	ok.

kick(PlayerTabID, DestinationXY) ->
	PlayerID = ets:first(PlayerTabID),
	PlayerPos = ets:lookup_element(PlayerTabID, PlayerID, 4),
	Angle = utils:angle(PlayerPos, DestinationXY),
	ets:update_element(PlayerTabID, PlayerID, {6, false}),
	gen_server:cast(field_manager, {playerKick, PlayerID, {Angle, ?KICK_SPEED}}),
	ok.

updateOffensiveTeam(PlayerTabID, OffensiveTeam) ->
	PlayerTeamID = ets:lookup_element(PlayerTabID, ets:first(PlayerTabID), 2),
	if PlayerTeamID == OffensiveTeam ->
		ets:update_element(PlayerTabID, ets:first(PlayerTabID), {7, true});
	true ->
		ets:update_element(PlayerTabID, ets:first(PlayerTabID), {7, false})
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%