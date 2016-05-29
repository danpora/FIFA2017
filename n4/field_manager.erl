-module(field_manager).
-export([start/0, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).
-export([getOpponentsInDZ/3, getPlayerById/1, distance/2, sendEventToAllPlayers/2, beginGame/2, handleGetAllData/3]).
-export([startMonitor/1, getPlayerPositions/5, sendLocDataToGfx/1]).
-define(FIELD_SZ_X, 1000). %???
-define(FIELD_SZ_Y, 600). %???
-define(DANGER_RADIUS, 40).
-define(INTERCEPTION_RADIUS, 10).
-define(TRACE, false).
-define(IP_MAP, nodes).
-define(GFX_CLIENT, n_gfx).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Monitor process functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

startMonitor(_ServerPID) ->
	%erlang:monitor(ServerPID),
	receive
		{crash, Ets_data} ->
			io:fwrite("SERVER CRASHED...~n", []),
			%%sendAsyncReq({toggleGamePause, '1', pause, both}, both),
			timer:sleep(200),
			MonitorPID = self(),
			{ok, PID} = gen_server:start({local, field_manager}, field_manager, [{reconstruct, MonitorPID, Ets_data}], []),
			timer:sleep(2000),			
			startMonitor(PID);
		_ ->
			ok
	end.

reconstructServer(Ets_data) ->
	ets:new(gameStat, [set, named_table]),
	ets:insert(gameStat, {'1', playing}),

	ets:new(?IP_MAP, [bag,named_table]),
	specific_fm:setNodeIP("ip_map.txt"),
	
	%% reconstruct ball
	[Ball] = [B || B<-Ets_data, tuple_size(B) =:= 9], %% Ball has exactly 9 elements in the ETS
	ets:insert(gameStat, Ball),
	{BallID, _, OffenseTeamID, {X, _}, _, _, _, _, _} = Ball,
	OutOfBound = specific_fm:posIsOutOfBounds(X),
	if OutOfBound =:= inbounds ->			
		gen_server:cast(field_manager, {newBall, BallID});
	true ->
		do_nothing
	end,

	%% reconstruct players
	Players = [P || P<-Ets_data, tuple_size(P) =:= 8], %% Players has exactly 8 elements in the ETS
	lists:foreach(fun(Player) -> 
		%ets:insert(gameStat, Player),
		gen_server:cast(field_manager, {newPlayer, Player, OffenseTeamID})
	end, Players),

	%% resume game
	sendAsyncReq({toggleGamePause, '1', resume, both}, both).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Server methods implementations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start()->
	S = self(),
	MonitorPID = spawn(field_manager, startMonitor, [S]),
	gen_server:start_link({local, field_manager}, field_manager, [{init, MonitorPID}], []).

stop() ->
	%sendAsyncReq({terminate, both}, both),
	gen_server:stop(field_manager).

handle_info(Info, State) ->
	trace("request info: ~p ~n", [Info]),
	{noreply, State}.

handle_call(Request, From, State) ->
	trace("request: ~p recieved from ~p~n", [Request, From]),
	case Request of		
		{getPlayerPositions, TeamID, GameID, Left_X, Right_X} -> 			
			spawn(field_manager, getPlayerPositions, [TeamID, GameID, Left_X, Right_X, From]),
			%{reply, getPlayerPositions(TeamID, GameID, Left_X, Right_X), State};	
			{noreply, State};
		{getFreePlayers, TeamID, GameID, X, Y} -> 
			{reply, getFreePlayers(TeamID, GameID, X, Y), State};
		_ ->
			trace("request (call) not found~n", []),
			{reply, badRequest, State}
	end.

%% TODO: what if a game sent a request while another game is restarting?
handle_cast(Request, {restartingGame, MonitorPID}) ->
	case Request of
			{beginGame, GameID, TeamID} -> %% TODO: start sending events to graphics
				{BallID, GameID, _ ,_ ,_ ,_ ,_, _, _} = getBallByGameID(GameID),
				ets:update_element(gameStat, BallID, {3, TeamID}),
				specific_fm:beginGame(GameID, TeamID),				
				toggleSendDataToGraphics(),
				{noreply, {playing, MonitorPID}};
			{beginGame, GameID, TeamID, SpreadDir} ->  %% TODO: start sending events to graphics
				{BallID, GameID, _ ,_ ,_ ,_ ,_, _, _} = getBallByGameID(GameID),
				ets:update_element(gameStat, BallID, {3, TeamID}),
				sendAsyncReq({beginGame, GameID, TeamID, SpreadDir}, SpreadDir),				
				specific_fm:beginGame(GameID, TeamID),
				toggleSendDataToGraphics(),
				{noreply, {playing, MonitorPID}};
			{toggleGamePause, GameID, resume} -> %% PauseOrResume = pause | resume
				sendEventToAllFSMs(GameID, resume),
				toggleSendDataToGraphics(),
				sendAsyncReq({toggleGamePause, GameID, resume, both}, both),
				{noreply, {playing, MonitorPID}};
			{toggleGamePause, GameID, resume, SpreadDir} ->			
				sendEventToAllFSMs(GameID, resume),
				toggleSendDataToGraphics(),
				sendAsyncReq({toggleGamePause, GameID, resume, SpreadDir}, SpreadDir),
				{noreply, {playing, MonitorPID}};
			{terminate, SpreadDir} ->
				sendAsyncReq({terminate, SpreadDir}, SpreadDir),		
				gen_server:stop(field_manager, shutdown, 1000),
				{noreply, {playing, MonitorPID}};
		_ ->
			trace("----------request: ~p rejected! game is restarting!~n", [Request]),
			{noreply, {restartingGame, MonitorPID}}
	end;

handle_cast(Request, State) ->
	{_, MonitorPID} = State,
	%trace("request: ~p recieved~n", [Request]),
    case Request of
    	{playerPos, PlayerID, X, Y} -> % Player sends its position to the field every delta t
    		Player = ets:lookup(gameStat, PlayerID),
    		handlePlayerPosUpdate(Player, X, Y),
			{noreply, State};
		{ballPos, BallID, BallPos, IsFree, MotionVector} -> % Ball sends its position to the field every delta t
			{CurrentBall_X, _} = ets:lookup_element(gameStat, BallID, 4),			
			handleBallPosUpdate(BallID, BallPos, IsFree, MotionVector, specific_fm:posIsOutOfBounds(CurrentBall_X), both),
			{noreply, State};
		{ballPos, BallID, BallPos, IsFree, MotionVector, SpreadDir} -> % Ball sends its position to the field every delta t. SpreadDir = left | right | both
			%trace("ball pos update direction ~p motion: ~p ~n", [SpreadDir, MotionVector]),					
			handleBallPosUpdate(BallID, BallPos, IsFree, MotionVector, SpreadDir),
			{noreply, State};
		{playerPass, PlayerID, MotionVector, DestPlayerID, SourcePos, Dest_X} -> % Request from player. a player passes the ball to his teammate
			[{PlayerID, _, GameID, _, _, _, _, HasBall}] = ets:lookup(gameStat, PlayerID),
			handlePass(PlayerID, GameID, MotionVector, {DestPlayerID, SourcePos, Dest_X}, HasBall),		
			{noreply, State};
		{playerPass, DestPlayerID, SourcePos, Dest_X} -> % Request from other server. player passes the ball to his teammate (Destination player)
			sendPassEventToDestPlayer(DestPlayerID, SourcePos, Dest_X, specific_fm:posIsOutOfBounds(Dest_X)),
			{noreply, State};
		{playerKick, PlayerID, MotionVector} -> % Request from player. a player kicks the ball
			[{PlayerID, _, GameID, _, _, _, _, HasBall}] = ets:lookup(gameStat, PlayerID),
			handleKick(PlayerID, GameID, MotionVector, HasBall),		
			{noreply, State};
		{newBall, BallID} ->
			[{BallID, _, _, Location, IsFree, MotionVector, _, _, _}] = ets:lookup(gameStat, BallID),
			ball_fsm:start(BallID, {BallID, Location, IsFree, MotionVector}),
			{noreply, State};
		{newPlayer, Player, OffenseTeamID} ->
			trace("new player has arrived ~p~n", [Player]),
			ets:insert(gameStat, Player),
			{PlayerID, TeamID, _, Position, X, Y, _, HasBall} = Player,
			player_fsm:start(PlayerID, {PlayerID, TeamID, Position, {X, Y}, HasBall, OffenseTeamID}),
			{noreply, State};
		{newPlayerFromUI, X, Y, TeamID, Position} ->
			OutOfBound = specific_fm:posIsOutOfBounds(X),
			handleNewPlayerFromUI(X, Y, TeamID, Position, OutOfBound),
			{noreply, State};
		{toggleGamePause, GameID, pause} ->			
			pauseGame(GameID),
			sendAsyncReq({toggleGamePause, GameID, pause, both}, both),
			{noreply, {restartingGame, MonitorPID}};
		{toggleGamePause, GameID, pause, SpreadDir} ->					
			pauseGame(GameID),
			sendAsyncReq({toggleGamePause, GameID, pause, SpreadDir}, SpreadDir),
			{noreply, {restartingGame, MonitorPID}};
		{goalScored, BallID, TeamID} ->
			handleGoalScored(BallID, TeamID, both),
			{noreply, {restartingGame, MonitorPID}};
		{goalScored, BallID, TeamID, SpreadDir} ->		
			handleGoalScored(BallID, TeamID, SpreadDir),
			{noreply, {restartingGame, MonitorPID}};
		{ballTaken, BallID, TeamID} -> %% TODO: this request is useless. The server is the one who initiates ballTaken
			handleBallTaken(BallID, TeamID, both),
			{noreply, State};
		{ballTaken, BallID, TeamID, SpreadDir} ->
			handleBallTaken(BallID, TeamID, SpreadDir),
			{noreply, State};
		{getAllOffensiveData, PlayerID, Left_X, Right_X, Angle1, Angle2} ->
			L = ets:lookup(gameStat, PlayerID),
			spawn(field_manager, handleGetAllData, [offensive, L, {Left_X, Right_X, Angle1, Angle2}]),
			%handleGetAllData(offensive, L, {Left_X, Right_X, Angle1, Angle2}),
			%trace("response ~p~n", [Response]),
			{noreply, State};
		{getAllDefensiveData, PlayerID, NaturalZone} -> %% NaturalZone is a rectangle: {MiddleX, MiddleY, Width, Height}
			L = ets:lookup(gameStat, PlayerID),
			spawn(field_manager, handleGetAllData, [defensive, L, NaturalZone]),
			%handleGetAllData(defensive, L, NaturalZone),			
			%trace("response: ~p~n", [Response]),
			{noreply, State};
		{kill, X, Y} -> %% UI sends a message to kill a server in X, Y
			Direction = specific_fm:posIsOutOfBounds(X),
			if (Direction =:= inbounds) ->
				Kill = 2/0;
			true ->
				sendAsyncReq({kill, X, Y}, Direction)
			end,
			{noreply, State};
		{terminate, SpreadDir} ->
			trace("termination requets received! ~n", []),
			sendAsyncReq({terminate, SpreadDir}, SpreadDir),		
			gen_server:stop(field_manager, shutdown, 1000),
			{noreply, State};
		_ ->
			trace("request not found~n", []),
			{noreply, State}
	end.
	
%% TODO: send stop event to all players in the node	
terminate(normal, _State) ->
	trace("stopping due to other node order~n", []),
	sendEventToAllPlayers('1', stop), %% TODO: sendEventToAllFSMs
	trySendBallEvent(ball_1, stop);
terminate(Reason, {_, MonitorPID}) -> 		
	io:fwrite("stopping for reason ~p~n", [Reason]),		
	sendEventToAllPlayers('1', stop),
	trySendBallEvent(ball_1, stop),
	Ets_data = ets:tab2list(gameStat),
	io:fwrite("sending crash msg to ~p~n", [MonitorPID]),	
	MonitorPID ! {crash, Ets_data}.
	%sendAsyncReq({terminate, both}, both).

%%
%% reconstruct crash
%%
init([{reconstruct, MonitorPID, Ets_data}]) ->
	io:fwrite("RECONSTRUCTING... Data: ~p~n", [Ets_data]),
	reconstructServer(Ets_data),
	toggleSendDataToGraphics(),
	{ok, {playing, MonitorPID}};

%%
%% Create a new game - create player processes, ball process, inits ETS
%%
init([{init, MonitorPID}]) ->
	io:fwrite("Server Starting...~n", []),
	ets:new(gameStat, [set, named_table]),	
	ets:new(?IP_MAP, [bag,named_table]),
	ets:insert(gameStat, {'1', pause}),

	%% set nodes ip's ETS
	specific_fm:setNodeIP("ip_map.txt"),

	%% ETS Ball data:{ballID, GameID, OffenseTeamID, {X, Y}, IsFree, {Angle, Speed}, ScoreA, ScoreB, GameState}
	ets:insert(gameStat, {ball_1, '1', 0, {?FIELD_SZ_X/2, ?FIELD_SZ_Y/2}, false, {0, 0}, 0, 0, pause}),
	specific_fm:initGame('1'),	

	%% start sending events to the ui
	%timer:sleep(200),
	%gen_server:cast(field_manager, {startGFX_trans,both}),

	{ok, {restartingGame, MonitorPID}}.

toggleSendDataToGraphics() ->
	trace("stopping for reason~n~n~n", []),
	{BallID,_,_,_,_,_, ScoreA, ScoreB, _} = getBallByGameID('1'),
	ets:update_element(gameStat, BallID, {9, playing}),
	sendEventToUI({updateScore, '1', {ScoreA, ScoreB}}),
	spawn_link(field_manager, sendLocDataToGfx, [playing]).

%%
%% Begins a game
%%
beginGame(GameID, TeamID) ->
	sendAsyncReq({beginGame, GameID, TeamID, both}, both),
	gen_server:cast(field_manager, {beginGame, GameID, TeamID}).

pauseGame(GameID) ->
	{BallID, _, _, _, _, _, _, _, _} = getBallByGameID(GameID),
	ets:update_element(gameStat, BallID, {9, pause}),
	sendEventToAllFSMs(GameID, pause).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

%% gets distance between two points
distance({X1, Y1}, {X2, Y2}) ->
	math:sqrt(math:pow((X1-X2), 2) + math:pow((Y1 - Y2), 2)).

%% gets the ID of the opponent's team
oppTeam('A') -> 'B';
oppTeam('B') -> 'A'.

getGK_ID(TeamID, GameID) ->
	list_to_atom("p1_" ++ atom_to_list(TeamID) ++ "_" ++ atom_to_list(GameID)).

getBallByGameID(GameID) ->
	BallID = list_to_atom("ball_" ++ atom_to_list(GameID)),
	[Ball] = ets:lookup(gameStat, BallID),
	Ball.

%% Sends an event (all state) to all players
sendEventToAllPlayers(GameID, Event) ->	
	IDs = getAllPlayerIDsInNode(GameID),
	trace("sending message ~p to all players: ~p~n", [Event, IDs]),
	lists:map(fun(ID) ->
		trySendFSMEvent(ID, Event)	
	end, IDs).

%% Send event to all players and ball of game <GameID>
sendEventToAllFSMs(GameID, Event) ->
	sendEventToAllPlayers(GameID, Event),
	{BallID, _, _, _, _, _, _, _, _} = getBallByGameID(GameID),
	trySendBallEvent(BallID, Event).

trySendBallEvent(BallID, Event) ->
	try 
		ball_fsm:sendEvent(BallID, Event)
	catch
		_Throw -> nofsm;
		error:_Error -> nofsm;
		exit:_Exit -> nofsm
	end.	

trySendFSMEvent(FSM_ID, Event) ->
	P = ets:lookup(gameStat, FSM_ID),
	trySendFSMEvent(FSM_ID, Event, P).

trySendFSMEvent(_, _, []) -> nofsm;
trySendFSMEvent(FSM_ID, Event, _) -> 
	try
		player_fsm:sendEvent(FSM_ID, Event)		
	catch
		_Throw -> nofsm;
		error:_Error -> nofsm;
		exit:_Exit -> nofsm
	end.	

trySendReq(0, _) -> server_not_exists;
trySendReq(ServerRef, Request) -> gen_server:cast(ServerRef, Request).

%%sendAsyncReq(Request, SpreadDir)
sendAsyncReq(Request, left) ->
	{LeftRef, _} = specific_fm:getNeighbourNodesRef(),
	trySendReq(LeftRef, Request);
sendAsyncReq(Request, right) ->
	{_, RightRef} = specific_fm:getNeighbourNodesRef(),
	trySendReq(RightRef, Request);
sendAsyncReq(Request, both) when is_tuple(Request)-> %% Request must be tuple and its last element should be SpreadDir
	{LeftRef, RightRef} = specific_fm:getNeighbourNodesRef(),
	LeftRequest = setelement(size(Request), Request, left),
	trySendReq(LeftRef, LeftRequest),
	RightRequest = setelement(size(Request), Request, right),
	trySendReq(RightRef, RightRequest).

trace(String, ArgList) ->
	trace(String, ArgList, ?TRACE).

trace(_, _, false) -> notrace;
trace(String, ArgList, true) ->
	io:fwrite(String, ArgList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ETS Qeuries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getPlayerById(PlayerID) ->
	[Player] = ets:lookup(gameStat, PlayerID),
	Player.

%%
%% gets player positions (as below) and returns the answer to From
%%
getPlayerPositions(TeamID, GameID, Left_X, Right_X, From) ->
	Ans = getPlayerPositions(TeamID, GameID, Left_X, Right_X),
	gen_server:reply(From, Ans).

%%
%% gets all the player positions from team <TeamID>, game <GameID>, and bewteen Left_X and Right_X.
%% if Left_X or Right_X is out of bounds - sends a request to the corresponding fields to get its players
%% retuns a List of tuples: [{PlayerID, X, Y, HasBall},...]
%%
getPlayerPositions(TeamID, GameID, Left_X, Right_X) ->
	{LeftRef, RightRef} = specific_fm:getNeighbourNodesRef(),
	{{Left_L_X, Left_R_X}, {Mid_L_X, Mid_R_X}, {Right_L_X, Right_R_X}} = specific_fm:splitAreaToAreasByNodes(Left_X, Right_X),
	if 
		 {Mid_L_X, Mid_R_X} =/= {0,0} ->		 	
			L1 = ets:select(gameStat, [{{'$1', TeamID, GameID, '_', '$2', '$3', '_', '$4'}, 
					[{'andalso', {'>=', '$2', Left_X}, {'=<', '$2', Right_X}}], 
					[{{'$1', '$2', '$3', '$4'}}]
			}]);
		true ->
			L1 = []
	end,
	if
		{Left_L_X, Left_R_X} =/= {0,0} andalso LeftRef =/= 0 ->
			L2 = gen_server:call(LeftRef, {getPlayerPositions, TeamID, GameID, Left_L_X, Left_R_X});
		true ->
			L2 = []
	end,
	if
		{Right_L_X, Right_R_X} =/= {0,0} andalso RightRef =/= 0 ->
			L3 = gen_server:call(RightRef, {getPlayerPositions, TeamID, GameID, Right_L_X, Right_R_X});
		true ->
			L3 = []
	end,
	lists:flatten([L1, L2, L3]).

%
getPlayerPositions(TeamID, GameID, {Left_X, Right_X, Top_Y, Bottom_Y}) ->	
	ets:select(gameStat, [{{'$1', TeamID, GameID, '_', '$2', '$3', '_', '$4'}, 
			[{'andalso', {'>=', '$2', Left_X}, {'=<', '$2', Right_X}, {'>=', '$3', Top_Y}, {'=<', '$3', Bottom_Y}}],
			[{{'$1', '$2', '$3', '$4'}}]
	}]).

%%
%% gets all the player PIDs from game <GameID>, inside this node only.
%% retuns a List of IDs: [ID1, ID2,...]
%%
getAllPlayerIDsInNode(GameID) ->
	ets:select(gameStat, [{ {'$1', '_', GameID, '_', '_', '_', '_', '_'}, [], ['$1'] }]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Request handlers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

handleGetAllData(_, [], _) -> noresponse;
handleGetAllData(offensive, [Player], {Left_X, Right_X, Angle1, Angle2}) when Left_X > Right_X ->
	handleGetAllData(offensive, [Player], {Right_X, Left_X, Angle1, Angle2});
handleGetAllData(offensive, [Player], {Left_X, Right_X, Angle1, Angle2}) ->
	{PlayerID, TeamID, GameID, _, X, Y, _, _} = Player,
	{_, GameID, _, BallPos, IsFree, MotionVector, _, _, _} = getBallByGameID(GameID),
	FreePlayersIncludingMe = getFreePlayers(TeamID, GameID, Left_X, Right_X),
	FreePlayers = lists:keydelete(PlayerID, 1, FreePlayersIncludingMe),
	PlayersInDZ = getOpponentsInDZ({TeamID, GameID, X, Y}, Angle1, Angle2),
	GKPos = getGKPosition(oppTeam(TeamID), GameID),
	Response = {offensive, {{BallPos, IsFree, MotionVector}, FreePlayers, PlayersInDZ, GKPos}},
	trySendFSMEvent(PlayerID, {dataReceived, Response}),
	%trace("send event to ~p with data  ~p~n", [PlayerID, Response]),
	Response;

%% RequestArgs: rectangle of natural zone
handleGetAllData(defensive, [Player], RequestArgs) ->
	{PlayerID, TeamID, GameID, _, _, _, _, _} = Player,
	{_, GameID, _, BallPos, IsFree, MotionVector, _, _, _} = getBallByGameID(GameID),
	PlayersInNZ = getOpponentsInNZ(TeamID, GameID, RequestArgs),
	Response = {defensive, {{BallPos, IsFree, MotionVector}, PlayersInNZ}},
	trySendFSMEvent(PlayerID, {dataReceived, Response}),
	%trace("send event to ~p with data  ~p~n", [PlayerID, Response]),
	Response.

%%
%% handlePlayerPosUpdate([Player], New_X, New_Y)
%% When a player updates the field about its location, this handler is called.
%% Updates the player position in ETS, if the new pos is out of bounds - sends {newPlayer} request to the curresponding field (server).
%% Returns true if the player remains in bounds, otherwise false - and the player process should be eliminated as a result.
%%
handlePlayerPosUpdate([], _, _) -> noplayer;
handlePlayerPosUpdate([{PlayerID, _, GameID, _, Old_X, OldY, _, HasBall}], X, Y) ->
	%trace("updating position for player ~p in server~n", [PlayerID]),
	ets:update_element(gameStat, PlayerID, [{5, X}, {6, Y}]),
	sendBallDribbleEvent(HasBall, GameID, {X, Y}),
	OutOfBound = specific_fm:posIsOutOfBounds(X),

	F = fun(Direction) -> 
			trace("~p is leaving this node X = ~p~n", [PlayerID, X]),
			player_fsm:stop(PlayerID),
			{_, GameID, OffenseTeamID, _, _, _, _, _, _} = getBallByGameID(GameID),
			[Player] = ets:lookup(gameStat, PlayerID),
			ets:delete(gameStat, PlayerID),		
			sendAsyncReq({newPlayer, Player, OffenseTeamID}, Direction) %% TODO: if send fails player should not have been deleted from ets
	end,

	case OutOfBound of
		inbounds ->	
			handleBallInterception(PlayerID, {X, Y}, {Old_X, OldY}); %% checks if the ball was intercepted by this player, and handles stuff
		Direction ->	%% Exeeds right bounds
			F(Direction)		
	end,
	OutOfBound == inbounds.

%%
%% sendBallDribbleEvent(PlayerHasBall, GameID, PlayerLocation)
%% if the player has the ball, sends a ball of game <GameID> a dribble message with the player's location.
%% otherwise - do nothing.
%%
sendBallDribbleEvent(false, _, _) -> ok;
sendBallDribbleEvent(true, GameID, Location) ->
	{BallID, GameID, _, _, _, _, _, _, _} = getBallByGameID(GameID),
	trySendBallEvent(BallID, {dribble, Location}).

%%
%% gets a PlayerID, and its location, and checks if the ball was intercepted
%% if true - sends a message to the player that he intercepted the ball
%% if true and the player was defense - sends message to all players in the game that the offense team has now changed.
%%
handleBallInterception(PlayerID, NewLocation, OldLocation) ->
	[{PlayerID, TeamID, GameID, _, _, _, _, HasBall}] = ets:lookup(gameStat, PlayerID),
	{BallID, GameID, OffenseTeamID, BallPos, IsFree, _, _, _, _} = getBallByGameID(GameID),	
	IsIntercepted = HasBall =:= false andalso 
		(distance(BallPos, NewLocation) =< ?INTERCEPTION_RADIUS orelse distance(BallPos, OldLocation) =< ?INTERCEPTION_RADIUS) andalso
		(IsFree orelse OffenseTeamID =/= TeamID),
	if IsIntercepted ->
			%% TODO: update the player which the ball was taked from
			L = ets:match(gameStat, {'$1', '_', GameID, '_', '_', '_', '_', true}),			
			lists:map(fun([OppPlayerID]) ->
				ets:update_element(gameStat, OppPlayerID, {8, false}), %% update HasBall in the player
				trySendFSMEvent(OppPlayerID, ballWasTaken)				
			end, L),
			ets:update_element(gameStat, PlayerID, {8, true}), %% update HasBall in the player
			trySendFSMEvent(PlayerID, ballIntercepted),
			trySendBallEvent(BallID, {ballTaken, NewLocation}),
			handleBallTaken(BallID, TeamID, both);
		true ->
			ok
	end.

%%
%% handleBallPosUpdate(BallID, {X, Y}, IsFree, Angle, CurrentPosInbounds, SpreadDir)
%% When the ball updates the field about its location, this handler is called.
%% Updates the ball position in ETS, if the new pos is out of bounds - sends {newBall} request to the curresponding field (server).
%% In any case, sends {ballPos} requests to both neibouring nodes (if such exist)
%% Returns true if the ball remains in bounds, otherwise false - and the ball process should be eliminated as a result.
%%
handleBallPosUpdate(BallID, BallPos, IsFree, MotionVector, inbounds, both) ->
	trace("~p position is updated in the server. pos: ~p (inbounds)~n", [BallID, BallPos]),
	ets:update_element(gameStat, BallID, [{4, BallPos}, {5, IsFree}, {6, MotionVector}]),
	sendAsyncReq({ballPos, BallID, BallPos, IsFree, MotionVector, both}, both),

	%% If the ball is out of bounds, sends {newBall} request to corresponding node	
	{X, _} = BallPos,
	OutOfBound = specific_fm:posIsOutOfBounds(X),	
	case OutOfBound of
		inbounds ->
			true;			
		Direction ->	%% Exeeds bounds
			sendAsyncReq({newBall, BallID}, Direction),
			ball_fsm:stop(BallID)
	end,
	OutOfBound == 0;
handleBallPosUpdate(BallID, _, _, _, _OutOfBound, both) ->  %% ball fsm might be alive but out of bounds
	trace("~p position is updated in the server. OUT OF BOUNDS ~n", [BallID]),
	trySendBallEvent(BallID, stop),
	noball.

handleBallPosUpdate(BallID, BallPos, IsFree, MotionVector, SpreadDir) ->
	ets:update_element(gameStat, BallID, [{4, BallPos}, {5, IsFree}, {6, MotionVector}]),
	sendAsyncReq({ballPos, BallID, BallPos, IsFree, MotionVector, SpreadDir}, SpreadDir).

handleNewPlayerFromUI(X, Y, TeamID, Position, inbounds) ->
	RealPosition = round(random:uniform()*9.4) + 1,
	%% update new player in ets
	PlayerID = list_to_atom("p" ++ integer_to_list(Position) ++ "_" ++ atom_to_list(TeamID) ++ "_1"),
	ets:insert(gameStat, {PlayerID, TeamID, '1', RealPosition, X, Y, true, false}),

	%% start its fsm
	OffenseTeamID = ets:lookup_element(gameStat, ball_1, 3),
	player_fsm:start(PlayerID, {PlayerID, TeamID, RealPosition, {X, Y}, false, OffenseTeamID});

handleNewPlayerFromUI(X, Y, TeamID, Position, Direction) ->
	sendAsyncReq({newPlayerFromUI, X, Y, TeamID, Position}, Direction).
	
% handlePass(PlayerID, GameID, {BallSpeed, BallAngle}, {DestPlayerID, SourcePos, Dest_X}, HasBall) ->
handlePass(_, _, _,_, false) -> ok;
handlePass(PlayerID, GameID, BallMotionVector, {DestPlayerID, SourcePos, Dest_X}, true) ->
	ets:update_element(gameStat, PlayerID, {8, false}),			
	{BallID, GameID, _, _, _, _, _, _, _} = getBallByGameID(GameID),
	ets:update_element(gameStat, BallID, {5, true}),	%% update IsFree
	trySendBallEvent(BallID, {pass, BallMotionVector}),
	sendPassEventToDestPlayer(DestPlayerID, SourcePos, Dest_X, specific_fm:posIsOutOfBounds(Dest_X)).

%% sendPassEventToDestPlayer(DestPlayerID, SourcePosition, DestinationX, OutOfBound)
sendPassEventToDestPlayer(DestPlayerID, SourcePos, _, inbounds) ->
	trySendFSMEvent(DestPlayerID, {gettingPass, SourcePos});
sendPassEventToDestPlayer(DestPlayerID, SourcePos, Dest_X, OutOfBoundDir) ->
	sendAsyncReq({playerPass, DestPlayerID, SourcePos, Dest_X}, OutOfBoundDir).

%% handleKick(PlayerID, GameID, KickAngle, PlayerHasBall) -> ok;
handleKick(_, _, _, false) -> ok;
handleKick(PlayerID, GameID, MotionVector, true) ->	
	ets:update_element(gameStat, PlayerID, {8, false}),		%% update HasBall
	{BallID, GameID, _, _, _, _, _, _, _} = getBallByGameID(GameID),
	ets:update_element(gameStat, BallID, [{5, true}, {6, MotionVector}]),	%% update IsFree
	trySendBallEvent(BallID, {kick, MotionVector}).

%%
%% gets all the free players for a team
%%	
getFreePlayers(TeamID, GameID, Left_X, Right_X) ->
	OurPlayers = getPlayerPositions(TeamID, GameID, Left_X, Right_X),
	OppPlayers = getPlayerPositions(oppTeam(TeamID), GameID, Left_X - ?DANGER_RADIUS, Right_X + ?DANGER_RADIUS),
	FreePlayers = lists:filter(fun({_PlayerID, X1, Y1, _}) -> 
		L = lists:filter(fun({_OppPlayerID, X2, Y2, _}) ->
			distance({X1, Y1}, {X2, Y2}) < ?DANGER_RADIUS
		end, OppPlayers),
		L == []
	end, OurPlayers),
	lists:sort(fun({_,X1,_,_}, {_,X2,_,_}) -> isInFront(X1, X2, TeamID) end, FreePlayers).

isInFront(X1, X2, 'A') -> X1 > X2;
isInFront(X1, X2, 'B') -> X1 < X2.

%%
%% gets all opponent players in danger zone for a player, between two angles, a.k.a Pizza zone. 
%%
%% PlayerData: player date whose asking for the request. PlayerData = {TeamID, GameID, X1, Y1, HasBall}
%% Angle1: first angle in Radians, values between (-pi, pi]
%% Angle2: second angle in Radians, values between (-pi, pi]
%% TODO: Should a radius be a parameter as well?
getOpponentsInDZ(PlayerData, Angle1, Angle2) ->
	{TeamID, GameID, X1, Y1} = PlayerData,
	OppPlayers = getPlayerPositions(oppTeam(TeamID), GameID, X1 - ?DANGER_RADIUS, X1 + ?DANGER_RADIUS),
	lists:filter(fun({_OppId, X2, Y2, _}) ->
			OppAngle = math:atan2(Y1-Y2, X2-X1), % Y1-Y2 because Y axis is opposite - 0 is top and 1, 2, 3.. is lower
			InDZ = distance({X1, Y1}, {X2, Y2}) =< ?DANGER_RADIUS,
			if
				Angle2 - Angle1 > 0 ->
					InDZ andalso OppAngle >= Angle1 andalso OppAngle =< Angle2;
				Angle2 - Angle1 < 0 ->
					InDZ andalso (OppAngle >= Angle1 orelse OppAngle =< Angle2);
				true ->
					InDZ
			end
	end, OppPlayers).

%%
%% getOpponentsInNZ(PlayerID, Rectangle)
%% gets all opponent players in natural zone - defined by a rectangle (X, Y, Height, Width)
%% TeamID - the team ID of the player to which the natural zone belongs
%% returns a list of tuple: {PlayerID, X, Y, HasBall}
%%
getOpponentsInNZ(TeamID, GameID, {MiddleX, MiddleY, Width, Height}) ->		
	getPlayerPositions(oppTeam(TeamID), GameID, {MiddleX - Width/2, MiddleX + Width/2, MiddleY - Height/2, MiddleY + Height/2}).

getGKPosition(TeamID, GameID) ->
	GK_ID = getGK_ID(TeamID, GameID),
	GK = ets:lookup(gameStat, GK_ID),	
	returnGKPOS(GK).

returnGKPOS([]) -> {0, 0};	
returnGKPOS([{_, _, _, _, X, Y, _, _}]) -> {X, Y}.

%%
%% Handler for {goalScored} request.
%% Updates the score in local ETS, and spreads the message to other nodes by the spread direction
%% 
handleGoalScored(BallID, TeamID, SpreadDir) ->	
	Score = updateScoreInEts(BallID, TeamID),
	trace("~n GOALLLLLLL!!!!!! What a wonderful goal for team ~p!!. The score is ~p~n~n", [TeamID, Score]),

	GameID = ets:lookup_element(gameStat, BallID, 2),
	sendEventToAllPlayers(GameID, stop),

	sendAsyncReq({goalScored, BallID, TeamID, SpreadDir}, SpreadDir),

	timer:sleep(100),
	ets:update_element(gameStat, BallID, {9, pause}),
	specific_fm:initGame(GameID),
	timer:sleep(1000),
	gen_server:cast(field_manager, {beginGame, GameID, oppTeam(TeamID)}).	


%%
%% updates the score in ets, and returns the new score
%%
updateScoreInEts(BallID, 'A') ->
	[{BallID, _, _, _, _, _, ScoreA, ScoreB, _}] = ets:lookup(gameStat, BallID),	
	A = ScoreA + 1,
	ets:update_element(gameStat, BallID,{7, A}),
	{A, ScoreB};
updateScoreInEts(BallID, 'B') ->
	[{BallID, _, _, _, _, _, ScoreA, ScoreB, _}] = ets:lookup(gameStat, BallID),	
	B = ScoreB + 1,
	ets:update_element(gameStat, BallID,{8, B}),
	{ScoreA, B}.

%%
%% Handles {ballTaken} request
%% Updates the ball in ETS (isFree=false, MotionVector={0,0}), and spreads the message to other nodes by the spread direction.
%% Also notifies all players in this node which is the offense team
%%
handleBallTaken(BallID, TeamID, SpreadDir) ->
	GameID = ets:lookup_element(gameStat, BallID, 2),
	ets:update_element(gameStat, BallID, [{5, false}, {6, {0,0}}, {3, TeamID}]),  %% update the OffenseTeamID, isFree, Angle of the ball
	sendEventToAllPlayers(GameID, {offenseTeam, TeamID}),
	sendAsyncReq({ballTaken, BallID, TeamID, SpreadDir}, SpreadDir).

%% send players location to graphic server at specific rate
sendLocDataToGfx(pause) -> do_nothing;
sendLocDataToGfx(playing) ->
	AllPlayers = ets:select(gameStat, [{ {'_', '$1', '_', '$4', '$2', '$3', '_', '_'}, [], [{{'$1', '$2', '$3', '$4'}}] }]),

	%% select the ball
	{Left_X, Right_X} = specific_fm:getFieldBounds(),
	BallPosList = ets:select(gameStat, [{ {'_', '_', '_', '$1', '_', '_', '_', '_', '_'}, [], ['$1'] }]),
	FilteredBallPosList = lists:filter(fun({Ball_X, _}) -> Ball_X >= Left_X andalso Ball_X =< Right_X end, BallPosList),
	sendEventToUI({updateLoc,{AllPlayers, FilteredBallPosList, specific_fm:getFieldBounds()}}),

	GameState = ets:lookup_element(gameStat, ball_1, 9),
	timer:sleep(50),
	sendLocDataToGfx(GameState).

sendEventToUI(Event) ->
	GFX_Clients_List = ets:lookup_element(?IP_MAP, ?GFX_CLIENT, 2),
	lists:foreach(fun(X) -> 
			gen_server:cast({wx_server, X}, Event) end, GFX_Clients_List).