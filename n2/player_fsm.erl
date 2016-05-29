-module(player_fsm).
-export([init/1, terminate/3, handle_event/3, kick/2, pass/2, dribble/2, move/2, hold/2, waitForData/2]).
-export([stop/1, start/2, sendEvent/2]).
-define(DELTA_T, 40).
-define(RESEND_REQ_TIMOUT, 1000).
-define(ZERO_TIME, 10).

%%
%% Inits the fsm.
%% Position - an integer between [1, 11] represents the player's position
%% TeamID - 'A' | 'B'
%% PlayerID - player's ID
%%

%% Player ETS data: {PlayerID, TeamID, {Natural_X, Natural_Y, Width, Height}, {Current_X, Current_Y}, Position, HasBall, OffenseTeam}
init({PlayerID, TeamID, Position}) ->
	TabID = ets:new(playerETS, [set, private]),
	{X, Y, W, H} = utils:getNaturalZone(Position, TeamID),
	ets:insert(TabID, {PlayerID, TeamID, {X, Y, W, H}, {X, Y}, Position, false, '_'}),
	{ok, doNothing, {TabID, nodata}};

init({PlayerID, TeamID, Position, PlayerLocation, HasBall, OffenseTeamID}) ->
	TabID = ets:new(playerETS, [set, private]),
	NatZone = utils:getNaturalZone(Position, TeamID),	
	ets:insert(TabID, {PlayerID, TeamID, NatZone, PlayerLocation, Position, HasBall, OffenseTeamID}),
	player_logic:requestForData(TabID),
	{ok, waitForData, {TabID, nodata}, ?RESEND_REQ_TIMOUT}.

gotoWaitForDecision(TabID) ->
	player_logic:requestForData(TabID),
	%utils:trace("player with ~p requested data~n", [ets:first(TabID)]),
	{next_state, waitForData, {TabID, nodata}, ?RESEND_REQ_TIMOUT}.

handleMoveEvent(StateName, {TabID, DestPoint}) ->	
	ArrivedToPoint = player_logic:runToPoint(TabID, DestPoint, StateName =:= dribble),
	utils:trace("~p ~ping to: ~p~n", [ets:first(TabID), StateName, ArrivedToPoint]),
	case ArrivedToPoint of	
		DestPoint ->
			timer:sleep(?DELTA_T),
			gotoWaitForDecision(TabID);
		_OtherPoint ->		
			{next_state, StateName, {TabID, DestPoint}, ?DELTA_T}
	end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Events
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kick(timeout, {TabID, DestPoint}) ->
	utils:trace("~p: kicks the ball to: ~p~n", [ets:first(TabID), DestPoint]),
	player_logic:kick(TabID, DestPoint),
	timer:sleep(?DELTA_T*5),
	gotoWaitForDecision(TabID).

pass(timeout, {TabID, Destination}) -> %% zero time state. Destination = {PlayerID, Location}
	utils:trace("~p: passes the ball to: ~p~n", [ets:first(TabID), Destination]),
	player_logic:pass(TabID, Destination),
	timer:sleep(?DELTA_T*5),
	gotoWaitForDecision(TabID).

dribble(timeout, StateData) ->
	handleMoveEvent(dribble, StateData).

move(timeout, StateData) ->
	handleMoveEvent(move, StateData).

hold(timeout, {TabID, TimeWaited}) ->
	utils:trace("~p: finished holding after ~p delat_t~n", [ets:first(TabID), TimeWaited]),
	gotoWaitForDecision(TabID).

waitForData(timeout, {TabID, _StateData}) ->
	gotoWaitForDecision(TabID).

handle_event({dataReceived, {DataType, Data}}, waitForData, {TabID, _NoData}) ->
	[{_, TeamID, _, _, _, _, OffenseTeam}] = ets:lookup(TabID, ets:first(TabID)),
	if 
		(TeamID =:= OffenseTeam andalso DataType =:= offensive) orelse (TeamID =/= OffenseTeam andalso DataType =:= defensive) -> %% make sure that data is relevant
			{NewState, NewStateData} = player_logic:makeDecision(TabID, {DataType, Data}),	
			utils:trace("~p: data recieved. Next state is: ~p, data: ~p~n", [ets:first(TabID), NewState, NewStateData]),
			{next_state, NewState, {TabID, NewStateData}, ?ZERO_TIME};
		true -> %% if data is not relevenat, ask for it again
			gotoWaitForDecision(TabID)
	end;

handle_event(ballWasTaken, _StateName, {TabID, _NoData}) ->
	utils:trace("******the ball was taken from player ~p******~n", [ets:first(TabID)]),
	TeamID = ets:lookup_element(TabID, ets:first(TabID), 2),
	OppTeamID = utils:oppTeam(TeamID),
	ets:update_element(TabID, ets:first(TabID), [{6, false}, {7, OppTeamID}]), %% TODO: check for the bug
	{next_state, hold, {TabID, 10}, 10*?DELTA_T};

handle_event(ballIntercepted, _StateName, {TabID, _NoData}) -> %% TODO: check for the bug
	PlayerID = ets:first(TabID),
	utils:trace("******player ~p intercepted the ball******~n", [PlayerID]),
	TeamID = ets:lookup_element(TabID, PlayerID, 2),
	ets:update_element(TabID, PlayerID, [{6, true}, {7, TeamID}]), 
	gotoWaitForDecision(TabID);

handle_event({gettingPass, SourcePostion}, _StateName, {TabID, _NoData}) ->
	PlayerPos = ets:lookup_element(TabID, ets:first(TabID), 4),
	TimeWait = utils:calcTimeWaitForPass(PlayerPos, SourcePostion),
	{next_state, hold, {TabID, TimeWait}, TimeWait*?DELTA_T};

handle_event(goalScored, _StateName, {TabID, _StateData}) ->
	% player_logic:initPlayer(TabID),
	{next_state, doNothing, {TabID, nodata}};

handle_event({offenseTeam, TeamID}, StateName, {TabID, _StateData}) when StateName =/= hold-> %% TODO: check bug
	utils:trace("~p: aware that offense team is ~p~n", [ets:first(TabID), TeamID]),
	ets:update_element(TabID, ets:first(TabID), {7, TeamID}),
	gotoWaitForDecision(TabID);

handle_event(stop, StateName, {TabID, StateData}) ->

	utils:trace("~p: stop event recieved. State: ~p~n", [ets:first(TabID), StateName]),
	{stop, normal, {TabID, StateData}};

handle_event(pause, StateName, {TabID, StateData}) ->
	utils:trace("~p: pause event recieved. State: ~p~n", [ets:first(TabID), StateName]),
	{next_state, doNothing, {TabID, {StateName, StateData}}};

handle_event(resume, doNothing, {TabID, {waitForData, _StateData}}) ->
	utils:trace("~p: resume event recieved. State: ~p~n", [ets:first(TabID), waitForData]),
	gotoWaitForDecision(TabID);
handle_event(resume, doNothing, {TabID, {StateName, StateData}}) ->
	utils:trace("resume event recieved. State: ~p~n", [StateName]),
	{next_state, StateName, {TabID, StateData}, ?DELTA_T};

handle_event({gameBegin, OffenseTeamID}, doNothing, {TabID, _StateData}) ->
	ets:update_element(TabID, ets:first(TabID), {7, OffenseTeamID}),
	gotoWaitForDecision(TabID);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Events that should be rejected (do nothing)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_event(_Event, hold, {TabID, TimeWait}) ->
	%utils:trace("All state event recieved. Event: ~p State: ~p~n", [Event, hold]),	
	{next_state, hold, {TabID, TimeWait}, TimeWait*?DELTA_T};

handle_event(_Event, doNothing, StateData) ->
	%utils:trace("All state event recieved. Event: ~p State: ~p~n", [Event, hold]),	
	{next_state, doNothing, StateData};

handle_event(_Event, StateName, StateData) ->
	%utils:trace("All state event recieved. Event: ~p State: ~p~n", [Event, StateName]),	
	{next_state, StateName, StateData, ?DELTA_T}.

terminate(_Reason, _StateName, {TabID, _StateData}) ->
	ets:delete(TabID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stop(PlayerID) ->
	try
		gen_fsm:stop(PlayerID)
	catch
		exit:MSG -> MSG
	end.

start(PlayerID, Args) ->
	utils:trace("start fsm for player ~p~n", [PlayerID]),
	gen_fsm:start_link({local, PlayerID}, player_fsm, Args, []).

sendEvent(PlayerID, Event) ->
	gen_fsm:send_all_state_event(PlayerID, Event).

%% player_logic:getNaturalPosition
%% player_logic:makeDecision
%% player_logic:requestForData
%% player_logic:runToPoint
%% player_logic:dribble
%% player_logic:pass
%% player_logic:kick
%% player_logic:initPlayer
%% player_logic:updateOffenseTeam
%% player_logic:axisSpin - player spins on its central axis. if HasBall, the ball should change its position also