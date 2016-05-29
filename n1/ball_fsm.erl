-module(ball_fsm).
-export([init/1, terminate/3, handle_event/3, freeMotion/2]).
-export([stop/1, start/2, sendEvent/2]).
-define(DELTA_T, 40).
-define(ZERO_TIME, 20).
-define(PASS_SPEED, 5).
-define(DECELERATION, 0.5).
%%
%% Inits the fsm.
%% Position - an integer between [1, 11] represents the player's position
%% TeamID - 'A' | 'B'
%% PlayerID - player's ID
%%
init({BallID, Location, IsFree, {Angle, Speed}}) ->
	if IsFree ->
		utils:trace("crashed here? ~p~n", [?DELTA_T]),
		{ok, freeMotion, {BallID, {Location, Angle, Speed}}, ?DELTA_T};
	true ->
		{ok, hold, {BallID, Location}}
	end;

init({BallID, Location}) ->
	{ok, doNothing, {BallID, Location}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Events
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

freeMotion(timeout, {BallID, {Location, _Angle, Speed}}) when Speed =< 0 ->
	{next_state, freeMotion, {BallID, {Location, 0, 0}}};
freeMotion(timeout, {BallID, {Location, Angle, Speed}}) ->	
	{NextLocation, NextAngle, NextSpeed} = calcNextLocation(Location, Angle, Speed),
	utils:trace("ball goes wild to ~p. angle: ~p, speed: ~p ~n", [NextLocation, NextAngle*180/math:pi(), NextSpeed]),
	gen_server:cast(field_manager, {ballPos, BallID, NextLocation, true, {NextAngle, Speed}}),
	TeamScored = isGoalScored(Location, NextLocation),
	if TeamScored =/= 0 -> %% Goal scored
			gen_server:cast(field_manager, {goalScored, BallID, TeamScored}),		
			{stop, normal, nodata};
		true ->
			{next_state, freeMotion, {BallID, {NextLocation, NextAngle, NextSpeed}}, ?DELTA_T}
	end.

handle_event({ballTaken, PlayerLocation}, StateName, {BallID, _Location}) when StateName =:= hold orelse StateName =:= freeMotion ->
	utils:trace("ball ~p was intercepted ~n", [BallID]),
	{next_state, hold, {BallID, PlayerLocation}};

handle_event({pass, {Angle, Speed}}, hold, {BallID, Location}) ->
	utils:trace("ball ~p passed to direction: ~p degrees ~n", [BallID, Angle*180/math:pi()]),
	{next_state, freeMotion, {BallID,  {Location, Angle, Speed}}, ?ZERO_TIME};

handle_event({kick, {Angle, Speed}}, hold, {BallID, Location}) ->
	utils:trace("ball ~p got kicked to direction: ~p degrees ~n", [BallID, Angle*180/math:pi()]),
	{next_state, freeMotion, {BallID,  {Location, Angle, Speed}}, ?ZERO_TIME};

handle_event({dribble, NewLocation}, hold, {BallID, _Location}) ->
	utils:trace("ball ~p is dribbled to: ~p ~n", [BallID, NewLocation]),
	gen_server:cast(field_manager, {ballPos, BallID, NewLocation, false, {0,0}}),
	{next_state, hold, {BallID, NewLocation}};

handle_event(pause, StateName, {TabID, StateData}) ->
	utils:trace("ball pause event recieved. State: ~p~n", [StateName]),
	{next_state, doNothing, {TabID, {StateName, StateData}}};

handle_event(resume, doNothing, {TabID, {freeMotion, StateData}}) ->
	{next_state, freeMotion, {TabID, StateData}, ?DELTA_T};
handle_event(resume, doNothing, {TabID, {StateName, StateData}}) ->
	utils:trace("ball resume event recieved. State: ~p~n", [StateName]),
	{next_state, StateName, {TabID, StateData}};

handle_event({gameBegin, 'A'}, doNothing, {BallID, Location}) ->
	{next_state, freeMotion, {BallID, {Location, math:pi(), ?PASS_SPEED}}, ?ZERO_TIME};

handle_event({gameBegin, 'B'}, doNothing, {BallID, Location}) ->
	{next_state, freeMotion, {BallID, {Location, 0, ?PASS_SPEED}}, ?ZERO_TIME};

handle_event(stop, StateName, StateData) ->
	utils:trace("ball got stop event recieved. State: ~p~n", [StateName]),
	{stop, normal, StateData};

handle_event(Event, StateName, StateData) ->
	utils:trace("ball got all state event recieved. Event: ~p State: ~p~n", [Event, StateName]),
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% TOOD: if th ball steps out of field bounds, should the game stop? or the ball changes the angle?
calcNextLocation({Current_X, Current_Y}, Angle, Speed) ->	
	NewAngle = calcNextAngle({Current_X, Current_Y}, Angle, utils:getFieldDimentions()),	
	NewSpeed = calNextSpeed(Angle, NewAngle, Speed),
	NewLocation = {Current_X + math:cos(NewAngle)*NewSpeed, Current_Y - math:sin(NewAngle)*NewSpeed},
	utils:trace("ball calc next position: ~p~n when at: ~p ~n", [NewLocation, {Current_X, Current_Y}]),
	{NewLocation, NewAngle, NewSpeed}.

%% if the ball goes out of bounds - calculates the angle so it goes back to the field
%% calcNextAngle(BallLocation, Angle, FieldDimenstions) -> Angle.
calcNextAngle({_, LocationY}, Angle, {_, FieldHeight}) when LocationY >= FieldHeight orelse LocationY =< 0 -> -1*Angle;
calcNextAngle({LocationX, _}, Angle, {FieldWidth, _}) when Angle >= 0 andalso (LocationX >= FieldWidth orelse LocationX =< 0) -> math:pi() - Angle;
calcNextAngle({LocationX, _}, Angle, {FieldWidth, _}) when Angle < 0 andalso (LocationX >= FieldWidth orelse LocationX =< 0) -> -1*math:pi() - Angle;
calcNextAngle(_Location, Angle, _) -> Angle.

% if the ball changed angle (got out of bounds), it should have the same speed as before so it can go back again
calNextSpeed(Angle, Angle, Speed) -> Speed - ?DECELERATION;
calNextSpeed(_, _, Speed) -> Speed.

isGoalScored({_, Current_Y}, {_, Next_Y}) when Next_Y == Current_Y-> 0; %% if next Y is the same as current Y - no goal will be scored.
isGoalScored({_, _}, {Next_X, Next_Y}) ->
	{A_X, {A_Y1, A_Y2}} = utils:getGoalPos('A'),
	{B_X, {B_Y1, B_Y2}} = utils:getGoalPos('B'),
	%M = (Next_X - Current_X) / (Current_Y - Next_Y),
	%N = Current_Y - M*Current_X,
	%{CrossPoint_A_X, CrossPoint_A_Y} = {A_X, M*A_X + N},
	%{CrossPoint_B_X, CrossPoint_B_Y} = {B_X, M*B_X + N},
	if
		Next_X =< A_X andalso Next_Y >= A_Y1 andalso Next_Y =< A_Y2  -> 'B';
		Next_X >= B_X andalso Next_Y >= B_Y1 andalso Next_Y =< B_Y2  -> 'A';	
		true -> 0	
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stop(BallID) ->
	utils:trace("stop fsm for ball ~p~n", [BallID]),
	try
		gen_fsm:send_all_state_event(BallID, stop)
	catch
		exit:MSG -> MSG
	end.


start(BallID, Args) ->
	utils:trace("start fsm for ball ~p~n", [BallID]),
	gen_fsm:start_link({local, BallID}, ball_fsm, Args, []).

sendEvent(BallID, Event) ->
	gen_fsm:send_all_state_event(BallID, Event).

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