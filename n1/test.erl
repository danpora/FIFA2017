-module(test).
-export([test/0, fsm_test/0, ball_test/0, game_test/0]).

test() ->

	%ets:insert(gameStat, {p1_A_1, 'A', GameID, 1, 0, ?FIELD_SZ_Y/2, true, false}), 				% inserts the GK
	%ets:insert(gameStat, {p1_B_1, 'B', GameID, 1, 10, ?FIELD_SZ_Y/2 + 10, true, false}), 				% inserts the GK
	%ets:insert(gameStat, {p2_A_1, 'A', GameID, 2, ?FIELD_SZ_X/2, ?FIELD_SZ_Y/2 - ?FIELD_SZ_Y/5, true, false}), 	% inserts the CB
	%ets:insert(gameStat, {p3_A_1, 'A', GameID, 3, ?FIELD_SZ_X/2, ?FIELD_SZ_Y/2 + ?FIELD_SZ_Y/5, true, false}), 	% inserts the CB
	%ets:insert(gameStat, {p4_A_1, 'A', GameID, 4, ?RIGHT_X - 1, ?FIELD_SZ_Y/5, true, false}), 				% inserts the LB
	%ets:insert(gameStat, {p5_A_1, 'A', GameID, 5, 15.5, 40.0, true, false}), % inserts the RB	
	%ets:update_element(gameStat, ball_1, {4, {?FIELD_SZ_X/2, ?FIELD_SZ_Y/2}}),

	field_manager:start_link(),

	L = gen_server:call(field_manager, {getPlayerPositions, 'A', '1', 0, 24}),
	L = [{p5_A_1,15.5,40.0,false},{p4_A_1,24,10.0,false},{p3_A_1,12.5,35.0,false}, {p2_A_1,12.5,15.0,false},{p1_A_1,0,25.0,false}],
	io:fwrite(standard_io, "getPlayerPositions OK~n~n", []),

	L2 = gen_server:call(field_manager, {getFreePlayers, 'A', '1', 0, 49}),
	L2 = [{p5_A_1,15.5,40.0,false},{p2_A_1,12.5,15.0,false},{p1_A_1,0,25.0,false},
			{p9_A_1,25,40,false},{p8_A_1,25,30,false},{p7_A_1,25,20,false},{p11_A_1,45,30,false},{p10_A_1,45,20,false}],
	L3 = gen_server:call(field_manager, {getFreePlayers, 'A', '1', 0, 24}),
	L3 = [{p5_A_1,15.5,40.0,false},{p2_A_1,12.5,15.0,false},{p1_A_1,0,25.0,false}],
	io:fwrite(standard_io, "getFreePlayers node OK~n~n", []),

	gen_server:cast(field_manager, {playerPos, p4_A_1, 20, 10}),
	[{p5_A_1,15.5,40.0,false}, {p4_A_1,20,10,false}, {p3_A_1,12.5,35.0,false}, 
		{p2_A_1,12.5,15.0,false},{p1_A_1,0,25.0,false}] =  gen_server:call(field_manager, {getPlayerPositions, 'A', '1', 0, 24}),
	io:fwrite(standard_io, "playerPos inside node OK~n~n", []),

	gen_server:cast(field_manager, {playerPos, p4_A_1, 33, 10}),
	[{p5_A_1,15.5,40.0,false}, {p3_A_1,12.5,35.0,false}, 
		{p2_A_1,12.5,15.0,false},{p1_A_1,0,25.0,false}] =  gen_server:call(field_manager, {getPlayerPositions, 'A', '1', 0, 24}),
	[{p4_A_1,33,10,false}] =  gen_server:call(field_manager, {getPlayerPositions, 'A', '1', 33, 33}),
	io:fwrite(standard_io, "playerPos to other node OK~n~n", []),

	gen_server:cast(field_manager, {ballPos, ball_1, {20, 30}, false, 0}),
	timer:sleep(100),
	{ball_1, '1', _, {20, 30}, false, 0, _, _} = field_manager:getPlayerById(ball_1),
	io:fwrite(standard_io, "ballPos inside node OK~n~n", []),

	gen_server:cast(field_manager, {ballPos, ball_1, {30, 30}, false, 0}),
	timer:sleep(100),
	{ball_1, '1', _, {30, 30}, false, 0, _, _} = field_manager:getPlayerById(ball_1),
	io:fwrite(standard_io, "ballPos to other node OK~n~n", []),

	gen_server:cast(field_manager, {goalScored, ball_1, 'A'}),
	timer:sleep(100),
	{ball_1,'1', _, _, _, _, 1, 0} = field_manager:getPlayerById(ball_1),
	io:fwrite(standard_io, "players after goalScored ~p~n~n", [ gen_server:call(field_manager, {getPlayerPositions, 'A', '1', 0, 24})]),

	gen_server:cast(field_manager, {goalScored, ball_1, 'B'}),
	timer:sleep(100),
	{ball_1, '1', _, _, _, _, 1, 1} = field_manager:getPlayerById(ball_1),
	io:fwrite(standard_io, "goalScored inside node OK~n~n", []),

	gen_server:cast(field_manager, {ballTaken, ball_1, 'A'}),
	timer:sleep(100),
	{ball_1, '1', _, _, false, {0,0}, _, _} = field_manager:getPlayerById(ball_1),
	io:fwrite(standard_io, "ballTaken inside node OK~n~n", []),

	field_manager:stop().

fsm_test() ->
	field_manager:start_link(),
	timer:sleep(500),

	io:fwrite(standard_io, "field manager started~n~n", []),

	field_manager:sendEventToAllPlayers('1', {gameBegin, 'A'}),
	

	%field_manager:sendEventToAllPlayers('1', noRealEvent),

	timer:sleep(100),

	%%field_manager:sendEventToAllPlayers('1', noRealEvent),

	io:fwrite(standard_io, "game Begin OK~n~n", []),

	timer:sleep(800),

	field_manager:stop().

ball_test() ->
	ball_fsm:start(ball_1, ball_1),
	io:fwrite(standard_io, "ball ~p fsm started ~n~n", [ball_1]),
	ball_fsm:sendEvent(ball_1, {gameBegin, 'A'}),
	timer:sleep(1000),
	ball_fsm:sendEvent(ball_1, {ballTaken, {14, 25}}),
	ball_fsm:sendEvent(ball_1, {dribble, {16, 25}}),
	ball_fsm:sendEvent(ball_1, {dribble, {18, 25}}),
	ball_fsm:sendEvent(ball_1, {dribble, {20, 25}}),
	ball_fsm:sendEvent(ball_1, {pass, math:pi()/4}),
	timer:sleep(1000),
	ball_fsm:stop(ball_1).

game_test() ->
	field_manager:start(),
	timer:sleep(200),
	field_manager:beginGame('1', 'A'),
	timer:sleep(200).
	%field_manager:gfx_trans_link(),

	%timer:sleep(180000),
	%io:fwrite(standard_io, "test finishes ~n~n", []),
	%field_manager:stop().