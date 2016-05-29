-module(specific_fm).
-export([initPlayerProcs/0, initGame/1, beginGame/2, posIsOutOfBounds/1, getFieldBounds/0, splitAreaToAreasByNodes/2, getNeighbourNodesRef/0, setNodeIP/1]).
-define(PLAYER_SZ, 2).
-define(FIELD_WIDTH, 1000).
-define(FIELD_HEIGHT, 600).
-define(LEFT_X, 500). %% Defines the left-most X of this field
-define(RIGHT_X, 750). %% Defines the right-most X of this field (could be calculated by LEFT_X + FIELD_WIDTH)
-define(RESOLUTION, 0.000001). %% Defines resultion of the fiedld
-define(IP_MAP, nodes).

%% insert the players to the ets with their initial positions
%% Field1

initGame(GameID) ->
	ets:match_delete(gameStat, {'_', '_', GameID, '_', '_', '_', '_', '_'}),

	%% ETS Player data:{playerID, TeamID, GameID, Position, X, Y, IsOffenseTeam, HasBall}
	ets:insert(gameStat, {p8_B_1, 'B', GameID, 8, 3*?FIELD_WIDTH/4 + 50, ?FIELD_HEIGHT/2 - 100, true, false}), 				% inserts the GK
	ets:insert(gameStat, {p9_B_1, 'B', GameID, 9, 3*?FIELD_WIDTH/4 + 50, ?FIELD_HEIGHT/2 + 100, true, false}), 				% inserts the 
	ets:insert(gameStat, {p10_B_1, 'B', GameID, 10, ?FIELD_WIDTH/2 + 100, ?FIELD_HEIGHT/2 - 50, true, false}), 				% inserts the 
	ets:insert(gameStat, {p11_B_1, 'B', GameID, 11, ?FIELD_WIDTH/2 + 50, ?FIELD_HEIGHT/2 + 50, true, false}), 				% inserts the 
	ets:update_element(gameStat, ball_1, {4, {?FIELD_WIDTH/2, ?FIELD_HEIGHT/2}}),

	%% Inits FSMs
	lists:map( fun({PlayerID, TeamID, Position}) ->
		player_fsm:start(PlayerID, {PlayerID, TeamID, Position})
	end ,[{p8_B_1, 'B', 8}, {p9_B_1, 'B', 9}, {p10_B_1, 'B', 10}, {p11_B_1, 'B', 11}]),
	ball_fsm:start(ball_1, {ball_1, {?FIELD_WIDTH/2, ?FIELD_HEIGHT/2}}).

beginGame(GameID, OffenseTeamID) ->
	ball_fsm:sendEvent(ball_1,  {gameBegin, OffenseTeamID}),
	field_manager:sendEventToAllPlayers(GameID, {gameBegin, OffenseTeamID}).


%%
%% Returns 1 When X is over the left side of the field, 2 - over the right side, 0 - inside bounds
%%
posIsOutOfBounds(X) when X < ?LEFT_X -> left;	
posIsOutOfBounds(X) when X >= ?RIGHT_X -> right;
posIsOutOfBounds(_) -> inbounds.

%%
%% slipts area (Left_X, Right_X) to 3 areas: left area - which belongs to left node, middle - belongs to this node, right - belongs to right node 
%% return value is { {LEFT_L_X, RIGHT_L_X}, {LEFT_M_X, RIGHT_M_X}, {LEFT_R_X, RIGHT_R_X} }. or: {Left_Area, Middle_Area, Right_Area}
%% !: Assums that Right_X > Left_X
%%
splitAreaToAreasByNodes(Left_X, Right_X) ->
	L = posIsOutOfBounds(Left_X),
	R = posIsOutOfBounds(Right_X),
	if
		L == right -> %% all area is in the right side
			{{0,0}, {0,0}, {Left_X, Right_X}};
		R == left -> %% all area is in the left side
			{{Left_X, Right_X}, {0,0}, {0,0}};
		L == left andalso R == inbounds ->  %% area spreads to the left side only
			{{Left_X, ?LEFT_X - ?RESOLUTION}, {?LEFT_X, Right_X}, {0,0}};
		L == inbounds andalso R == right -> %% area spreads to the right side only
			{{0,0}, {Left_X, ?RIGHT_X}, {?RIGHT_X, Right_X}};
		L == inbounds andalso R == inbounds -> %% all area is in bounds
			{{0,0}, {Left_X, Right_X}, {0,0}};
		L == left andalso R == right ->  %% area spreads both to the left and right
			{{Left_X, ?LEFT_X - ?RESOLUTION}, {?LEFT_X, ?RIGHT_X}, {?RIGHT_X, Right_X}};
		true -> %% other option is invalid
			not_ok
	end.
%%
%% returns the field bounds as {Left_X, Right_X}
%%
getFieldBounds() -> {?LEFT_X, ?RIGHT_X}.

getNeighbourNodesRef() -> 
	[RightNeighbour|_] = ets:lookup_element(?IP_MAP, n4, 2),
	[LeftNeighbour|_] = ets:lookup_element(?IP_MAP, n2, 2),
	{{field_manager, LeftNeighbour}, {field_manager, RightNeighbour}}.

%% init the player processes for the field	
initPlayerProcs() -> ok.

%% initiate nodes ETS from file
setNodeIP(FileName) ->
	NodesList = readFile(FileName),
	lists:foreach(fun([Node, Addr]) -> NodeAtom = list_to_atom(Node),
									   AddrAtom = list_to_atom(Addr),
									   ets:insert(nodes, {NodeAtom, AddrAtom}) end, NodesList).

%%%%%%%%%%%%%%%%%%%%%%%
%
% read from file utils
%
%
readFile(FileName) ->	
	Lines = readlines(FileName),
	cleanList(Lines, []).

%%
%% readlines receives a filename and reads all the lines from the file and return 
%% a LIST expression of them
readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    get_all_lines(Device, []).
 
%% the function which make the actual reading from file
get_all_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), Accum;
        Line -> get_all_lines(Device, Accum ++ [Line])
    end.

%%
%% cleanList function receives a String as List and return a clean version of that
%% string: a list which includes only pure words, without blank spaces and new line tokens
cleanList([],NewList) ->
	NewList;
cleanList([H|T],NewList) ->
	NewLine = string:strip(H,right,$\n),
	TokLine = string:tokens(NewLine, ","),
	NewCleanList = NewList ++ [TokLine],	
	cleanList(T,NewCleanList).


