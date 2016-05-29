-module(fifa_gserver).
-behaviour(wx_object).
-define(FIELD_HEIGHT, 600).
-define(FIELD_WIDTH, 1000).
-define(MARGIN_LENGTH, 60).
-define(PLAYER_HEIGHT, 20).
-define(PLAYER_WIDTH, 20).
-define(GOAL_LENGTH, 100).
-define(REFRESH_RATE, 0).
-define(IP_MAP, nodes).
-define(GFX_CLIENT, n_gfx).

-compile(export_all). %% To be converted

 
-include_lib("wx/include/wx.hrl").

-export([start/0, init/1, terminate/2,  code_change/3,
handle_info/2, handle_event/2, handle_cast/2]).
 
-record(state, 
        {
         parent,
         canvas,
         panel,
         ets_name,
         self
        }).
 
start() ->
    Server = wx:new(),
    io:format("Server: ~p~n",[Server]),
    GFX_REF = wx_object:start(?MODULE, Server, []),
    put(gfx_ref, GFX_REF),
    GFX_REF.
 
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Server) ->
        wx:batch(fun() -> do_init(Server) end).
 
do_init(Server) ->
    ets:new(graphics, [set, named_table]),       
    ets:new(?IP_MAP, [bag,named_table]), 
    ets:insert(graphics, {'1', {0,0}}),  
    ets:insert(graphics, {numPlayers, 11}),

    %% set nodes ip's ETS
	setNodeIP("ip_map.txt"),

    %%register(wx_server,self()),
    Frame = wxFrame:new(Server, -1, "FIFA", [{size,{?FIELD_WIDTH + 2*?MARGIN_LENGTH + 16, ?FIELD_HEIGHT + 40}}]),      %%create frame for the simulator
    Panel = wxPanel:new(Frame,[]),                                       %%create panel from the frame
    wxFrame:show(Frame),   

    Button = wxToggleButton:new(Panel, 11, "", [{style, ?wxNO_BORDER}]),
    wxToggleButton:connect(Button, command_togglebutton_clicked),

    StartButton = wxButton:new(Panel, 21, [{label,"Top Aligned"}, {size, {-1, 50}},{style,?wxBU_TOP}]),
    wxButton:connect(StartButton, command_button_clicked),

    OnPaint = fun(_Evt, _Obj) ->
            %FieldImg = wxImage:new("soccer_field.jpg"),
            %Bmp = wxBitmap:new(FieldImg), 
            %wxImage:destroy(FieldImg),

            Paint = wxBufferedPaintDC:new(Panel),

            Score = ets:lookup_element(graphics, '1', 2),
            %loadFieldBG(Paint),       
            drawSoccerCourt(Paint, Score),       
            wxBufferedPaintDC:destroy(Paint)
      end,
    wxFrame:connect(Panel, paint, [{callback, OnPaint}]),
    wxFrame:connect(Panel, close_window), % works 
    wxPanel:connect(Panel, left_down),
    wxPanel:connect(Panel, right_down),
    wxPanel:connect(Panel, middle_down),

    %spawn_link(fifa_gserver, printRefresh, []),
    register(wx_server,self()),

    {Panel, #state{parent = Panel,canvas = Frame, panel = Panel, ets_name = playersLoc, self = self()}}.
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
%% Async Events are handled in handle_event as in handle_info

handle_event(#wx{event=#wxCommand{type=command_togglebutton_clicked, commandInt = Int}}, Panel = #state{}) ->
    io:format("PAUSING~n",[]),
    [FirstNode|_] = ets:lookup_element(?IP_MAP, n1, 2),
    case Int of
        0 ->
            gen_server:cast({field_manager, FirstNode}, {toggleGamePause, '1', resume});
        1 ->
            gen_server:cast({field_manager, FirstNode}, {toggleGamePause, '1', pause})
    end,    
    {noreply,Panel};

handle_event(#wx{id=Id, event=#wxCommand{type=command_button_clicked}}, Panel = #state{}) ->
	 [FirstNode|_] = ets:lookup_element(?IP_MAP, n1, 2),
     rpc:call(FirstNode, field_manager, beginGame, ['1', 'A']),
     %wxButton:destroy(Id),
    {noreply,Panel};

handle_event(#wx{event = #wxMouse{type = left_down,x = X,y = Y}}, Panel = #state{}) ->
    addPlayer(X, Y, 'A'),
    {noreply, Panel};

handle_event(#wx{event = #wxMouse{type = right_down,x = X,y = Y}}, Panel = #state{}) ->
    addPlayer(X, Y, 'B'),
    {noreply, Panel};

handle_event(#wx{event = #wxMouse{type = middle_down,x = X,y = Y}}, Panel = #state{}) ->
    io:fwrite("killing ~p, ~p~n", [X - ?MARGIN_LENGTH, Y]),    
    [FirstNode|_] = ets:lookup_element(?IP_MAP, n1, 2),
    gen_server:cast({field_manager, FirstNode}, {kill, X, Y}),
    {noreply, Panel};

handle_event(_Event, Panel=#state{}) ->
    %updategraphics(Panel),
    {noreply, Panel}.

handle_info(_Event, Panel=#state{}) ->
    {noreply, Panel}.

handle_cast(selfRateUpdate, Panel=#state{}) ->
    wxWindow:refresh(Panel#state.parent ,[{eraseBackground,false}]),
    %updategraphics(Panel), 
    {noreply, Panel};

handle_cast({updateLoc,{PlayerLocList, BallPos, FieldBounds}}, Panel=#state{}) ->
    updategraphics(PlayerLocList, BallPos, FieldBounds, Panel), 
   % ets:update_element(graphics, playersLocList, {2, PlayersLocList}),
    %ets:update_element(graphics, ballPos, {2, BallPos}),
    {noreply, Panel};

handle_cast({updateScore, GameID, Score}, Panel=#state{}) ->
    ets:insert(graphics, {GameID, Score}),
    ets:insert(graphics, {numPlayers, 11}),
    {noreply, Panel}.

code_change(_, _, State) ->  
    {stop, ignore, State}.
 
terminate(_Reason, _State) ->
    wx:destroy(),
    ok.
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addPlayer(X, Y, TeamID) ->

    %% update number of additional players
    Num = ets:lookup_element(graphics, numPlayers, 2) + 1,
    ets:insert(graphics, {numPlayers, Num}),
    [FirstNode|_] = ets:lookup_element(?IP_MAP, n1, 2),
    gen_server:cast({field_manager, FirstNode}, {newPlayerFromUI, X - ?MARGIN_LENGTH, Y, TeamID, Num}).

format(_Config, Str, Args) ->
    io:format(Str,Args),
    ok.

loadFieldBG(Paint) ->
    FieldImg = wxImage:new("soccer_field.jpg"),
    Bmp = wxBitmap:new(FieldImg), 
    wxImage:destroy(FieldImg),
    wxDC:drawBitmap(Paint, Bmp, {0,0}),
    wxBitmap:destroy(Bmp).

drawSoccerCourt(Paint, {ScoreA, ScoreB}) ->
    Pen = wxPen:new({255,255,255}, [{width, 2}]),
    Brush = wxBrush:new(),
    wxBrush:setColour(Brush, {75,160,55}),
    wxDC:setPen(Paint, Pen), 
    wxDC:setBrush(Paint, Brush),

    %% field + out rectangle
    wxDC:drawRectangle(Paint, {0 , 0 , ?FIELD_WIDTH + 2*?MARGIN_LENGTH, ?FIELD_HEIGHT}),

    %% field
    wxDC:drawRectangle(Paint, {?MARGIN_LENGTH , 0 , ?FIELD_WIDTH, ?FIELD_HEIGHT}),


    %%left goal
    wxDC:drawRectangle(Paint, {round(?MARGIN_LENGTH/2), round(?FIELD_HEIGHT/2 - ?GOAL_LENGTH/2), round(?MARGIN_LENGTH/2), ?GOAL_LENGTH}),

    %% right goal
    wxDC:drawRectangle(Paint, {?MARGIN_LENGTH + ?FIELD_WIDTH, round(?FIELD_HEIGHT/2 - ?GOAL_LENGTH/2), round(?MARGIN_LENGTH/2), ?GOAL_LENGTH}),

    %% left half circle
    wxDC:drawCircle(Paint, {?MARGIN_LENGTH + 160, round(?FIELD_HEIGHT/2)}, 80),

    %% left rahava
    wxDC:drawRectangle(Paint, {?MARGIN_LENGTH, 50, 200, ?FIELD_HEIGHT - 100}),
    wxDC:drawRectangle(Paint, {?MARGIN_LENGTH, round(?FIELD_HEIGHT/2 - 200/2), 70, 200}),

    %% right half circle
    wxDC:drawCircle(Paint, {?MARGIN_LENGTH + ?FIELD_WIDTH - 160, round(?FIELD_HEIGHT/2)}, 80),

    %% right rahava
    wxDC:drawRectangle(Paint, {?MARGIN_LENGTH + ?FIELD_WIDTH - 200, 50, 200, ?FIELD_HEIGHT - 100}),
    wxDC:drawRectangle(Paint, {?MARGIN_LENGTH + ?FIELD_WIDTH - 70, round(?FIELD_HEIGHT/2 - 200/2), 70, 200}),

    %% center of court
    wxDC:drawCircle(Paint, {round(?FIELD_WIDTH/2 + ?MARGIN_LENGTH), round(?FIELD_HEIGHT/2)}, 80),    

    %% middle court
    wxDC:drawLine(Paint, {round(?FIELD_WIDTH/2 + ?MARGIN_LENGTH), 0}, {round(?FIELD_WIDTH/2 + ?MARGIN_LENGTH), ?FIELD_HEIGHT}),

    wxBrush:setColour(Brush, {255,255,255}),
    wxDC:setBrush(Paint, Brush),
    wxDC:drawCircle(Paint, {round(?FIELD_WIDTH/2 + ?MARGIN_LENGTH), round(?FIELD_HEIGHT/2)}, 4),
    wxDC:drawCircle(Paint, {round(?MARGIN_LENGTH + (200 + 70)/2), round(?FIELD_HEIGHT/2)}, 4),
    wxDC:drawCircle(Paint, {round(?MARGIN_LENGTH + ?FIELD_WIDTH - 200 + (200 - 70)/2), round(?FIELD_HEIGHT/2)}, 4),
    
    Font = wxFont:new(80, ?wxFONTFAMILY_MODERN , ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL),
    wxDC:setFont(Paint, Font),
    
    wxDC:setTextForeground(Paint, {130,170,45}),
    wxDC:drawText(Paint, integer_to_list(ScoreA), {?MARGIN_LENGTH + round(?FIELD_WIDTH/2 - 160) - 30, round(?FIELD_HEIGHT/2 - 56)}),
    wxDC:drawText(Paint, integer_to_list(ScoreB), {?MARGIN_LENGTH + round(160 + ?FIELD_WIDTH/2) - 30, round(?FIELD_HEIGHT/2 - 56)}),

    wxFont:destroy(Font),
    wxBrush:destroy(Brush),
    wxPen:destroy(Pen).

paintPlayer(_, _, _, false, _) -> pointOutOfBounds;
paintPlayer('A', 1, PlayerLocation, true, Panel) ->
    paintCircle({150,150,150}, PlayerLocation, Panel);
paintPlayer('B', 1, PlayerLocation, true, Panel) ->
    paintCircle({235,65,146}, PlayerLocation, Panel);
paintPlayer('A', _Position, PlayerLocation, true, Panel) -> 
    paintCircle({255,255,0}, PlayerLocation, Panel);
paintPlayer('B', _Position, PlayerLocation, true, Panel) -> 
    paintCircle({200,10,10}, PlayerLocation, Panel).

paintCircle(BrushColor, {X,Y}, #state{panel = Panel}) ->
    %%Map1 = wxImage:new("pic/red1.png"),
    Paint = wxClientDC:new(Panel),
    Brush = wxBrush:new(),
    wxBrush:setColour(Brush, BrushColor),
    wxDC:setBrush(Paint, Brush),
    wxDC:drawCircle(Paint, {X + ?MARGIN_LENGTH,Y}, 10),
    wxBrush:destroy(Brush),    
    wxClientDC:destroy(Paint).
    %%wxDC:drawBitmap(Paint, Map1, {X,Y}).  

paintBall(_, false, _) -> pointOutOfBounds;
paintBall({X,Y}, true, #state{panel = Panel}) -> 
    %%Map1 = wxImage:new("pic/red1.png"),
    Paint = wxClientDC:new(Panel),
    wxDC:drawCircle(Paint, {X + ?MARGIN_LENGTH,Y}, 5),
    %wxClientDC:destroy(Paint).
    wxClientDC:destroy(Paint).
    %%wxDC:drawBitmap(Paint, Map1, {X,Y}).  

updategraphics(PlayersLocList, BallsList, {LeftX, RightX}, Panel) ->
    %io:fwrite(standard_io, "~p --------- ~p ~n", [PlayersLocList, LeftX]),
    Rect = {LeftX + ?MARGIN_LENGTH, 0, RightX - LeftX, ?FIELD_HEIGHT}, %% Rect = {X, Y, Width, Height}
    wxWindow:refreshRect(Panel#state.parent, Rect ,[{eraseBackground,false}]),
    lists:foreach(fun({TeamID,X,Y, Position}) ->
            IsPointInRange = true,
            IntPoint = {round(X), round(Y)},
            paintPlayer(TeamID, Position, IntPoint, IsPointInRange, Panel)
        end, PlayersLocList),
    lists:foreach(fun({BallX, BallY}) ->
            IsPointInRange = checkAreaValidity(BallX,BallY),
            IntPoint = {round(BallX), round(BallY)},
            paintBall(IntPoint, IsPointInRange, Panel)
        end, BallsList).

checkAreaValidity(X,Y) ->
    X >= 0 andalso X =< ?FIELD_WIDTH andalso Y >= 0 andalso Y =< ?FIELD_HEIGHT.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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