-module(web_agent).
-export([start/0, login/3, is_login/1, enter_room/2, start_game/1, start_robot/3, set_move/2, get_state/1, show/1]).

-record(state,  {username = none,
				 password = none,
				 room = none,
				 web_player = none,
				 player = none,
				 robot_player = none}).

%% APIs.

start() ->
	P = spawn(fun() -> init() end),
	{ok, P}.

login(Pid, UserName, Password) ->
	call(Pid, {login, UserName, Password}).	

is_login(Pid) ->
	call(Pid, is_login).	

enter_room(Pid, RoomID) ->
	call(Pid, {enter_room, RoomID}).

start_robot(Pid, RobotName, RobotType) ->
	call(Pid, {start_robot, RobotName, RobotType}).

start_game(Pid) ->
	call(Pid, start_game).

set_move(Pid, Move) ->
	call(Pid, {set_move, Move}).

get_state(Pid) ->
	call(Pid, get_state).

show(Pid) ->
	call(Pid, show).

%%
init() ->
	loop(#state{}).

loop(State) ->
	receive
		{call, Ref, From, Msg} ->
			case handle_call(Msg, State) of
				{reply, Reply, NewState} ->
					From ! {Ref, Reply},
					loop(NewState);
				stop ->
					From ! {Ref, stop},
					stop
			end
	    after 10 * 60 * 1000 ->    %% keep state for 60 secs only
	        exit(time_out)			
	end.


call(Pid, Msg) ->
	Ref = make_ref(),
	Pid ! {call, Ref, self(), Msg},
	receive
		{Ref, Reply} ->
			Reply
	end.

handle_call(show, State) ->
	io:format("web_agent ~p state ~p~n", [self(), State]),
	{reply, ok, State};

handle_call({login, UserName, Password}, State=#state{username = none}) ->
	case game_auth:login(UserName, Password) of
			ok ->				
				{reply, ok, State#state{username = UserName, password = Password}};	
			Reaseon ->
				{reply, Reaseon, State}
	end;	

handle_call({login, UserName, _Password}, State=#state{username = LoginedName}) ->
	Reason = io_lib:format("user ~p already login, try login username ~p", [LoginedName, UserName]),
	io:format("~p~n", [Reason]),
	{reply, Reason, State};

handle_call(is_login, State=#state{username = none}) ->
	{reply, false, State};	

handle_call(is_login, State) ->
	{reply, true, State};	

handle_call({enter_room, RoomID}, State) ->
	{reply, ok, State#state{room = RoomID}};

handle_call(get_state, State=#state{web_player = none}) ->
	{reply, json2:obj_from_list([{"is_update_move", false}]), State};

handle_call(get_state, State=#state{web_player = WebPlayerPid}) ->
    {IsUpdateMove, Move, LegalMovesJsonList} = case webplayer:is_move(WebPlayerPid) of
		    	{ok, true} ->

		    		{ok, OpponentMove} = webplayer:get_opponent_move(WebPlayerPid),
		    		MoveJson = case OpponentMove of
		    			none -> "";
		    			{R, C, R1, C1} ->
							io:format("Opponent Move ~p~n", [OpponentMove]),		
		    				json2:obj_from_list([{"R", R}, {"C", C}, {"r", R1}, {"c", C1}])
		    		end,
					{ok, LegalMoves} = webplayer:get_legal_move(WebPlayerPid),
						LegalMovesJson = [ json2:obj_from_list([{"R", R}, {"C", C}, {"r", R1}, {"c", C1}]) || {R, C, R1, C1} <- LegalMoves ],
		    		{true, MoveJson, LegalMovesJson};
		    	_ -> 
		    		{false, [], []}
		   end,
	StateJson = json2:obj_from_list([{"is_update_move", IsUpdateMove},
									 {"move", Move},
									 {"legal_moves", {array, LegalMovesJsonList}}]),
	{reply, StateJson, State};
handle_call({set_move, Move}, State=#state{web_player = WebPlayerPid}) ->
	webplayer:set_move(WebPlayerPid, Move),	
	{reply, ok, State};

handle_call(start_game, State=#state{username = UserName, password = Password, room = RoomID, 
		player = none}) ->
	{ok, Player} = player_client:start(UserName, webplayer, board, "127.0.0.1", 8011),
	player_client:login(Player, Password),
	WebPlayerPid = player_client:get_player(Player),
	player_client:enter_room(Player, RoomID),
	{reply, ok, State#state{player = Player, web_player = WebPlayerPid}};

handle_call(start_game, State=#state{room = RoomID, player = Player}) ->
	player_client:enter_room(Player, RoomID),
	{reply, ok, State};

handle_call({start_robot, RobotName, RobotType}, State=#state{room = RoomID, robot_player = none}) ->
	{ok, RobotPlayer} = player_client:start(RobotName, RobotType, board, "127.0.0.1", 8011),	
	player_client:login(RobotPlayer, ""),	
	player_client:enter_room(RobotPlayer, RoomID),
	{reply, ok, State#state{robot_player = RobotPlayer}};

handle_call({start_robot, _RobotName, _RobotType}, State=#state{room = RoomID, robot_player = RobotPlayer}) ->
	player_client:enter_room(RobotPlayer, RoomID),
	{reply, ok, State}.


