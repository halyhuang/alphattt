-module(web_agent).
-export([start/0, login/3, logout/1, is_login/1, enter_room/2, leave_room/1, start_robot/3, set_move/2, 
		get_state/1, show/1, stop/1]).

-record(state,  {username = none,	
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

logout(Pid) ->
	call(Pid, logout).

is_login(Pid) ->
	call(Pid, is_login).	

enter_room(Pid, RoomID) ->
	call(Pid, {enter_room, RoomID}).

leave_room(Pid) ->
	call(Pid, leave_room).	

start_robot(Pid, RobotName, RobotType) ->
	call(Pid, {start_robot, RobotName, RobotType}).

set_move(Pid, Move) ->
	call(Pid, {set_move, Move}).

get_state(Pid) ->
	call(Pid, get_state).

show(Pid) ->
	call(Pid, show).

stop(Pid) ->
	call(Pid, stop).

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

handle_call(show, State=#state{username = UserName, player = Player, 
		web_player = WebPlayer, robot_player = RobotPlayer, room = RoomID}) ->
	{reply, {ok, {UserName, RoomID, Player, WebPlayer, RobotPlayer}}, State};

handle_call(stop, _State) ->
	stop;

handle_call({login, UserName, Password}, State=#state{username = none, player = none}) ->
	{ok, Player} = player_client:start(UserName, web_player, board, "127.0.0.1", 8011),
	erlang:link(Player),
	case player_client:login(Player, Password) of
		ok ->
    		io:format("user ~p login~n", [UserName]),    		
			WebPlayer = player_client:get_player(Player),
			{reply, ok, State#state{username = UserName, player = Player, web_player = WebPlayer}};
		Reason ->
			io:format("user ~p login failed,reason:~p~n", [UserName, Reason]),			
			{reply, ok, State}
	end;

handle_call({login, UserName, Password}, State=#state{username = UserName}) ->
	handle_call(logout, State),
	handle_call({login, UserName, Password}, #state{});

handle_call(logout, #state{username = UserName, player = Player, robot_player = RobotPlayer}) ->
	erlang:unlink(Player),
	player_client:stop(Player),
	case RobotPlayer of 
		none -> ok;
		RobotPlayer ->
			erlang:unlink(RobotPlayer),
			player_client:stop(RobotPlayer)
	end,
	io:format("user ~p logout~n", [UserName]),
	{reply, ok, #state{}};

handle_call(is_login, State=#state{username = none}) ->
	{reply, false, State};

handle_call(is_login, State) ->
	{reply, true, State};	

handle_call({enter_room, RoomID}, State=#state{room = none, player = Player}) ->
    io:format("enter room ~p~n", [RoomID]),
	player_client:enter_room(Player, RoomID),
	{reply, ok, State#state{room = RoomID}};

handle_call(leave_room, State=#state{room = RoomID, player = Player}) ->
    io:format("leave room ~p~n", [RoomID]),
	player_client:leave_room(Player),
	{reply, ok, State#state{room = none}};


handle_call(get_state, State=#state{web_player = none}) ->
	{reply, json2:obj_from_list([{"is_update_move", false}]), State};

handle_call(get_state, State=#state{web_player = WebPlayer}) ->
    {IsUpdateMove, Move, LegalMovesJsonList} = case web_player:is_move(WebPlayer) of
		    	{ok, true} ->

		    		{ok, OpponentMove} = web_player:get_opponent_move(WebPlayer),
		    		MoveJson = case OpponentMove of
		    			none -> "";
		    			{R, C, R1, C1} ->
							io:format("Opponent Move ~p~n", [OpponentMove]),		
		    				json2:obj_from_list([{"R", R}, {"C", C}, {"r", R1}, {"c", C1}])
		    		end,
					{ok, LegalMoves} = web_player:get_legal_move(WebPlayer),
						LegalMovesJson = [ json2:obj_from_list([{"R", R}, {"C", C}, {"r", R1}, {"c", C1}]) || {R, C, R1, C1} <- LegalMoves ],
		    		{true, MoveJson, LegalMovesJson};
		    	_ -> 
		    		{false, [], []}
		   end,
	StateJson = json2:obj_from_list([{"is_update_move", IsUpdateMove},
									 {"move", Move},
									 {"legal_moves", {array, LegalMovesJsonList}}]),
	{reply, StateJson, State};
handle_call({set_move, Move}, State=#state{web_player = WebPlayer}) ->
	web_player:set_move(WebPlayer, Move),	
	{reply, ok, State};

handle_call({start_robot, RobotName, RobotType}, State=#state{robot_player = none, room = RoomID}) ->
	{ok, RobotPlayer} = player_client:start(RobotName, RobotType, board, "127.0.0.1", 8011),	
	erlang:link(RobotPlayer),	
	player_client:login(RobotPlayer, ""),
	player_client:enter_room(RobotPlayer, RoomID),
	{reply, ok, State#state{robot_player = RobotPlayer}};

handle_call({start_robot, _RobotName, _RobotType}, State=#state{robot_player = RobotPlayer, room = RoomID}) ->
	player_client:enter_room(RobotPlayer, RoomID),
	{reply, ok, State}.


