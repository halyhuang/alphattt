-module(web_agent).

-export([start/0, login/3, logout/1, is_login/1, set_room/2, enter_room/1, leave_room/1, start_robot/3, 
		get_info/1, start_observe/1, is_move/1, get_legal_moves/1, set_move/2,  
		get_display_move/1, get_room/1, get_room_state/1, stop/1, get_state/1]).

-define(TIME_OUT, 60 * 10).

-record(state,  {
				 status = waiting_login,
				 username = none,	
				 room = none,
				 web_player = none,
				 player = none,
				 robot_player = none}).

%% APIs.

start() ->
	P = spawn(fun() -> init() end),
	{ok, P}.

login(Pid, UserName, Password) ->
	cast(Pid, {login, UserName, Password}).	

logout(Pid) ->
	cast(Pid, logout).

is_login(Pid) ->
	call(Pid, is_login).	

set_room(Pid, RoomID) ->
	cast(Pid, {set_room, RoomID}).

get_room(Pid) ->
	call(Pid, get_room).

get_room_state(Pid) ->	
	call(Pid, get_room_state).

enter_room(Pid) ->
	cast(Pid, enter_room).

leave_room(Pid) ->
	cast(Pid, leave_room).	

start_robot(Pid, RobotName, RobotType) ->
	cast(Pid, {start_robot, RobotName, RobotType}).

start_observe(Pid) ->
	cast(Pid, start_observe).

set_move(Pid, Move) ->
	cast(Pid, {set_move, Move}).

get_display_move(Pid) ->
	call(Pid, get_display_move).

is_move(Pid) ->
	call(Pid, is_move).

get_legal_moves(Pid) ->
	call(Pid, get_legal_moves).	

get_info(Pid) ->
	call(Pid, get_info).		

get_state(Pid) ->
	call(Pid, get_state).

stop(Pid) ->
	Pid ! stop.

cast(Pid, Msg) ->
	Pid ! Msg.	

call(Pid, Msg) ->
	Ref = make_ref(),
	Pid ! {Msg, Ref, self()},
	receive
		{Ref, Reply} ->
			Reply
	end.
%%
init() ->
	loop(#state{}).

loop(State=#state{status = waiting_login}) ->
	receive
		{login, UserName, Password} ->
			NewState = handle_cast({login, UserName, Password}, State),
			loop(NewState);
		{is_login, Ref, From} ->
			From ! {Ref, false},
			loop(State);
		{get_state, Ref, From} ->
			From ! {Ref, {waiting_login, "", none, none, none, none}},
			loop(State);						
		Unexpected ->
			io:format("unexpected @waiting_login ~p~n", [Unexpected]),
			loop(State)	
	end;

loop(State=#state{status = waiting_enter_room, username = UserName,  
	player = Player, web_player = WebPlayer}) ->
	receive
		{is_login, Ref, From} ->
			From ! {Ref, true},
			loop(State);
		logout ->
			NewState = handle_cast(logout, State),			
			loop(NewState);
		{get_state, Ref, From} ->
			From ! {Ref, {waiting_enter_room, UserName, none, Player, WebPlayer, none}},
			loop(State);			
		{get_room, Ref, From} ->
			From ! {Ref, 0},
			loop(State);			
		{set_room, RoomID} ->
			io:format("~p enter room ~p~n", [UserName, RoomID]),		
			event_store:observe(RoomID, WebPlayer),
			loop(State#state{status = enter_room, room = RoomID});
		stop ->
			exit(stop);
		Unexpected ->
			error_logger:format("unexpected @waiting_enter_room ~p~n", [Unexpected]),
			loop(State)	
	    after ?TIME_OUT * 1000 ->    %% keep state for 60 secs only
	        exit(time_out)				
	end;


loop(State=#state{status = enter_room, username = UserName, room = RoomID, 
		player = Player, robot_player = RobotPlayer, web_player = WebPlayer}) ->
	receive
		{is_login, Ref, From} ->
			From ! {Ref, true},
			loop(State);
		{get_state, Ref, From} ->
			From ! {Ref, {enter_room, UserName, RoomID, Player, WebPlayer, RobotPlayer}},
			loop(State);						
		{get_room_state, Ref, From} ->
			RoomState = room_mgr:get_room_state(RoomID),
			From ! {Ref, RoomState},
			loop(State);	
		logout ->
			NewState = handle_cast(logout, State),			
			loop(NewState);			
		{get_room, Ref, From} ->
			From ! {Ref, RoomID},
			loop(State);
		enter_room ->
 			io:format("~p enter room ~p~n", [UserName, RoomID]),
 			event_store:unsuscribe(WebPlayer),
			player_client:enter_room(Player, RoomID),
			loop(State);									
		leave_room ->
			io:format("~p leave room ~p~n", [UserName, RoomID]),
			web_player:leave_room(WebPlayer),
			end_game(Player, RobotPlayer),
			event_store:unsuscribe(WebPlayer),			
			loop(State#state{status = waiting_enter_room, room = none});
		{start_robot, RobotName, RobotType}	->
			NewRobotPlayer = case RobotPlayer of
								none ->
									{ok, RobotPlayer2} = player_client:start(RobotName, RobotType, board, "127.0.0.1", 8011),	
									erlang:link(RobotPlayer2),	
									player_client:login(RobotPlayer2, ""),
									RobotPlayer2;
								RobotPlayer -> RobotPlayer
							 end,
			player_client:enter_room(NewRobotPlayer, RoomID),
			io:format("~p enter room ~p~n", [RobotName, RoomID]),
			loop(State#state{robot_player = NewRobotPlayer});
		{is_move, Ref, From} ->
			{ok, IsMove} = web_player:is_move(WebPlayer),		
			From ! {Ref, IsMove},
			loop(State);												

		{get_display_move, Ref, From} ->
			{ok, Moves} = web_player:get_display_move(WebPlayer),					    		
			From ! {Ref, Moves},
			loop(State);

		{get_legal_moves, Ref, From} ->
			{ok, PlayerID, LegalMoves} = web_player:get_legal_moves(WebPlayer),	
			From ! {Ref, {PlayerID, LegalMoves}},
			loop(State);

		{get_info, Ref, From} ->
			Infos = case WebPlayer of
						none -> [];
						_ -> web_player:get_info(WebPlayer)
					end,
			From ! {Ref, Infos},
			loop(State);

		{set_move, Move} ->
			web_player:set_move(WebPlayer, Move),
			loop(State);
		stop ->
			exit(stop);
		Unexpected ->
			error_logger:format("unexpected @enter_room ~p~n", [Unexpected]),
			loop(State)	
	    after ?TIME_OUT * 1000 ->    %% keep state for 60 secs only
	        exit(time_out)				
	end.


handle_cast({login, UserName, Password}, State) ->
	{ok, Player} = player_client:start(UserName, web_player, board, "127.0.0.1", 8011),
	erlang:link(Player),
	NewState = case player_client:login(Player, Password) of
		ok ->
    		io:format("user ~p login~n", [UserName]),    		
			WebPlayer = player_client:get_player(Player),
			State#state{status = waiting_enter_room, username = UserName, player = Player, web_player = WebPlayer};
		Reason ->
			io:format("user ~p login failed,reason:~p~n", [UserName, Reason]),			
			State
	end,
	NewState;
handle_cast(logout, #state{status = waiting_enter_room, username = UserName, 
		player = Player, robot_player = RobotPlayer, web_player = WebPlayer}) ->
	event_store:unsuscribe(WebPlayer),		
	erlang:unlink(Player),
	player_client:stop(Player),
	case RobotPlayer of 
		none -> ok;
		RobotPlayer ->
			erlang:unlink(RobotPlayer),
			player_client:stop(RobotPlayer)
	end,
	io:format("user ~p logout~n", [UserName]),	
	#state{status = waiting_login}.

end_game(Player, RobotPlayer) ->
	player_client:leave_room(Player),
	case RobotPlayer of
		none -> 
			ok;
		RobotPlayer ->
			player_client:leave_room(RobotPlayer)
	end.





