-module(web_agent).

-export([start/0, login/3, logout/1, is_login/1, is_guest/1, set_room/2, enter_room/1, leave_room/1, start_robot/2, 
		get_info/1, start_observe/1, is_move/1, get_legal_moves/1, set_move/2,  
		get_display_move/1, get_room/1, get_room_state/1, stop/1, get_state/1, get_all_robots/1, 
		chat/2, get_msg/1, get_user_name/1]).

-define(TIME_OUT, 600 * 1000).

-record(state,  {
				 status = waiting_login,
				 username = none,	
				 room = none,
				 web_player = none,
				 player = none,
				 robot_player = none,
				 robot = [],
				 is_guest = false}).

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

is_guest(Pid) ->
	call(Pid, is_guest).	


get_user_name(Pid) ->
	call(Pid, get_user_name).

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

start_robot(Pid, RobotName) ->
	cast(Pid, {start_robot, RobotName}).

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

get_msg(Pid) ->
	call(Pid, get_msg).			

get_state(Pid) ->
	call(Pid, get_state).

chat(Pid, Msg) ->
	Pid ! {chat, Msg}.	

get_all_robots(Pid) ->
	call(Pid, get_all_robots).	

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
	after 5 * 1000 ->
		io:format("web_agent call timeout")
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
	    after ?TIME_OUT ->    %% keep state for 60 secs only
	        exit(time_out)			
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
		{get_user_name, Ref, From} ->
			From ! {Ref, UserName},
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
	    after ?TIME_OUT ->    %% keep state for 60 secs only
	        exit(time_out)				
	end;


loop(State=#state{status = enter_room, username = UserName, room = RoomID, 
		player = Player, robot_player = RobotPlayer, web_player = WebPlayer, robot = Robots, is_guest = IsGuest}) ->
	receive
		{is_login, Ref, From} ->
			From ! {Ref, true},
			loop(State);
		{is_guest, Ref, From} ->
			From ! {Ref, IsGuest},
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
		{get_all_robots, Ref, From} ->
			RobotList = [{"mcts", ""}, {"mcts_pall", ""}, {"mcts_ucb1", ""}, {"mcts_ucb1_pall", ""}],	
			RobotList2 = [RobotName || {RobotName, _RobotPassword} <- RobotList],	
			From ! {Ref, RobotList2},
			loop(State#state{robot = RobotList});			
		{start_robot, RobotName}	->
			case lists:keyfind(RobotName, 1, Robots) of
				{RobotName, RobotPassword} ->
					case RobotPlayer of
						none ->
							ok;
						RobotPlayer -> 
							player_client:stop(RobotPlayer),
							erlang:unlink(RobotPlayer)
					end,				
					{ok, NewRobotPlayer} = player_client:start(RobotName, list_to_atom(RobotName), board, "127.0.0.1", 8011),	
					erlang:link(NewRobotPlayer),	
					player_client:login(NewRobotPlayer, RobotPassword),
					player_client:enter_room(NewRobotPlayer, RoomID),
					io:format("~p enter room ~p~n", [RobotName, RoomID]),
					loop(State#state{robot_player = NewRobotPlayer});
				_ ->
					io:format("robot ~p not exist~n", [RobotName]),
					loop(State)
			end;

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
		{get_msg, Ref, From} ->
			Msgs = case WebPlayer of
						none -> [];
						_ -> web_player:get_msg(WebPlayer)
					end,
			From ! {Ref, Msgs},
			loop(State);			
		{chat, Msg} ->
			%% io:format("web_agent receive a msg ~p~n", [Msg]),
			player_client:chat(Player, Msg),
			loop(State);
		{get_user_name, Ref, From} ->
			From ! {Ref, UserName},
			loop(State);	
		{set_room, RoomID} ->
			io:format("~p enter room ~p again ~n", [UserName, RoomID]),		
			event_store:observe(RoomID, WebPlayer),
			loop(State);	
		{set_move, Move} ->
			web_player:set_move(WebPlayer, Move),
			loop(State);
		stop ->
			exit(stop);
		Unexpected ->
			error_logger:format("unexpected @enter_room ~p~n", [Unexpected]),
			loop(State)	
	    after ?TIME_OUT ->    %% keep state for 60 secs only
	        exit(time_out)				
	end.


handle_cast({login, UserName, Password}, State) ->
	{ok, Player} = player_client:start(UserName, web_player, board, "127.0.0.1", 8011),
	erlang:link(Player),
	NewState = case player_client:login(Player, Password) of
		ok ->
    		io:format("user ~p login~n", [UserName]),    		
			WebPlayer = player_client:get_player(Player),
			IsGuest = (UserName == "guest"),
			State#state{status = waiting_enter_room, username = UserName, player = Player, web_player = WebPlayer, is_guest = IsGuest};
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





