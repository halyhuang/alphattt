-module(web_agent).
-export([start/0, login/3, logout/1, is_login/1, set_room/2, enter_room/1, leave_room/1, start_robot/3, 
		get_info/1, start_observe/1, is_move/1, get_legal_moves/1, set_move/2, is_display_move/1, get_display_move/1, get_room/1, show/1, stop/1]).

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

set_room(Pid, RoomID) ->
	call(Pid, {set_room, RoomID}).

get_room(Pid) ->
	call(Pid, get_room).

enter_room(Pid) ->
	call(Pid, enter_room).

leave_room(Pid) ->
	call(Pid, leave_room).	

start_robot(Pid, RobotName, RobotType) ->
	call(Pid, {start_robot, RobotName, RobotType}).

set_move(Pid, Move) ->
	call(Pid, {set_move, Move}).

is_display_move(Pid) ->
	call(Pid, is_display_move).	

get_display_move(Pid) ->
	call(Pid, get_display_move).

is_move(Pid) ->
	call(Pid, is_move).

get_legal_moves(Pid) ->
	call(Pid, get_legal_moves).	

start_observe(Pid) ->
	call(Pid, start_observe).	

get_info(Pid) ->
	call(Pid, get_info).		

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

handle_call({set_room, RoomID}, State) ->
    io:format("set room ~p~n", [RoomID]),
	{reply, ok, State#state{room = RoomID}};

handle_call(get_room, State=#state{room = none}) ->
	{reply, 0, State};
handle_call(get_room, State=#state{room = RoomID}) ->
	{reply, RoomID, State};	

handle_call(enter_room, State=#state{username = UserName, room = none}) ->
    io:format("~p room isn't set~n", [UserName]),
	{reply, ok, State};

handle_call(enter_room, State=#state{username = UserName, room = RoomID, player = Player}) ->
    io:format("~p enter room ~p~n", [UserName, RoomID]),
	player_client:enter_room(Player, RoomID),
	{reply, ok, State#state{room = RoomID}};

handle_call(leave_room, State=#state{username = UserName, room = none}) ->
    io:format("~p room isn't set~n", [UserName]),
	{reply, ok, State};

handle_call(leave_room, State=#state{username = UserName, room = RoomID, player = Player, web_player = WebPlayer}) ->
    io:format("~p leave room ~p~n", [UserName, RoomID]),
	player_client:leave_room(Player),
	case WebPlayer of
		none -> 
			ok;
		WebPlayer ->
			player_client:leave_room(WebPlayer)
	end,
	{reply, ok, State#state{room = none}};

handle_call(is_move, State=#state{web_player = WebPlayer}) ->
	{ok, IsMove} = web_player:is_move(WebPlayer),
	{reply, IsMove, State};

handle_call(is_display_move, State=#state{web_player = WebPlayer}) ->
	{ok, IsMove} = web_player:is_display_move(WebPlayer),
	{reply, IsMove, State};

handle_call(get_display_move, State=#state{web_player = WebPlayer}) ->
	{ok, Moves} = web_player:get_display_move(WebPlayer),					    		
	{reply, Moves, State};

handle_call(get_legal_moves, State=#state{web_player = none}) ->
	{reply, [], State};
handle_call(get_legal_moves, State=#state{web_player = WebPlayer}) ->
	{ok, PlayerID, LegalMoves} = web_player:get_legal_moves(WebPlayer),	
	{reply, {PlayerID, LegalMoves}, State};
	
handle_call({set_move, Move}, State=#state{username = UserName, web_player = WebPlayer}) ->
	io:format("~p Move ~p~n", [UserName, Move]),
	web_player:set_move(WebPlayer, Move),	
	{reply, ok, State};

handle_call(get_info, State=#state{web_player = WebPlayer}) ->
	Infos = get_player_info(WebPlayer),			    		
	{reply, Infos, State};

handle_call(start_observe, State=#state{room = none, username = UserName}) ->
	io:format("~p observe room invalid!~n", [UserName]),
	{reply, [], State};

handle_call(start_observe, State=#state{room = RoomID, username = UserName, web_player = WebPlayer}) ->
	io:format("~p observe room ~p~n", [UserName, RoomID]),
	Moves = event_store:observe(RoomID, WebPlayer),
	{reply, Moves, State};	

handle_call({start_robot, RobotName, _RobotType}, State=#state{room = none}) ->
    io:format("~p room isn't set~n", [RobotName]),
    {reply, ok, State};

handle_call({start_robot, RobotName, RobotType}, State=#state{robot_player = none}) ->
	{ok, RobotPlayer} = player_client:start(RobotName, RobotType, board, "127.0.0.1", 8011),	
	erlang:link(RobotPlayer),	
	player_client:login(RobotPlayer, ""),
	handle_call({start_robot, RobotName, RobotType}, State#state{robot_player = RobotPlayer});

handle_call({start_robot, RobotName, _RobotType}, State=#state{robot_player = RobotPlayer, room = RoomID}) ->
	player_client:enter_room(RobotPlayer, RoomID),
	io:format("~p enter room ~p~n", [RobotName, RoomID]),
	{reply, ok, State}.


get_player_info(none) -> [];
get_player_info(WebPlayer) -> 
	web_player:get_info(WebPlayer).




