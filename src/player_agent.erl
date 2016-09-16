-module(player_agent).
-export([init/1, handle_tcp_data/2, handle_info/2]).

-record(state, {socket, status = wait_login, room = none}).

init(Socket) ->
	{ok, #state{socket = Socket}}.

handle_tcp_data(TcpData, State=#state{status = wait_login}) ->
	case binary_to_term(TcpData) of
		{echo, Msg} ->
			send_message({echo, Msg}, State),
			{ok, State};		
		{login, UserName, Password} ->
			LoginState = game_auth:login(UserName, Password), 
			send_message({login, LoginState}, State),
			{ok, State#state{status = wait_enter_room}};		
		Unexpected ->
			Notify = io_lib:format("Unexpected is ~p before login ~n", [Unexpected]),
			send_message({notify, Notify}, State),
			{ok, State}
	end;

handle_tcp_data(TcpData, State=#state{status = wait_enter_room}) ->
	case binary_to_term(TcpData) of
		{echo, Msg} ->
			send_message({echo, Msg}, State);		
		{show_room, _NickName} ->
			send_message({show_room, room_mgr:get_all_rooms()}, State),
			{ok, State};		
		{enter_room, NickName, RoomID} ->
			case room_mgr:enter(RoomID) of
				{ok, NewRoomPid} ->
					room:enter(NewRoomPid, {self(), NickName}),
					{ok, State#state{status = enter_room, room = NewRoomPid}};
				Reason ->
					Notify = io_lib:format("player ~p enter room failed, reason ~p~n", [NickName, Reason]),
					send_message({notify, Notify}, State),
					{ok, State}
			end;	
		{observe, NickName, RoomID} ->
			case room_mgr:enter(RoomID) of
				{ok, NewRoomPid} ->
					room:observe(NewRoomPid, {self(), NickName}),
					{ok, State#state{status = enter_room, room = NewRoomPid}};
				Reason ->
					Notify = io_lib:format("player ~p observe room failed, reason ~p~n", [NickName, Reason]),
					send_message({notify, Notify}, State),
					{ok, State}
			end;	
		{notify, _PlayerID, _Info} ->
			{ok, State};							
		Unexpected ->
			Notify = io_lib:format("Unexpected is ~p when wait_enter_room ~n", [Unexpected]),
			send_message({notify, Notify}, State),
			{ok, State}
	end;	

handle_tcp_data(TcpData, State=#state{status = enter_room, room = RoomPid}) ->
    case binary_to_term(TcpData) of
		{echo, Msg} ->
			send_message({echo, Msg}, State);
		{enter_room, NickName, RoomID} ->
			case room_mgr:enter(RoomID) of
				{ok, NewRoomPid} ->			
					room:enter(NewRoomPid, {self(), NickName}),
					{ok, State#state{room = NewRoomPid}};
				Reason ->
					Notify = io_lib:format("player ~p enter room ~p failed, reason ~p~n", [NickName, RoomID, Reason]),
					send_message({notify, Notify}, State),
					{ok, State}
			end;	
		{observe, NickName, RoomID} ->
			case room_mgr:enter(RoomID) of
				{ok, NewRoomPid} ->
					room:observe(NewRoomPid, {self(), NickName}),
					{ok, State#state{status = enter_room, room = NewRoomPid}};
				Reason ->
					Notify = io_lib:format("player ~p observe room failed, reason ~p~n", [NickName, Reason]),
					send_message({notify, Notify}, State),
					{ok, State}
			end;									
		{leave_room, NickName} ->
			room:leave(RoomPid, self()),
			Notify = io_lib:format("player ~p leave room~n", [NickName]),
			send_message({notify, Notify}, State),
			{ok, State#state{status = wait_enter_room}};									
		{play, Move} ->
			room:play(RoomPid, {self(), Move}),
			{ok, State};
		{notify, PlayerID, Info} ->
			room:notify_player(RoomPid, PlayerID, Info),
			{ok, State};
		Unexpected ->
			Notify = io_lib:format("Unexpected is ~p when enter_room ~n", [Unexpected]),
			send_message({notify, Notify}, State),			
			{ok, State}
	end.

handle_info(Msg, State) ->
	send_message(Msg, State),
	{ok, State}.

send_message(Msg, State) ->
	TcpData = term_to_binary(Msg),
	gen_tcp:send(State#state.socket, TcpData),
	ok.
		