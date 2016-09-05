-module(player_agent).
-export([init/1, handle_tcp_data/2, handle_info/2]).

-record(state, {socket, room = none, wait_login = true}).

init(Socket) ->
	{ok, #state{socket = Socket}}.

handle_tcp_data(TcpData, State=#state{wait_login = true}) ->
	case binary_to_term(TcpData) of
		{echo, Msg} ->
			send_message({echo, Msg}, State),
			{ok, State};
		{login, UserName, Password, Ref, From} ->
			LoginState = game_auth:login(UserName, Password), 
			send_message({login, LoginState, Ref, From}, State),
			{ok, State#state{wait_login = false}};
		Unexpected ->
			io:format("Unexpected is ~p before login ~n", [Unexpected]),
			{ok, State}
	end;
handle_tcp_data(TcpData, State=#state{room = RoomPid}) ->
	NewState =  case binary_to_term(TcpData) of
					{echo, Msg} ->
						send_message({echo, Msg}, State);
					{enter_room, NickName, RoomID} ->
						case roommgr:enter(RoomID) of
							{ok, NewRoomPid} ->
								room:enter(NewRoomPid, {self(), NickName}),
								State#state{room = NewRoomPid};
							Reason ->
								io:format("enter room failed, reason ~p~n", [Reason]),
								State
						end;						
					{leave_room, _NickName} ->
						room:leave(RoomPid, self()),
						State#state{room = none};
					{play, Move} ->
						room:play(RoomPid, {self(), Move}),
						State;
					Unexpected ->
						io:format("Unexpected is ~p~n", [Unexpected]),
						State
				end,
	{ok, NewState}.

handle_info(Msg, State) ->
	send_message(Msg, State),
	{ok, State}.

send_message(Msg, State) ->
	TcpData = term_to_binary(Msg),
	gen_tcp:send(State#state.socket, TcpData),
	ok.
		