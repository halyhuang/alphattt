-module(player_agent).
-export([init/1, handle_tcp_data/2, handle_info/2]).

-record(state, {socket, room = null, wait_login = true, username, password}).

init(Socket) ->
	State = #state{socket = Socket},
	send_message(challenge, State),
	{ok, State}.

handle_tcp_data(TcpData, State=#state{wait_login = true}) ->
	case binary_to_term(TcpData) of
		{echo, Msg} ->
			send_message({echo, Msg}, State),
			{ok, State};
		{challenge, UserName, Password} ->
			case game_auth:login(UserName, Password) of
				ok ->
					send_message({challenge_result, ok}, State),
					{ok, State#state{wait_login = false, username = UserName, password = Password}};
				Reaseon ->
					send_message({challenge_result, Reaseon}, State),
					{ok, State}							
			end;
		Unexpected ->
			io:format("Unexpected is ~p before challenge ~n", [Unexpected]),
			{ok, State}
	end;
handle_tcp_data(TcpData, State=#state{room = RoomPid, username = NickName}) ->
	NewState =  case binary_to_term(TcpData) of
					{echo, Msg} ->
						send_message({echo, Msg}, State);
					{enter_room, RoomID} ->
						case roommgr:enter(RoomID) of
							{ok, NewRoomPid} ->
								room:enter(NewRoomPid, {self(), NickName}),
								State#state{room = NewRoomPid};
							Reason ->
								io:format("enter room failed, reason ~p~n", [Reason]),
								State
						end;						
					leave_room ->
						room:leave(RoomPid, self()),
						State#state{room = null};
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
		