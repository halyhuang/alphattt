-module(player_agent).
-export([init/1, handle_tcp_data/2, handle_info/2]).

-record(state, {socket}).

init(Socket) ->
	{ok, #state{socket = Socket}}.

handle_tcp_data(TcpData, State) ->
	case binary_to_term(TcpData) of
		{echo, Msg} ->
			send_message({echo, Msg}, State);
		{enter_room, NickName} ->
			game:enter(self(), NickName),
			game:start_match(self());
		{leave_room, _NickName} ->
			game:leave(self());
		{play, Move} ->
			game:play(self(), Move);
		{start_match, _} ->
			game:start_match(self());
		Unexpected ->
			io:format("Unexpected is ~p~n", [Unexpected])
	end,
	{ok, State}.

handle_info(Msg, State) ->
	send_message(Msg, State),
	{ok, State}.

send_message(Msg, State) ->
	TcpData = term_to_binary(Msg),
	gen_tcp:send(State#state.socket, TcpData),
	ok.
		