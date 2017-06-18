-module(player_client).
-export([start/5]).
-export([login/2, enter_room/2, leave_room/1, show_room/1, get_info/1, show/1, stop/1]).
-export([get_player/1]).

-record(state, {nickname,
				type,
				player,
				board,
				socket,
			    ref,
			    from}).
start(NickName, PlayerType, Board, SIp, SPort) ->
	Pid = spawn(fun() -> init(NickName, PlayerType, 
							  Board, SIp, SPort) end),
	{ok, Pid}.

login(Pid, Password) ->
	call(Pid, {login, Password}).	

enter_room(Pid, RoomID) ->
	Pid ! {enter_room, RoomID},
	ok.

leave_room(Pid) ->
	Pid ! leave_room,
	ok.
show_room(Pid) ->
	call(Pid, show_room).

get_info(Pid) ->
	call(Pid, get_info).	

stop(Pid) ->
	Pid ! stop,
	ok.

show(Pid) ->
	Pid ! show,
	ok.	

get_player(Pid) ->
	call(Pid, get_player).	

call(Pid, Msg) ->
	Ref = make_ref(),
	Pid ! {Msg, Ref, self()},
	receive
		{Ref, Reply} ->
			Reply
	end.	

init(NickName, PlayerType, Board, SIp, SPort) ->
	io:format("connect to ~p~n", [{SIp, SPort}]),
	{ok, Sock} = gen_tcp:connect(SIp, SPort, [binary, {active, true},
													  {packet, 2}]),
	Player = player:start(PlayerType, Board),
	erlang:link(Player),
	loop(#state{nickname=NickName,
			    type=PlayerType,
			    player=Player,
			    board=Board,
			    socket=Sock}).

loop(State = #state{nickname=NickName,
					type=Type,
					player=Player,
					board=_Board,
					socket=Sock,
					ref = StoreRef, 
					from = StoreFrom}) ->
	receive
		{echo, Msg} ->
			gen_tcp:send(Sock, term_to_binary({echo, Msg}));			
		{{login, Password}, Ref, From} ->
			gen_tcp:send(Sock, term_to_binary({login, NickName, Password})),
			loop(State#state{ref = Ref, from = From});		
		{enter_room, RoomID} ->
			gen_tcp:send(Sock, term_to_binary({enter_room, NickName, RoomID})),
			loop(State);
		leave_room ->
			gen_tcp:send(Sock, term_to_binary({leave_room, NickName})),
			loop(State);
		{show_room, Ref, From} ->
			gen_tcp:send(Sock, term_to_binary({show_room, NickName})),
			loop(State#state{ref = Ref, from = From});	
		{play, Move} ->
			gen_tcp:send(Sock, term_to_binary({play, Move})),
			loop(State);
		{notify, PlayerID, Info} ->
			gen_tcp:send(Sock, term_to_binary({notify, PlayerID, Info})),
			loop(State);						
		stop ->
			player:stop(Type, Player);	
		{get_player, Ref, From} ->
			From ! {Ref, Player},
			loop(State);				
		show ->
			player:show(Type, Player),
			loop(State);
		{tcp, _, TcpData} ->
			case binary_to_term(TcpData) of
				{echo, Msg} ->
					io:format("ECHO: ~p~n", [Msg]);
				{login, Result} ->
					case Result of
						ok ->
							io:format("User ~p success login~n", [NickName]);
						Reason ->
							io:format("User ~p login failed, reason ~p~n", [NickName, Reason])
					end,
					StoreFrom ! {StoreRef, Result};	
				{show_room, Reply} ->
					StoreFrom ! {StoreRef, Reply};
				{update, Move, GameState} ->
					player:update(Type, Player, GameState),
					player:display(Type, Player, GameState, Move);
				{notify, Msg} ->
					player:notify(Type, Player, Msg);
				play ->
					player:get_move(Type, Player);
				stop ->
					player:stop(Type, Player);
				Unexpected ->
					io:format("client receive unexpected tcp ~p~n", [Unexpected])
			end,
			loop(State)
	end.
