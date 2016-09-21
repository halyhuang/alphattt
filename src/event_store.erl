-module(event_store).

-behaviour (gen_server).

-export([start/1, observe/2, show/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {board, rooms = []}).

start(Board) ->
	{ok, Pid} = gen_server:start({local, ?MODULE}, ?MODULE, [Board], []),	
	{ok, Pid}.

init([Board]) ->
	{ok, #state{board = Board}}.

observe(RoomID, WebPlayer) ->
	gen_server:call(?MODULE, {observe, RoomID, WebPlayer}).	

show() ->
	gen_server:cast(?MODULE, show).

next_player(1) -> 2;
next_player(2) -> 1.
		
handle_info({update, RoomID, GameState, Move}, State=#state{rooms=Rooms, board = Board}) ->
	case lists:keyfind(RoomID, 1, Rooms) of
		{RoomID, Moves, Obs} ->
			[web_player:display(WebPlayer, GameState, Move) || WebPlayer <- Obs],
			NewMoves = case Move of
							none -> [];					 
							_ -> [{next_player(Board:current_player(GameState)), Move} | Moves]
					   end,
			NewRooms = lists:keyreplace(RoomID, 1, Rooms, {RoomID, NewMoves, Obs}),		
			{noreply, State#state{rooms = NewRooms}};
		_ ->
			{noreply, State}
	end;
	
handle_info({notify_observer, RoomID, Msg}, State=#state{rooms=Rooms}) ->
	io:format("receive msg ~p~n", [Msg]),
	case lists:keyfind(RoomID, 1, Rooms) of
		{RoomID, _Moves, Obs} ->
			[web_player:notify(WebPlayer, Msg) || WebPlayer <- Obs],
			{noreply, State};
		_ ->
			{noreply, State}
	end;

handle_info({'DOWN', _, process, WebPlayer, _Reason}, State=#state{rooms=Rooms}) ->
	NewRooms = [{RoomID, Moves, lists:delete(WebPlayer, Obs)}  || {RoomID, Moves, Obs} <- Rooms],
	{noreply, State#state{rooms = NewRooms}}.	


handle_cast(show, State=#state{rooms=Rooms}) ->
	[ io:format("Room ~p observers ~p, Moves ~p~n", [RoomID, Obs, Moves]) || {RoomID, Moves, Obs} <- Rooms],
	{noreply, State}.


handle_call({observe, RoomID, WebPlayer}, _From, State=#state{rooms=Rooms}) ->
	case lists:keyfind(RoomID, 1, Rooms) of
		{RoomID, Moves, Obs} ->
			NewObs = case lists:member(WebPlayer, Obs) of
								true -> Obs;
					 			   _ -> [WebPlayer | Obs]
					 end,
			NewRooms = lists:keyreplace(RoomID, 1, Rooms, {RoomID, Moves, NewObs}),
			{reply, Moves, State#state{rooms = NewRooms}};			
		_ ->
			Moves = room_mgr:observe(RoomID),
			_Ref = erlang:monitor(process, WebPlayer),
			{reply, Moves, State#state{rooms = [{RoomID, Moves, [WebPlayer]} | Rooms]}}
	end.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.








			


	









