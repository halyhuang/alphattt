-module(roommgr).

-behaviour (gen_server).

-export([start/2, enter/1, show/0, get_empty_room/0, get_all_rooms/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {board, rooms = []}).

start(Board, RoomNum) ->
	{ok, Pid} = gen_server:start({global, ?MODULE}, ?MODULE, [Board, RoomNum],[]),	
	{ok, Pid}.

init([Board, RoomNum]) ->
	Rooms = [ spawn_room(RoomID, Board) || RoomID <- lists:seq(1, RoomNum)],
	{ok, #state{board = Board, rooms=Rooms}}.

spawn_room(RoomID, Board) ->
	{ok, Pid} = room:start(Board),
	Ref = erlang:monitor(process, Pid),
	{RoomID, Pid, Ref}.

enter(RoomID) ->
	gen_server:call({global, ?MODULE}, {enter, RoomID}).

show() ->
	[io:format("Room ~p is ~p, players ~p~n", [RoomID, RoomState, Players])
	 || {RoomID, {RoomState, Players}} <- get_all_rooms()],
	ok.

get_all_rooms() ->
	gen_server:call({global, ?MODULE}, get_all_rooms).	

get_empty_room() ->
	gen_server:call({global, ?MODULE}, get_empty_room).			

handle_info({'DOWN', _, process, Pid, Reason}, State=#state{board = Board, rooms=Rooms}) ->
	case lists:keyfind(Pid, 2, Rooms) of
		{RoomID, Pid, _Ref} ->
			io:format("room ~p down, reason ~p~n", [RoomID, Reason]),
			NewRoom = spawn_room(RoomID, Board),
			NewRooms = lists:keyreplace(Pid, 2, Rooms, NewRoom),
		    {noreply, State#state{rooms = NewRooms}};
		_ ->
			io:format("room ~p down, reason ~p, can't find room~n", [Pid, Reason]),
			{noreply, State}
	end.


handle_cast(show, State=#state{rooms=Rooms}) ->
	[begin
		{RoomState, Players} = room:get_state(RoomPid),
		io:format("Room ~p is ~p, players ~p~n", [RoomID, RoomState, Players])
	end || {RoomID, RoomPid, _Ref} <- Rooms],
	{noreply, State}.


handle_call(get_empty_room, _From, State=#state{rooms=Rooms}) ->
	RoomStates = [ 
					begin
						{RoomState, Players} = room:get_state(RoomPid),
						{{RoomState, length(Players)}, RoomID}
					end || {RoomID, RoomPid, _Ref} <- Rooms],
	Reply = case lists:keyfind({waiting, 0}, 1, RoomStates) of
				{_, RoomID} ->
					{ok, RoomID};
				_ ->
					"no empty room"
			end,
	{reply, Reply, State};

handle_call(get_all_rooms, _From, State=#state{rooms=Rooms}) ->
	RoomStates = [ {RoomID, room:get_state(RoomPid)} || {RoomID, RoomPid, _Ref} <- Rooms],
	{reply, RoomStates, State};

handle_call({enter, RoomID}, _From, State=#state{rooms=Rooms}) ->
	Reply = case lists:keyfind(RoomID, 1, Rooms) of
		{RoomID, RoomPid, _Ref} -> 
			{ok, RoomPid};
		_ ->
			io:format("RoomID ~p not exist~n", [RoomID]),	
			room_not_exist
	end,
	{reply, Reply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.








			


	









