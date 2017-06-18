-module(room_mgr).

-behaviour (gen_server).

-export([start_link/2, enter/1, observe/1, show/0, get_all_rooms/0, get_all_rooms/1, get_room_state/1, reset/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {board, rooms = [], room_number = 0}).

start_link(Board, RoomNum) ->
	{ok, Pid} = gen_server:start_link({global, ?MODULE}, ?MODULE, [Board, RoomNum],[]),	
	{ok, Pid}.

init([Board, RoomNum]) ->
	Rooms = [ spawn_room(RoomID, Board, practice) || RoomID <- lists:seq(1, RoomNum)] ++
			[ spawn_room(RoomID, Board, game) || RoomID <- lists:seq(RoomNum + 1, 2 * RoomNum)],
	{ok, #state{board = Board, rooms=Rooms, room_number = RoomNum}}.

spawn_room(RoomID, Board, RoomType) ->
	{ok, Pid} = room:start(Board, RoomID, RoomType),
	Ref = erlang:monitor(process, Pid),
	{RoomID, Pid, Ref}.

enter(RoomID) ->
	gen_server:call({global, ?MODULE}, {enter, RoomID}).

observe(RoomID) ->
	gen_server:cast({global, ?MODULE}, {observe, RoomID, self()}).	

reset(RoomID) ->
	gen_server:cast({global, ?MODULE}, {reset, RoomID}).

show() ->
	[io:format("Room ~p is ~p, players ~p~n", [RoomID, RoomState, Players])
	 || {RoomID, RoomState, Players} <- get_all_rooms()],
	ok.

get_all_rooms() ->
	gen_server:call({global, ?MODULE}, {get_all_rooms, all}).	

get_all_rooms(practice) ->
	gen_server:call({global, ?MODULE}, {get_all_rooms, practice});
get_all_rooms(game) ->
	gen_server:call({global, ?MODULE}, {get_all_rooms, game}).				

get_room_state(RoomID) ->
	gen_server:call({global, ?MODULE}, {get_room_state, RoomID}).

get_room_type(RoomID, RoomNum) ->
	case RoomID =< RoomNum of
		true -> practice;
		false -> game
	end.

handle_info({'DOWN', _, process, Pid, Reason}, State=#state{board = Board, rooms=Rooms, room_number=RoomNum}) ->
	case lists:keyfind(Pid, 2, Rooms) of
		{RoomID, Pid, _Ref} ->
			NewRoom = spawn_room(RoomID, Board, get_room_type(RoomID, RoomNum)),
			NewRooms = lists:keyreplace(Pid, 2, Rooms, NewRoom),
		    {noreply, State#state{rooms = NewRooms}};
		_ ->
			error_logger:format("room ~p down, reason ~p, can't find room~n", [Pid, Reason]),
			{noreply, State}
	end.

handle_cast({observe, RoomID, Observer}, State=#state{rooms=Rooms}) ->
	case lists:keyfind(RoomID, 1, Rooms) of
		{RoomID, RoomPid, _Ref} -> room:observe(RoomPid, Observer);
		_ -> ok
	end,
	{noreply, State};

handle_cast({reset, RoomID}, State=#state{rooms=Rooms}) ->
	case lists:keyfind(RoomID, 1, Rooms) of
		{RoomID, RoomPid, _Ref} ->
			room:reset(RoomPid);
		_ ->
			io:format("Room ~p not exist~n", [RoomID])
	end,
	{noreply, State}.

handle_call({get_all_rooms, RoomType}, _From, State=#state{rooms=Rooms, room_number=RoomNum}) ->
	Fun = fun(RoomID, RoomPid) ->
			{Status, Players, _} = room:get_state(RoomPid),
			PlayerTypes = [ begin
								Type = db_api:get_user_type(Player), 
								{Player, atom_to_list(Type)} 
							end || Player <- Players ],					    
			{RoomID, Status, PlayerTypes} 
		end,
	RoomStates = 
		case RoomType of
			all -> 	[ Fun(RoomID, RoomPid)  || {RoomID, RoomPid, _Ref} <- Rooms];
			practice -> [ Fun(RoomID, RoomPid)  || {RoomID, RoomPid, _Ref} <- Rooms, RoomID =< RoomNum];
			game -> [ Fun(RoomID, RoomPid)  || {RoomID, RoomPid, _Ref} <- Rooms, RoomID > RoomNum]
		end,
	{reply, RoomStates, State};

handle_call({get_room_state, RoomID}, _From, State=#state{rooms=Rooms}) ->
	RoomStatus = case lists:keyfind(RoomID, 1, Rooms) of
					{RoomID, RoomPid, _Ref} ->
						{ok, room:get_state(RoomPid)};						
					_ ->
						error_logger:format("RoomID ~p not exist~n", [RoomID]),
						room_not_exist
			     end,
	{reply, RoomStatus, State};	

handle_call({enter, RoomID}, _From, State=#state{rooms=Rooms}) ->
	Reply = case lists:keyfind(RoomID, 1, Rooms) of
		{RoomID, RoomPid, _Ref} -> 
			{ok, RoomPid};
		_ ->
			error_logger:format("RoomID ~p not exist~n", [RoomID]),	
			room_not_exist
	end,
	{reply, Reply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.








			


	









