-module(room_mgr).

-behaviour (gen_server).

-export([start_link/2, enter/1, observe/1, show/0, get_all_rooms/0, get_room_state/1, reset/1, test/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {board, rooms = []}).

start_link(Board, RoomNum) ->
	{ok, Pid} = gen_server:start_link({global, ?MODULE}, ?MODULE, [Board, RoomNum],[]),	
	{ok, Pid}.

init([Board, RoomNum]) ->
	Rooms = [ spawn_room(RoomID, Board) || RoomID <- lists:seq(1, RoomNum)],
	{ok, #state{board = Board, rooms=Rooms}}.

spawn_room(RoomID, Board) ->
	{ok, Pid} = room:start(Board, RoomID),
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

test() ->
	gen_server:cast({global, ?MODULE}, test).	

get_all_rooms() ->
	gen_server:call({global, ?MODULE}, get_all_rooms).			

get_room_state(RoomID) ->
	gen_server:call({global, ?MODULE}, {get_room_state, RoomID}).

handle_info({'DOWN', _, process, Pid, Reason}, State=#state{board = Board, rooms=Rooms}) ->
	case lists:keyfind(Pid, 2, Rooms) of
		{RoomID, Pid, _Ref} ->
			error_logger:format("room ~p down, reason ~p~n", [RoomID, Reason]),
			NewRoom = spawn_room(RoomID, Board),
			NewRooms = lists:keyreplace(Pid, 2, Rooms, NewRoom),
		    {noreply, State#state{rooms = NewRooms}};
		_ ->
			error_logger:format("room ~p down, reason ~p, can't find room~n", [Pid, Reason]),
			{noreply, State}
	end.


handle_cast(test, State=#state{rooms=Rooms}) ->
	case lists:keyfind(none_none, 1, Rooms) of
		{_RoomID, RoomPid, _Ref} ->
			room:reset(RoomPid)
	end,
	{noreply, State};

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

handle_call(get_all_rooms, _From, State=#state{rooms=Rooms}) ->
	RoomStates = [ begin
					{Status, Players} = room:get_state(RoomPid),
					PlayerTypes = [ begin
										Type = db_api:get_user_type(Player),							
										 {Player, atom_to_list(Type)}
								    end || Player <- Players ],					    
					{RoomID, Status, PlayerTypes} end || {RoomID, RoomPid, _Ref} <- Rooms],
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








			


	









