-module(roommgr).

-behaviour (gen_server).

-export([start/2, enter/1, show/0, get_all_rooms/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {rooms = []}).

start(Board, RoomNum) ->
	{ok, Pid} = gen_server:start({global, ?MODULE}, ?MODULE, [Board, RoomNum],[]),	
	{ok, Pid}.

init([Board, RoomNum]) ->
	Rooms = [{RoomID, room:start(Board)} || RoomID <- lists:seq(1, RoomNum)],
	{ok, #state{rooms=Rooms}}.

enter(RoomID) ->
	gen_server:call({global, ?MODULE}, {enter, RoomID}).

show() ->
	[io:format("Room ~p is ~p, players ~p~n", [RoomID, RoomState, Players])
	 || {RoomID, {RoomState, Players}} <- get_all_rooms()],
	ok.

get_all_rooms() ->
	gen_server:call({global, ?MODULE}, get_all_rooms).	

handle_info(_Msg, State) ->
	{noreply, State}.

handle_cast(show, State=#state{rooms=Rooms}) ->
	[begin
		{RoomState, Players} = room:get_state(RoomPid),
		io:format("Room ~p is ~p, players ~p~n", [RoomID, RoomState, Players])
	end || {RoomID, RoomPid} <- Rooms],
	{noreply, State}.


handle_call(get_all_rooms, _From, State=#state{rooms=Rooms}) ->
	RoomStates = [ {RoomID, room:get_state(RoomPid)} || {RoomID, RoomPid} <- Rooms],
	{reply, RoomStates, State};
handle_call({enter, RoomID}, _From, State=#state{rooms=Rooms}) ->
	Reply = case lists:keyfind(1, RoomID, Rooms) of
		{RoomID, RoomPid} -> 
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








			


	









