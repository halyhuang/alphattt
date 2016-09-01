-module(roommgr).

-behaviour (gen_server).

-export([start/2, enter/1, show/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {rooms = []}).

start(Board, RoomNum) ->
	{ok, Pid} = gen_server:start({local, ?MODULE}, ?MODULE, [Board, RoomNum],[]),	
	{ok, Pid}.

init([Board, RoomNum]) ->
	Rooms = [{RoomID, room:start(Board)} || RoomID <- lists:seq(1, RoomNum)],
	{ok, #state{rooms=Rooms}}.

enter(RoomID) ->
	gen_server:call(?MODULE, {enter, RoomID}).

show() ->
	gen_server:cast(?MODULE, show).

handle_info(_Msg, State) ->
	{noreply, State}.

handle_cast(show, State=#state{rooms=Rooms}) ->
	[begin
		{RoomState, Players} = room:get_state(RoomPid),
		io:format("Room ~p is ~p, players ~p~n", [RoomID, RoomState, Players])
	end || {RoomID, RoomPid} <- Rooms],
	{noreply, State}.

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








			


	









