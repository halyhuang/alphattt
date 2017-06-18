-module(event_store).

-behaviour (gen_server).

-export([start_link/0, observe/2, unsuscribe/1, show/0, get_all_rooms/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {rooms = []}).

start_link() ->
	{ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),	
	{ok, Pid}.

init([]) ->
	{ok, #state{}}.

observe(RoomID, WebPlayer) ->
	gen_server:cast(?MODULE, {observe, RoomID, WebPlayer}).	

unsuscribe(WebPlayer) ->
	gen_server:cast(?MODULE, {unsuscribe, WebPlayer}).	

get_all_rooms() ->
	gen_server:call(?MODULE, get_all_rooms).	
	
show() ->
	Rooms = get_all_rooms(),
	[ io:format("Room ~p Moves ~p observers ~p~n", [RoomID, Moves, Obs]) || {RoomID, Moves, Obs} <- Rooms],
	ok.
		
handle_info({update, RoomID, GameState, Move}, State=#state{rooms=Rooms}) ->
	NewRooms = case lists:keyfind(RoomID, 1, Rooms) of
					{RoomID, Moves, Obs} ->
						[web_player:display(WebPlayer, GameState, Move) || WebPlayer <- Obs],
						NewMoves = case Move of
										none -> [{GameState, Move}]; %% clear previous moves					 
										_ -> [{GameState, Move} | Moves]
								   end,
						lists:keyreplace(RoomID, 1, Rooms, {RoomID, NewMoves, Obs});						
					_ -> Rooms						
				end,
	{noreply, State#state{rooms = NewRooms}};
	
handle_info({notify_observer, RoomID, Msg}, State=#state{rooms=Rooms}) ->
%%	io:format("receive msg ~p~n", [Msg]),
	case lists:keyfind(RoomID, 1, Rooms) of
		{RoomID, _Moves, Obs} ->
			[web_player:notify(WebPlayer, Msg) || WebPlayer <- Obs];
		_ -> ok			
	end,
	{noreply, State};

handle_info({chat_observer, RoomID, Msg}, State=#state{rooms=Rooms}) ->
	%% io:format("event_store chat_observer ~p ~p ~p ~n", [RoomID, Msg, Rooms]),
	case lists:keyfind(RoomID, 1, Rooms) of
		{RoomID, _Moves, Obs} ->
			[web_player:chat(WebPlayer, Msg) || WebPlayer <- Obs];
		_ -> ok			
	end,
	{noreply, State};	

handle_info({'DOWN', _, process, WebPlayer, _Reason}, State=#state{rooms=Rooms}) ->
	NewRooms = delete_suscriber(Rooms, WebPlayer),
	{noreply, State#state{rooms = NewRooms}}.	

delete_suscriber(Rooms, WebPlayer) ->
	[{RoomID, Moves, lists:delete(WebPlayer, Obs)}  || {RoomID, Moves, Obs} <- Rooms].

handle_cast({unsuscribe, WebPlayer}, State=#state{rooms=Rooms}) ->
	NewRooms = delete_suscriber(Rooms, WebPlayer),
	{noreply, State#state{rooms =NewRooms}};

handle_cast({observe, RoomID, WebPlayer}, State=#state{rooms=Rooms}) ->
	room_mgr:observe(RoomID),
	NewRooms = case lists:keyfind(RoomID, 1, Rooms) of
					{RoomID, Moves, Obs} ->
						NewObs = case lists:member(WebPlayer, Obs) of
									true -> Obs;
						 			   _ -> 
						 			   		erlang:monitor(process, WebPlayer),
											[web_player:display(WebPlayer, GameState, Move) || {GameState, Move} <- lists:reverse(Moves)],
						 			   		[WebPlayer | Obs]
								 end,
						lists:keyreplace(RoomID, 1, Rooms, {RoomID, Moves, NewObs});			
					_ ->
						erlang:monitor(process, WebPlayer),
						[{RoomID, [], [WebPlayer]} | Rooms]
				end,
	{noreply, State#state{rooms = NewRooms}}.


handle_call(get_all_rooms, _From, State=#state{rooms=Rooms}) ->
	{reply, Rooms, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.