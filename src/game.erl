-module(game).

-behaviour (gen_server).

-export([start/0, enter/2, leave/1, start_match/1, play/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {idle_players=[], % player = {Pid, NickName, MonitorRef}
				playing_players=[], %{player_first, player_second}
				matching_players=[],%
				board,
				room
				}).

start() ->
	tcp_server:start(8011, player_agent),
	{ok, Pid} = gen_server:start({local, ?MODULE}, ?MODULE, [room, board],[]),	
	{ok, Pid}.

init([Room, Board]) ->
	{ok, #state{board=Board, room=Room}}.

enter(Pid, NickName) ->
	game ! {enter, Pid, NickName}.

leave(Pid) ->
	game ! {leave, Pid}.

start_match(Pid) ->
	game ! {start_match, Pid}.

play(Pid, Move) ->
	game ! {play, Pid, Move}.

handle_info({enter, Pid, NickName}, #state{idle_players=IdlePlayers, 
	playing_players=PlayingPlayers, matching_players=MatchingPlayers}=State) ->
	NewIdlePlayers = [{Pid, NickName}|IdlePlayers],
	Pid ! {notify, "Welcome! There are(" 
		++ integer_to_list(length(IdlePlayers)) ++ ") IdlePlayers, (" 
		++ integer_to_list(length(PlayingPlayers)) ++ ") PlayingPlayers, ("
		++ integer_to_list(length(MatchingPlayers)) ++ ") MatchingPlayers."},
	{noreply, State#state{idle_players=NewIdlePlayers}};
handle_info({start_match, Pid}, #state{idle_players=IdlePlayers, 
	playing_players=PlayingPlayers, matching_players=MatchingPlayers}=State) -> 
	case lists:keyfind(Pid, 1, IdlePlayers) of
		{Pid, NickName} ->
			NewIdlePlayers = lists:keydelete(Pid, 1, IdlePlayers),
			io:format("MatchingPlayers ~p~n", [MatchingPlayers]),			
			case MatchingPlayers of
				[] ->
					{noreply, State#state{idle_players=NewIdlePlayers,
										  matching_players=[{Pid, NickName}]}};
				[{Pid2, NickName2}] ->
					{ok, RoomPid} = room:start(board), 
					room:enter(RoomPid, {Pid, NickName}),
					room:enter(RoomPid, {Pid2, NickName2}),
					NewPlayingPlayers = [{Pid2, NickName2, RoomPid} ,{Pid, NickName, RoomPid} | PlayingPlayers],
					{noreply, State#state{idle_players=NewIdlePlayers,
										  matching_players=[],
										  playing_players=NewPlayingPlayers}};
				Unexpected ->
					io:format("Unexpected ~p~n", [Unexpected])
			end;
		false ->
			{noreply, State}
	end;
handle_info({play, Pid, Move}, #state{playing_players=PlayingPlayers}=State) ->
	case lists:keyfind(Pid, 1, PlayingPlayers) of
		{_, _, RoomPid} ->
			room:play(RoomPid, {Pid, Move}),
			{noreply, State};
		false ->
			{noreply, State}
	end.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_call(_Msg, _From, State) ->
	{noreply, ok, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.








			


	









