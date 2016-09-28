-module(web_player).
-export([start/1, start/3, update/2, display/3, get_move/1, leave_room/1, stop/1]).
-export([get_legal_moves/1, set_move/2, is_move/1, get_display_move/1, get_info/1, notify/2]).

-record(state,  {board = board,
			     game_states = [],
			     infos = [],
			     is_get_move = false,
			     moves = [],
			     player_client,
			     move}).

%% APIs.

start(Board, _MaxTime, _ExplorationFactor) ->
	start(Board).

start(Board) ->
	spawn(fun() -> 
				init([Board])
		  end).

update(Pid, GameState) ->
	Pid ! {update, GameState}.

display(Pid, GameState, Move) ->
	Pid ! {display, GameState, Move}.

notify(Pid, Info) ->
	Pid ! {notify, Info}.	

get_move(Pid) ->
	Pid ! {get_move, self()}.

set_move(Pid, Move) ->
	Pid ! {set_move, Move}.

leave_room(Pid) ->
	Pid ! leave_room.	

stop(Pid) ->
	Pid ! stop.	

is_move(Pid) ->
	call(Pid, is_move).		

get_legal_moves(Pid) ->
	call(Pid, get_legal_moves).	

get_display_move(Pid) ->
	call(Pid, get_display_move).	

get_info(Pid) ->
	call(Pid, get_info).	

init([Board]) ->
	loop(#state{board = Board, game_states = []}).

loop(State) ->
	receive
		{call, Ref, From, Msg} ->
			case handle_call(Msg, State) of
				{reply, Reply, NewState} ->
					From ! {Ref, Reply},
					loop(NewState)				
			end;
		stop ->			
			stop;			
		Msg ->
			case handle_cast(Msg, State) of
				{noreply, NewState} ->
					loop(NewState)
			end
	end.


call(Pid, Msg) ->
	Ref = make_ref(),
	Pid ! {call, Ref, self(), Msg},
	receive
		{Ref, Reply} ->
			Reply		
	end.

next_player(1) -> 2;
next_player(2) -> 1;
next_player(_) -> 0.


handle_cast({update, GameState}, State=#state{game_states=GSs}) -> 
	{noreply, State#state{game_states=[GameState | GSs]}};

handle_cast({display, _GameState, none}, State) ->
	{noreply, State#state{moves = [{0, {0,0,0,0}}]}};

handle_cast({display, GameState, Move}, State=#state{board = Board, moves = Moves}) ->
	{noreply, State#state{moves = [{next_player(Board:current_player(GameState)), Move} | Moves]}};

handle_cast({notify, Info}, State=#state{infos = Infos}) ->
%%	io:format("webplayer notify:~p~n", [Info]),
	{noreply, State#state{infos = [ Info | Infos]}};

handle_cast(leave_room, _State) ->
	{noreply, #state{}};	

handle_cast({get_move, From}, State) ->
	{noreply, State#state{player_client = From, is_get_move = true}};	

handle_cast({set_move, Move}, State=#state{player_client = PlayerClient}) ->
	case PlayerClient of
		[] -> ok;
		PlayerClient ->	
			PlayerClient ! {play, Move}
	end,
	{noreply, State}.	

handle_call(get_display_move, State=#state{moves = Moves}) ->
	{reply, {ok, lists:reverse(Moves)}, State#state{moves = []}};


handle_call(get_info, State=#state{infos = Infos}) ->
	{reply, lists:reverse(Infos), State#state{infos = []}};

handle_call(get_legal_moves, State=#state{game_states=[]}) ->
	{reply, {ok, 1, []}, State#state{is_get_move = false}};	

handle_call(get_legal_moves, State=#state{board=Board, game_states=[CurGameState | _GSs]}) ->
	Moves  = Board:legal_moves(CurGameState),
	{reply, {ok, Board:current_player(CurGameState), Moves}, State#state{is_get_move = false}};

handle_call(is_move, State=#state{is_get_move = IsMove}) ->
	{reply, {ok, IsMove}, State};

handle_call(stop, _State) ->
	stop.
	
