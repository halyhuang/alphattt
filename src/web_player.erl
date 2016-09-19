-module(web_player).
-export([start/1, start/3, update/2, display/3, get_move/1, stop/1]).
-export([get_legal_moves/1, set_move/2, is_move/1, is_display_move/1, get_display_move/1, get_info/1, notify/2]).

-record(state,  {board = board,
			     game_states = [],
			     infos = [],
			     from = none,
			     is_get_move = false,
			     is_display_move = false,
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
	call(Pid, {update, GameState}).

display(Pid, GameState, Move) ->
	call(Pid, {display, GameState, Move}).

get_move(Pid) ->
	call(Pid, get_move).

stop(Pid) ->
	call(Pid, stop).	

set_move(Pid, Move) ->
	call(Pid, {set_move, Move}).

is_move(Pid) ->
	call(Pid, is_move).		

get_legal_moves(Pid) ->
	call(Pid, get_legal_moves).	

is_display_move(Pid) ->
	call(Pid, is_display_move).	

get_display_move(Pid) ->
	call(Pid, get_display_move).	


notify(Pid, Info) ->
	call(Pid, {notify, Info}).

get_info(Pid) ->
	call(Pid, get_info).	

init([Board]) ->
	loop(#state{board = Board, game_states = []}).


loop(State) ->
	receive
		{call, Ref, From, Msg} ->
			case handle_call(Msg, State#state{from = From}) of
				{reply, Reply, NewState} ->
					From ! {Ref, Reply},
					loop(NewState);
				stop ->
					From ! {Ref, stop},
					stop
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
next_player(2) -> 1.

handle_call({update, GameState}, State=#state{game_states=GSs}) -> 
	{reply, ok, State#state{game_states=[GameState | GSs]}};

handle_call({display, _GameState, none}, State) ->
	{reply, ok, State#state{is_display_move = true, moves = []}};

handle_call({display, GameState, Move}, State=#state{board = Board, moves = Moves}) ->
	{reply, ok, State#state{is_display_move = true, moves = [{next_player(Board:current_player(GameState)), Move} | Moves]}};

handle_call(is_display_move, State=#state{is_display_move = IsDisplayMove}) ->
	{reply, {ok, IsDisplayMove}, State};

handle_call(get_display_move, State=#state{moves = Moves}) ->
	{reply, {ok, Moves}, State#state{is_display_move = false}};

handle_call({notify, Info}, State=#state{infos = Infos}) ->
	io:format("webplayer notify:~p~n", [Info]),
	{reply, ok, State#state{infos = [ Info | Infos]}};

handle_call(get_info, State=#state{infos = Infos}) ->
	{reply, lists:reverse(Infos), State#state{infos = []}};

handle_call(get_legal_moves, State=#state{game_states=[]}) ->
	{reply, {ok, 1, []}, State#state{is_get_move = false}};	

handle_call(get_legal_moves, State=#state{board=Board, game_states=[CurGameState | _GSs]}) ->
	Moves  = Board:legal_moves(CurGameState),
	{reply, {ok, Board:current_player(CurGameState), Moves}, State#state{is_get_move = false}};

handle_call(get_move, State=#state{from = From}) ->
	{reply, ok, State#state{player_client = From, is_get_move = true}};

handle_call(is_move, State=#state{is_get_move = IsMove}) ->
	{reply, {ok, IsMove}, State};

handle_call({set_move, Move}, State=#state{player_client = PlayerClient}) ->
	case PlayerClient of
		[] -> ok;
		PlayerClient ->	
			PlayerClient ! {play, Move}
	end,
	{reply, ok, State};

handle_call(stop, _State) ->
	stop.
	
