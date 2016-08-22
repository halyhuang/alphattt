-module(webplayer).
-export([start/1, start/3, update/2, display/3, get_move/1]).
-export([get_legal_move/1, set_move/2, is_move/1, get_opponent_move/1]).

-record(state,  {board = board,
			     game_states = [],
			     ref = none,
			     from = none,
			     is_move = false,
			     opponent_move = none,
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

set_move(Pid, Move) ->
	call(Pid, {set_move, Move}).

get_legal_move(Pid) ->
	call(Pid, get_legal_move).	

is_move(Pid) ->
	call(Pid, is_move).		

get_opponent_move(Pid) ->
	call(Pid, get_opponent_move).						

init([Board]) ->
	loop(#state{board = Board, game_states = []}).


loop(State) ->
	receive
		{call, Ref, From, Msg} ->
			case handle_call(Msg, State) of
				{reply, Reply, NewState} ->
					From ! {Ref, Reply},
					loop(NewState);
				{noreply, NewState} ->					
					loop(NewState#state{ref = Ref, from = From});
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


handle_call({update, GameState}, State=#state{game_states=GSs}) -> 
	{reply, ok, State#state{game_states=[GameState | GSs]}};
handle_call({display, GameState, Move}, State=#state{move = Move}) ->
	{reply, ok, State};
handle_call({display, GameState, Move}, State) ->
	{reply, ok, State#state{opponent_move = Move, is_move=true}};
handle_call(get_legal_move, State=#state{board=Board, game_states=[CurGameState | _GSs]}) ->
	Moves  = Board:legal_moves(CurGameState),
	{reply, {ok, Moves}, State};
handle_call(is_move, State=#state{is_move=IsMove}) ->
	{reply, {ok, IsMove}, State};
handle_call(get_opponent_move, State=#state{opponent_move = Move}) ->
	{reply, {ok, Move}, State#state{is_move = false}};	
handle_call({set_move, Move}, State=#state{ref = Ref, from = From}) ->
	case From of
		[] -> ok;
		From ->	
			From ! {Ref, {ok, Move}}
	end,
	{reply, ok, State#state{move = Move}};
handle_call(get_move, State) ->
	{noreply, State}.