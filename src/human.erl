-module(human).
-export([start/1, start/3, update/2, display/3, get_move/1]).

-record(state,  {board = board}).

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

init([Board]) ->
	loop(#state{board = Board}).


loop(State) ->
	receive
		{call, Ref, From, Msg} ->
			case handle_call(Msg, State) of
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


handle_call({update, _GameState}, State) ->
	{reply, ok, State};
handle_call({display, GameState, Move}, State=#state{board=Board}) ->
	io:format("Player move ~p~n", [Move]),
	io:format("~ts~n", [Board:display(GameState, Move)]),
	{reply, ok, State};
handle_call(get_move, State=#state{board=Board}) ->
	P = get_human_move(Board),
	{reply, {ok, P}, State}.

get_human_move(Board) ->
	Move = io:get_line("Your move (R C r c):"),
	case Board:parse(Move) of
		none ->
			get_human_move(Board);
		P ->
			P
	end.