-module(board1).
-export([start/0, is_legal/2, next_state/2, 
		winner/1, display/1, max_moves/0, 
		current_player/1, legal_moves/1]).

%% inline optimize
%% contain if not inline,there will be 2 times cosume.
-compile({inline,[contain/2]}).


start() -> {0, 0, 0, 0, 0, 0, 0, 0, 0, 
			0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 
			none, none, 1}.

%% API
%% is_legal
check_valid_move({LR, LC, R, C}) -> 
	(LR < 3 andalso LR >= 0) andalso
	(LC < 3 andalso LC >= 0) andalso
	(R < 3 andalso R >= 0) andalso
	(C < 3 andalso C >= 0).

is_legal(GameState, Move) -> 
	case check_valid_move(Move) of
		false -> false;
		true -> 
			Moves = legal_moves(GameState),
			lists:member(Move, Moves)
	end.

%% next_state
next_state(GameState, Move) -> 
	case check_valid_move(Move) of
		false -> GameState;
		true -> 
			CurrentPlayer = current_player(GameState),
			update_last_move_rc( 
					update_player_subchessboard_win_state(CurrentPlayer, 
						update_player_subchessboard_positions(CurrentPlayer, 
							update_player(GameState), Move), Move), Move)
	end.

%% add move to postions
update_player_subchessboard_positions(CurrentPlayer, GameState, {LR, LC, R, C}) ->
	OldPositions = get_player_subchessboard_positions(CurrentPlayer, GameState, LR, LC),
	NewPositions = OldPositions bor grid_position(R, C),
	update_player_subchessboard_positions(CurrentPlayer, GameState, LR, LC, NewPositions).

%% update states if subchessboard win
%% include situation of win and draw
update_player_subchessboard_win_state(CurrentPlayer, GameState, {LR, LC, _R, _C}) ->
	SubchessboardPositions = get_player_subchessboard_positions(CurrentPlayer, GameState, LR, LC),
	case is_subchessboard_win(SubchessboardPositions) of
		true -> 
			update_player_subchessboard_win_state(CurrentPlayer, GameState, LR, LC);
		false ->
			case is_subchessboard_full(GameState, LR, LC) of
				true ->  %draw
					update_player_subchessboard_win_state(next_player(CurrentPlayer), 
						update_player_subchessboard_win_state(CurrentPlayer, 
							GameState, LR, LC), LR, LC);
				false ->
					GameState
			end
	end.	

update_player_subchessboard_win_state(CurrentPlayer, GameState, LR, LC) ->
	OldWinState = get_player_subchessboard_win_states(CurrentPlayer, GameState),
	NewWinState = OldWinState bor grid_position(LR, LC),
	update_subchessboard_win_states(GameState, CurrentPlayer, NewWinState).

%% update last postions with new given rc, if this subchessboard is win or full, set none none
update_last_move_rc(GameState, {_, _, R, C}) ->
	case is_legal_subchessboard(GameState, R, C) of
		false -> update_last_move_rc(GameState, none, none);
		true -> update_last_move_rc(GameState, R, C)
	end.

%% winner
winner(GameState) -> 
	{Player1WinStates, Player2WinStates} = get_player_subchessboard_win_states(GameState),
	Is_player1_win = is_game_win(Player1WinStates, Player2WinStates),
	Is_player2_win = is_game_win(Player2WinStates, Player1WinStates),			
	get_winner(Is_player1_win, Is_player2_win, Player1WinStates, Player2WinStates).

get_winner(Is_player1_win, _, _, _) when true =:= Is_player1_win -> 1;
get_winner(_, Is_player2_win, _, _) when true =:= Is_player2_win -> 2;
get_winner(_, _, StateOfPlayer1, StateOfPlayer2) ->
	case is_game_draw(StateOfPlayer1, StateOfPlayer2) of
		true -> draw;
		false -> on_going
	end.

display(_GameState) -> {}.

max_moves() -> 81.

current_player(GameState) -> get_current_player(GameState).

legal_moves(GameState) -> 
	LegalRS = get_legal_subchessboard_coordinates(GameState),
	lists:foldl(fun(X, Acc) -> Acc ++ get_legal_moves(X, GameState) end, [], LegalRS).

subchessboard_coordinates() ->
	[{0, 0}, {0, 1}, {0, 2}, {1, 0}, {1, 1}, {1, 2}, {2, 0}, {2, 1}, {2, 2}].

get_legal_subchessboard_coordinates(GameState) -> 
	case get_last_move_rc(GameState) of 
		{none, none} ->
			lists:filter(fun({R, C}) ->
							is_legal_subchessboard(GameState, R, C)
						 end, subchessboard_coordinates());
		{LR, LC} ->	[{LR, LC}]
	end.

is_legal_subchessboard(GameState, R, C) ->
	case is_subchessboard_aready_win(1, GameState, R, C) orelse
		 is_subchessboard_aready_win(2, GameState, R, C) of
		 true  -> false;
	     false -> not is_subchessboard_full(GameState, R, C)
	end.



get_legal_moves({LR, LC}, GameState) ->
	SubchessboardPositions = 
		get_player_subchessboard_positions(1, GameState, LR, LC) +
		get_player_subchessboard_positions(2, GameState, LR, LC),
	lists:foldl(fun({R, C}, Y) ->
					case is_legal_position(SubchessboardPositions, {R, C}) of
						true -> Y ++ [{LR, LC, R, C}];
						false -> Y
					end
				end, [], subchessboard_coordinates()).

%% inner fuction
%% operate tuple
get_element(N, GameState) ->
	erlang:element(N, GameState).
set_element(N, NewValue, GameState) ->
	erlang:setelement(N, GameState, NewValue).

%% tuple positions
subchessboard(0, 0) -> 1;
subchessboard(0, 1) -> 3;
subchessboard(0, 2) -> 5;
subchessboard(1, 0) -> 7;
subchessboard(1, 1) -> 9;
subchessboard(1, 2) -> 11;
subchessboard(2, 0) -> 13;
subchessboard(2, 1) -> 15;
subchessboard(2, 2) -> 17.
subchessboard(1, R, C) -> subchessboard(R, C);
subchessboard(2, R, C) -> subchessboard(R, C) + 1.

get_pos_player1_state() -> 19.
get_pos_player2_state() -> 20.
get_pos_last_move_r() -> 21.
get_pos_last_move_c() -> 22.
get_pos_player() -> 23.

% tuple element getter and setter
get_current_player(GameState) ->
	get_element(get_pos_player(), GameState).


get_player_subchessboard_positions(Player, GameState, LR, LC) ->
	get_element(subchessboard(Player, LR, LC), GameState).

get_player_subchessboard_win_states(1, GameState) ->
	get_element(get_pos_player1_state(), GameState);
get_player_subchessboard_win_states(2, GameState) ->
	get_element(get_pos_player2_state(), GameState).

get_player_subchessboard_win_states(GameState) ->
	{get_player_subchessboard_win_states(1, GameState), 
	 get_player_subchessboard_win_states(2, GameState)}.

get_last_move_rc(GameState) -> 
	{get_element(get_pos_last_move_r(), GameState), 
	 get_element(get_pos_last_move_c(), GameState)}.

next_player(1) -> 2;
next_player(2) -> 1.

update_player(GameState) ->
	CurrentPlayer = get_current_player(GameState),
	NextPlayer = next_player(CurrentPlayer),
	set_element(get_pos_player(), NextPlayer, GameState).

update_last_move_rc(GameState, NewR, NewC) ->
	set_element(get_pos_last_move_c(), NewC, set_element(get_pos_last_move_r(), NewR, GameState)).

update_subchessboard_win_states(GameState, 1, NewState) ->
	set_element(get_pos_player1_state(), NewState, GameState);
update_subchessboard_win_states(GameState, 2, NewState) ->
	set_element(get_pos_player2_state(), NewState, GameState).


update_player_subchessboard_positions(CurrentPlayer, GameState, LR, LC, NewPositions) ->
	set_element(subchessboard(CurrentPlayer, LR, LC), NewPositions, GameState).


%% chessboard operate
%% 1 2 4 8 16 32 64 128 256
% grid_position(R, C) ->
% 	1 bsl (subchessboard(R, C) - 1).	
grid_position(0, 0) -> 1;
grid_position(0, 1) -> 2;
grid_position(0, 2) -> 4;
grid_position(1, 0) -> 8;
grid_position(1, 1) -> 16;
grid_position(1, 2) -> 32;
grid_position(2, 0) -> 64;
grid_position(2, 1) -> 128;
grid_position(2, 2) -> 256.

is_win(Value) ->
	contain(Value, 7) orelse
	contain(Value, 56) orelse
	contain(Value, 448) orelse
	contain(Value, 73) orelse
	contain(Value, 146) orelse
	contain(Value, 292) orelse
	contain(Value, 84) orelse
	contain(Value, 273).

contain(Value, SpecificWinValue) ->
	(SpecificWinValue band Value) =:= SpecificWinValue.

is_subchessboard_full(Positions) ->
	contain(Positions, 511).
is_subchessboard_full(GameState, LR, LC) ->
	is_subchessboard_full(
		get_player_subchessboard_positions(1, GameState, LR, LC) bor 
		get_player_subchessboard_positions(2, GameState, LR, LC)).

is_subchessboard_win(Positions) ->
	is_win(Positions).

is_legal_position(Positions, {R, C}) ->
	not contain(Positions, grid_position(R, C)).

is_subchessboard_aready_win(GridState, LR, LC) ->
	contain(GridState, grid_position(LR, LC)).

is_subchessboard_aready_win(Player, GameState, LR, LC) ->
	States = get_player_subchessboard_win_states(Player, GameState),
	is_subchessboard_aready_win(States, LR, LC).

is_game_win(State1, State2) ->
	State = remove_draw_state(State1, State2),
	is_win(State).

is_game_draw(State1, State2) ->
	State1 bor State2 =:= 511.

remove_draw_state(State1, State2) ->
	State1 bxor (State1 band State2).





