-module(board1_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Test server callbacks
-export([suite/0, all/0, groups/0,
		init_per_suite/1, end_per_suite/1,
		init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([check_board/1]).

suite() ->
	[].

init_per_suite(Config) ->
	Config.

end_per_suite(_Config) ->
	ok.

init_per_testcase(_TestCase, Config) ->
	Config.

end_per_testcase(_TestCase, _Config) ->
	ok.

all() -> 
	[{group, check}].

groups() ->
	[{check, [parallel , {repeat_until_any_fail, 10}], [check_board]}].

init() ->
	<<A:32, B:32, C:32>> = crypto:rand_bytes(12),
	random:seed({A, B, C}).

%% check board1 output is equal to board 
check_board(_Config) ->
	io:format("run check board ~n"),

	% test start
	init(),

	% check output of start 
	StateBoard = board:start(),
	StateBoard = board1:start(),

	going_on(StateBoard).

%% simulate the process of chess and compare the output
going_on(StateBoard) ->	
	% check output of winner
	Winner = board:winner(StateBoard),
	Winner = board1:winner(StateBoard),

	if 
		Winner =:= 1 -> 1;
		Winner =:= 2 -> 2;
		Winner =:= draw -> draw;
		Winner =:= on_going -> 
			% check output of leagl_moves
			LegalMoves = board:legal_moves(StateBoard),
			LegalMoves = board1:legal_moves(StateBoard),

			case [] =:= LegalMoves of
				true -> empty_legal_move;
				false -> 
					% random get move
					Random = random:uniform(length(LegalMoves)),
					Move = lists:nth(Random, LegalMoves),

					%check output of is_legal
					Is_Legal = board:is_legal(StateBoard, Move),
					Is_Legal = board1:is_legal(StateBoard, Move),

					% check output of next_state
					NextState = board:next_state(StateBoard, Move),	
					NextState = board1:next_state(StateBoard, Move),			

					going_on(NextState)
			end
	end.	
