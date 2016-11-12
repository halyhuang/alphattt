-module(bench).
-export([run/6, run_compete/0]).

run_compete() ->
	MT = 1000,
	EF1 = 0,
	EF2 = 0,
	N = 300,
%	compete(mcts, mcts_pall, MT, 0, 0, 100),
	[ compete(V1, V2, MT, EF2, EF2, N) || V1 <- [mcts, mcts_ucb1], 
	                                      V2 <- [mcts, mcts_ucb1],
	                                     EF2 <- [0.2, 0.3, 0.4, 0.5, 0.6, 1.0], 
	                                     V1 =/= V2],
	ok.

compete(V1, V2, MT, EF1, EF2, N) ->
	{V1, W1, V2, W2} = run(V1, V2, MT, EF1, EF2, N),
	log(V1, V2, MT, EF1, EF2, N, W1, W2).

log(V1, V2, MT, EF1, EF2, N, W1, W2) ->
	{ok, F}=file:open("mcts_complete.txt", [append]),
	io:format(F,"      ~p wins ~p, ~p wins ~p, MT ~p, EF1 ~p, EF2 ~p, total ~p~n", 
				[V1, W1, V2, W2, MT, EF1, EF2, N]),
	file:close(F).

run(V1, V2, MT, EF1, EF2, N) ->
	P1 = V1:start(board, MT, EF1),
	P2 = V2:start(board, MT, EF2),
	{V1, W1, V2, W2} = 
		play_game(0, N,
				{V1, make_player(V1, P1)},
				{V2, make_player(V2, P2)},
				{0, 0}),
	V1:stop(P1),
	V2:stop(P2),
	{V1, W1, V2, W2}.

make_player(V, P) ->
	fun(S) ->
		V:update(P, S),
		{ok, M} = V:get_move(P),
		M
	end.

play_game(N, N, {V1, _}, {V2, _}, {W1, W2}) ->
	show_stat(V1, W1, V2, W2, N),
	io:format("---------------------------------------------------~n"),
	io:format("      ~p wins ~p,~.2f%, ~p wins ~p,~.2f% ~n", [V1, W1, win_rate(W1, N), V2, W2, win_rate(W2, N)]),
	{V1, W1, V2, W2};
play_game(Cnt, N, {V1, P1}, {V2, P2}, {W1, W2}) ->
	show_stat(V1, W1, V2, W2, Cnt),
	InitS = board:start(),
	Winner = play_once(P1, P2, InitS),
	case Winner of
		1 ->
			play_game(Cnt + 1, N, {V1, P1}, {V2, P2}, {W1 + 1, W2});
		2 ->
			play_game(Cnt + 1, N, {V1, P1}, {V2, P2}, {W1, W2 + 1});
		_ ->
			play_game(Cnt + 1, N, {V1, P1}, {V2, P2}, {W1, W2})
	end.

play_once(P1, P2, State) ->
	Move = P1(State),
	NewState = board:next_state(State, Move),
	case board:winner(NewState) of
		on_going ->
			play_once(P2, P1, NewState);
		Winnner ->
			Winnner
	end.

win_rate(_W, 0) -> 0.0;
win_rate(W, Cnt) -> 100 * W / Cnt.

show_stat(V1, W1, V2, W2, Cnt) ->
	io:format("total: ~p, win rate: ~.2f%(~p), ~.2f%(~p)~n",
				[Cnt, win_rate(W1, Cnt), V1, win_rate(W2, Cnt), V2]).

