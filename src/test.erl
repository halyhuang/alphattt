-module(test).

-export([add_mcts/0, add_ucb1/0, add_player/4]).

add_player(Name, Password, Type, RoomID) ->
	{ok, Pid} = player_client:start(Name, Type, board, "127.0.0.1", 8011),
	player_client:login(Pid, Password),
	player_client:enter_room(Pid, RoomID),
	Pid.

add_mcts() ->
	add_player("mcts", "", mcts, 1).

add_ucb1() ->
	add_player("mcts_ucb1", "", mcts_ucb1, 1).	



			


	









