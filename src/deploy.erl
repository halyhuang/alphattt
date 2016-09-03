-module(deploy).

-export([run/0]).

run() ->
	db_api:add_user("mcts", "", robot),
	db_api:add_user("mcts_ucb1", "", robot).	



			


	









