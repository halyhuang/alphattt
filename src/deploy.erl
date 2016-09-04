-module(deploy).

-export([run/0]).

run() ->
	db:install(),
	db:start(),
	db_api:add_user("mcts", "", robot),
	db_api:add_user("mcts_ucb1", "", robot),
	db:stop(),
	halt().	



			


	









