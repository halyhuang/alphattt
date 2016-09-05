-module(deploy).

-export([run/0]).

run() ->
	db:install(),
	db:start(),
	db_api:add_user("guest", "", human),
	db_api:add_user("mcts", "", robot),
	db_api:add_user("mcts_ucb1", "", robot),
	db_api:add_user("kk", "", human),	
	db_api:add_user("KK", "", human),	
	db_api:add_user("tt", "", human),	
	db_api:add_user("TTT", "", human),	
	db:stop().



			


	









