-module(deploy).

-export([run/0]).

run() ->
	db:install(),
	db:start(),
	db_api:add_user("guest", "", human),
	db_api:add_user("mcts", "", robot),
	db_api:add_user("mcts_ucb1", "", robot),
	db_api:add_user("kk", "123456", human),	
	db_api:add_user("KK", "123456", human),	
	db_api:add_user("tt", "123456", human),	
	db_api:add_user("TT", "123456", human),	
	db_api:add_user("pybot", "", robot),
	db:stop().



			


	









