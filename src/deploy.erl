-module(deploy).

-export([run/0, run_backup/0]).

run() ->
	Symbol = db_backup:get_symbol_sec(),
	db:start(),
	db:backup(Symbol),   % backup data
	db:uninstall(),  % uninstall db to create master and backup db
	db:install(),    % create master and backup db
	db:restore(Symbol),    % restore data
	add_default_users().

run_backup() ->
	Symbol = db_backup:get_symbol_sec(),
	db:start(),
	db:backup(Symbol),   % backup data
	db:uninstall(backup),  % uninstall db to create master and backup db
	db:install(backup),    % create master and backup db
	db:restore(backup, Symbol),    % restore data
	add_default_users().


add_default_users() ->
	io:format("add all users~n"),
	db_api:add_user("guest", "", human),
	db_api:add_user("mcts", "", robot),
	db_api:add_user("mcts_pall", "", robot),		
	db_api:add_user("mcts_ucb1", "", robot),
	db_api:add_user("mcts_ucb1_pall", "", robot),	
	db_api:add_user("mcts_value", "", robot),	
	db_api:add_user("kk", "123456", human),	
	db_api:add_user("KK", "123456", human),	
	db_api:add_user("tt", "123456", human),	
	db_api:add_user("TT", "123456", human),	
	db_api:add_user("pybot", "", robot),
	ok.






			


	









