-module(game).

-export([start/0]).

start() ->
	{ok, _Pid} = tcp_server:start(8011, player_agent),
	roommgr:start(board, 5).





			


	









