-module(game).

-export([start/0]).

start() ->
    roommgr:start(board, 5),
    db:start(),
    tcp_server:start(8011, player_agent).





			


	









