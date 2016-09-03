-module(game_server).

-export([start/0]).

start() ->
    roommgr:start(board, 6),
    game_server_auth:start(),
    db:start(),
    tcp_server:start(8011, player_agent).





			


	









