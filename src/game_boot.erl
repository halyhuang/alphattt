-module(game_boot).

-export([start/0]).

start() ->
    db:start(),
    application:start(game_app).





			


	









