-module(db_backup_app).

-behavior(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    db_backup_sup:start_link().


stop(_State) ->
	ok.
