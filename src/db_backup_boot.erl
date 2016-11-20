-module(db_backup_boot).

-export([start/0]).

start() ->
    application:start(db_backup_app).




