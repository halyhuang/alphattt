nohup erl -boot start_sasl -config server.config -env ERL_LIBS ../erlport -sname gameserver@localhost -setcookie alphattt -detached -s game_boot start -s test connect &