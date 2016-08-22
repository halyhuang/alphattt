-module(test_ets).
-compile(export_all).

create_table() ->
	ets:new(ets_name, [set, named_table]).


test() ->	
	ets:insert(ets_name, {1, 'A'}),
	ets:insert(ets_name, {2, 'B'}),
	ets:insert(ets_name, {3, 'C'}),
	ets:insert(ets_name, {2, 'BB'}),
	ets:lookup(ets_name, 2).

test_ets_file() ->
	spawn(fun() -> init() end).

init() ->
	ets:new(ets_name, [set, named_table, public]),
	ets:insert(ets_name, {1, 'A'}),
	ets:insert(ets_name, {2, 'B'}),
	ets:insert(ets_name, {3, 'C'}),
	ets:tab2file(ets_name, "test.ets").

test_ets_load() ->
	spawn(fun() -> loop() end).

loop() ->
	ets:fil2tab("test.ets"),
	receive
		stop ->
			stop
	end.

