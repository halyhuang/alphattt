-module(run_test).
-export([run/0]).

run() ->
	code:add_path("D:/erlang/copration/GoodMood/alphattt/alphattt/ebin"),
	ct:run_test([{dir, "."}]).