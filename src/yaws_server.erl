-module(yaws_server).

-export([start/0]).

start() ->
	Id = "yaws_server",
	GconfList = [{logdir, "E:/10035938/github/alphattt/logs"},
	             {ebin_dir, ["E:/10035938/github/alphattt/ebin"]},
	             {id, Id}],
	Docroot = "E:/10035938/github/alphattt/www",
	SconfList = [{docroot, Docroot},
	             {port, 80},
	             {listen, {127,0,0,1}},
	             {appmods, [{"/", my_appmod}]}],
	{ok, SCList, GC, _ChildSpecs} =
    yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id).


	









