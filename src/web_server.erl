-module(web_server).

-export([start/0]).

start() ->
    ybed_sup:start_link(),
    timer:sleep(3000),
    ybed:start().





			


	









