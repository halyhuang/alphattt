-module(web_server).

-export([start/0]).

start() ->
    ybed_sup:start_link(),
    ybed:start().





			


	









