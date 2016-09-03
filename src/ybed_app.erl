-module(ybed_app).
-behavior(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    ybed_sup:start_link().


stop(_State) ->
	ok.





			


	









