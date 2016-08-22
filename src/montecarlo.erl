-module(montecarlo).
-compile(export_all).

test1() ->
	X = random:uniform(),
	Y = random:uniform(),	
	R = X * X + Y * Y,
	R =< 1.

test() ->
	N = 300000, 	
	(length(lists:filter(fun(X) -> X end, [ test1() || _X <- lists:seq(1, N)])) / N) * 4.
