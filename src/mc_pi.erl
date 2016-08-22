-module(mc_pi).
-compile(export_all).

run(N) ->
	<<A:32, B:32, C:32>> = crypto:rand_bytes(12),
	random:seed({A, B, C}),
	TotalPoints = gen_random_points(N),
	InCirclePoints = points_in_circle(TotalPoints),
	4 * length(InCirclePoints) / length(TotalPoints).

gen_random_points(N) ->
	[{random:uniform(), random:uniform()} || _ <- lists:seq(1, N)].

points_in_circle(Points) ->
	[{X, Y} || {X, Y} <- Points, math:sqrt(X*X + Y*Y) < 1.0].