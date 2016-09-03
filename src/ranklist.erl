-module(ranklist).
-include("db_table.hrl").

-export([top/1]).

score(#user{win=Wins, draw=Draws, total=Totals}) ->
	Loses = Totals - Wins - Draws,
	Wins * 5 + Draws * 2 + Loses.

top(Num) ->
	Scores = [ {score(User), User#user.name} || User <- db_api:get_all_users()],
	lists:sublist(lists:reverse(lists:sort(Scores)), Num).
