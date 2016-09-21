-module(db_api).

-include_lib("stdlib/include/qlc.hrl").
-include("db_table.hrl").

-export([add_user/3, get_user/1, get_all_users/0, del_user/1, get_user_type/1]).

-export([add_game/4, get_all_games/0]).

get_all_users() ->
	do_qeury(qlc:q([X || X <- mnesia:table(user)])).

get_user(Name) ->	
	do_qeury(qlc:q([X || X <- mnesia:table(user), X#user.name =:= Name])).

player_result(Player, Opponent, draw) ->
	[PlayerResult]   = get_user(Player),
	[OpponentResult] = get_user(Opponent),
	{PlayerResult#user{draw   = PlayerResult#user.draw + 1, 
					   total  = PlayerResult#user.total + 1},
	 OpponentResult#user{draw = OpponentResult#user.draw + 1, 
					   total  = OpponentResult#user.total + 1}};
player_result(Player, Opponent, Player) ->
	[PlayerResult]   = get_user(Player),
	[OpponentResult] = get_user(Opponent),
	{PlayerResult#user{win   = PlayerResult#user.win + 1, 
					   total = PlayerResult#user.total + 1},
	 OpponentResult#user{total = OpponentResult#user.total + 1}}.

add_user(Name, Password, Type) ->
	F = fun() ->
		mnesia:write(#user{name=Name, password=Password, type=Type})
	end,
	mnesia:transaction(F).

del_user(Name) ->
	F = fun() ->
			mnesia:delete({user, Name})
		end,
	mnesia:transaction(F).	

get_user_type(Name) ->
	case get_user(Name) of
		[] -> "none";		
		[User] -> User#user.type
	end.

add_game(Player, Opponent, Result, Steps) ->
	F = fun() ->
			{PlayerResult, OpponentResult} = player_result(Player, Opponent, Result),
			mnesia:write(PlayerResult),
			mnesia:write(OpponentResult),
			mnesia:write(#game{player = Player, opponent = Opponent, 
					time = now(), 
					result = Result, steps = Steps})
		end,
	mnesia:transaction(F).

get_all_games() ->
	do_qeury(qlc:q([X || X <- mnesia:table(game)])).

do_qeury(Query) ->
	F = fun() -> qlc:e(Query) end,
	{atomic, Val} = mnesia:transaction(F),
	Val.


