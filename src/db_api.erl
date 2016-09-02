-module(db_api).

-include_lib("stdlib/include/qlc.hrl").
-include("db_table.hrl").

-export([login/2,
			get_user_by_name/1, 
			get_all_users/0,
			register/1,
			register/3,
			unregister/1]).

-export([add_user/1, add_user/3, get_user/1, del_user/1]).

-export([add_game/4, get_all_games/0]).

get_all_users() ->
	do_qeury(qlc:q([X || X <- mnesia:table(user)])).

get_user_by_name(Name) ->
	Users = do_qeury(qlc:q([X || X <- mnesia:table(user), X#user.name =:= Name])),
	case [] =:= Users of
		true -> notexist;
		false -> 
			[H|_T] = Users,
			H
	end.

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


add_user(Name) ->
	add_user(Name, "", human).

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


add_game(Player, Opponent, Result, Steps) ->
	{PlayerResult, OpponentResult} = player_result(Player, Opponent, Result),
	F = fun() ->
			mnesia:write(PlayerResult),
			mnesia:write(OpponentResult),
			mnesia:write(#game{player = Player, opponent = Opponent, 
					time = calendar:local_time(), 
					result = Result, steps = Steps})
		end,
	mnesia:transaction(F).

get_all_games() ->
	do_qeury(qlc:q([X || X <- mnesia:table(game)])).

%% true | {false reason}
login(UserName, Password) ->
	case get_user_by_name(UserName) of
		notexist -> {false, user_name_not_exist};
		#user{name=UserName, password=UserPassword} -> 
			case Password =:= UserPassword of
				true -> true;
				false -> {false, password_not_right}
			end
	end.

%% true | {false, Reason}
register(User=#user{name=Name}) -> 
	case get_user_by_name(Name) of
		notexist -> add_record(User), true;
		_ -> {false, user_aready_exist}
	end. 

register(Name, Password, Type) ->
	User = #user{name=Name, password=Password, type=Type},
	register(User).

unregister(Name) ->
	del_record(Name).

do_qeury(Query) ->
	F = fun() -> qlc:e(Query) end,
	{atomic, Val} = mnesia:transaction(F),
	Val.

add_record(User) ->
	F = fun() ->
			mnesia:write(User)
		end,
	mnesia:transaction(F).

del_record(UserName) ->
	Oid = {user, UserName},
	F = fun() ->
			mnesia:delete(Oid)
		end,
	mnesia:transaction(F).

