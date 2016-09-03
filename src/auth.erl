-module(auth).
-include("db_table.hrl").
-export([login/2]).

login(UserName, Password) ->
	case db_api:get_user(UserName) of
		[] -> {error, user_not_exist};
		[#user{name=UserName, password=Password}] ->
			ok;
		_ ->
			{error, password_not_match}
	end.	



			


	









