-module(game_auth).
-include("db_table.hrl").

-behaviour (gen_server).

-export([start/0, login/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

start() ->
	gen_server:start({global, ?MODULE}, ?MODULE, [], []).

login(UserName, Password) ->
	gen_server:call({global, ?MODULE}, {login, UserName, Password}).

init([]) ->
	{ok, #state{}}.

handle_info(_Msg, State) ->
	{noreply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_call({login, UserName, Password}, _From, State) ->
	Reply = case db_api:get_user(UserName) of
		[] -> {error, user_not_exist};
		[#user{name=UserName, password=Password}] ->
			ok;
		_ ->
			{error, password_not_match}
	end,
	{reply, Reply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



			


	









