-module(game_auth).
-include("db_table.hrl").

-behaviour (gen_server).

-export([start_link/0, login/2, register/1, register/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

start_link() ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

login(UserName, Password) ->
	gen_server:call({global, ?MODULE}, {login, UserName, Password}).

register(UserName) ->
	register(UserName, "", unknown).

register(UserName, Password, Type) ->
	gen_server:call({global, ?MODULE}, {register, UserName, Password, Type}).

init([]) ->
	{ok, #state{}}.

handle_info(_Msg, State) ->
	{noreply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_call({login, UserName, Password}, _From, State) ->
	Reply = case db_api:get_user(UserName) of
		[] -> "user not register";
		[#user{name=UserName, password=Password}] ->
			ok;
		_ ->
			"password isn't match"
	end,
	{reply, Reply, State};
handle_call({register, UserName, Password, Type}, _From, State) ->	
	Reply = case db_api:get_user(UserName) of
		[] -> 
			db_api:add_user(UserName, Password, Type),
			ok;		
		_ ->
			{error, user_already_register}
	end,
	{reply, Reply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



			


	









