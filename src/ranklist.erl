-module(ranklist).
-include("db_table.hrl").


-behaviour (gen_server).

-export([start_link/0, top/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).
%% APIs
start_link() ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

top(practice) ->
	top(practice, 10);
top(game) ->
	top(game, 10).

top(Type, Num) ->
	gen_server:call({global, ?MODULE}, {top, Type, Num}).

init([]) ->
	{ok, #state{}}.

handle_info(_Msg, State) ->
	{noreply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_call({top, Type, Num}, _From, State) ->
	Reply = 
		case Type of
			practice ->
				Scores = [ {score(User), User#user.name, User#user.type} || User <- db_api:get_all_users()],
				lists:sublist(lists:reverse(lists:sort(Scores)), Num);
			game ->
				Scores = [ {score(Record), Record#game_record.name, Record#game_record.type} 
							|| Record <- db_api:get_all_game_records()],
				lists:sublist(lists:reverse(lists:sort(Scores)), Num)
		end,
	{reply, Reply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

score(#user{win=Wins, draw=Draws, total=Totals}) ->
	score(Wins, Draws, Totals);
score(#game_record{win=Wins, draw=Draws, total=Totals}) ->
	score(Wins, Draws, Totals).
score(Wins, Draws, Totals) ->
	Loses = Totals - Wins - Draws,
	Wins * 5 + Draws * 2 + Loses.	