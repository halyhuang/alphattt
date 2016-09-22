-module(ranklist).
-include("db_table.hrl").


-behaviour (gen_server).

-export([start_link/0, top/0, top/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).
%% APIs
start_link() ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

top() ->
	top(10).

top(Num) ->
	gen_server:call({global, ?MODULE}, {top, Num}).

init([]) ->
	{ok, #state{}}.

handle_info(_Msg, State) ->
	{noreply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_call({top, Num}, _From, State) ->
	Scores = [ {score(User), User#user.name, User#user.type} || User <- db_api:get_all_users()],
	Reply = lists:sublist(lists:reverse(lists:sort(Scores)), Num),
	{reply, Reply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

score(#user{win=Wins, draw=Draws, total=Totals}) ->
	Loses = Totals - Wins - Draws,
	Wins * 5 + Draws * 2 + Loses.