-module(db_backup).

-behaviour (gen_server).

%%APIs
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([get_symbol_sec/0, get_symbol/0]).


start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% use time to acitve backup 1 day time
init([]) ->
	mnesia:start(),
	io:format("init a timer~n"),
	timer:send_interval(24 * 60 * 60 * 1000, db_backup).


handle_call(_Msg, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

%% back up db each 24 hours
handle_info(db_backup, State) ->
	io:format("receive message db_backup~n"),
	Symbol = get_symbol(),
	db:backup(Symbol),
	{noreply, State}.


get_symbol_sec() ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
	list_to_atom("backup" ++ integer_to_list((Year * 10000 + Month*100 + Day)*1000000 + 
											  (Hour*10000 + Minute*100 + Second))).
get_symbol() ->
	{{Year, Month, Day}, _} = calendar:local_time(),
	list_to_atom("backup" ++ integer_to_list(Year * 10000 + Month*100 + Day)).

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}. 
