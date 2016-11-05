-module(db).
-include("db_table.hrl").

-export([create_table/1,
		 delete_table/1,
		 install/0,
		 start/0,
		 stop/0]).

%% the server must start database when boot
start() ->
	mnesia:start(),
	mnesia:info(),
	mnesia:wait_for_tables([user, game_record, game], 2000).

stop() ->
	mnesia:stop().

%% create database table, excute only once
install() ->
	install_database(),
	create_table(user),
	create_table(game_record),
	create_table(game).


table_fileds(user) -> record_info(fields, user);
table_fileds(game_record) -> record_info(fields, game_record);
table_fileds(game) -> record_info(fields, game).

install_database() ->
	case mnesia:create_schema([node()]) of
		ok -> io:format("database ~p has been create ~n", [node()]);
		{error, Reason} -> io:format("create database failed beacause ~p~n", [Reason])
	end.

create_table(TableName) ->
	mnesia:start(),
	case mnesia:create_table(TableName, 
		[{disc_copies, [node()]}, {attributes, table_fileds(TableName)}]) of
		{atomic, ok} -> io:format("create table ~p with fileds ~p successfully.~n", [TableName, table_fileds(TableName)]);
		{aborted, Reason} -> io:format("create table failed beacause ~p~n", [Reason])
	end,
	mnesia:stop().		

delete_table(TableName) ->
	mnesia:start(),
	case mnesia:delete_table(TableName) of
		{atomic, ok} -> io:format("delete table ~p successfully.~n", [TableName]);
		{aborted, Reason} -> io:format("delete table failed beacause ~p~n", [Reason])
	end,
	mnesia:stop().	
