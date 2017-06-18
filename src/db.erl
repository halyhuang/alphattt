-module(db).
-include("db_table.hrl").

-export([create_table/1, create_table/2,
		 delete_table/1, delete_table/2,
		 install/0, install/1,
		 uninstall/0, uninstall/1,
		 start/0,
		 stop/0, stop/1,
		 backup/1,
		 restore/1, restore/2]).

%% master node and backup node
get_nodes() ->
	[gameserver@localhost, dbbackup@localhost].

%% backup to local, the backup symbol must be uniqueness
backup(BackupSymbol) ->
	mnesia:start(),
	R = mnesia:backup(BackupSymbol),
	io:format("backup ~p ~p ~n", [BackupSymbol, R]).

%% recover data to db
restore(BackupSymbol) ->
	restore([node()], BackupSymbol).
restore(backup, BackupSymbol) ->
	restore(get_nodes(), BackupSymbol);
restore(Nodes, BackupSymbol) ->
	rpc:multicall(Nodes, mnesia, start, []),  % start mnesia on master and backup node, or restore will failed.
	R = mnesia:restore(BackupSymbol, []),
	io:format("restore ~p ~p~n", [BackupSymbol, R]).

%% the server must start database when boot
start() ->
	mnesia:start(),   % only start mnesia on master, becuase there will be time slap
	mnesia:info(),
	mnesia:wait_for_tables([user, game_record, game], 2000).

stop() ->
	stop([node()]).
stop(backup) ->
	stop(get_nodes());
stop(Nodes) ->
	rpc:multicall(Nodes, mnesia, stop, []). % stop mnesia on master and backup node

%% create database table, excute only once by manu
install() ->
	install([node()]).
install(backup) ->
	install(get_nodes());
install(Nodes) ->
	install_database(Nodes),
	create_table(user, Nodes),
	create_table(game_record, Nodes),
	create_table(game, Nodes).

install_database(Nodes) ->
	case mnesia:create_schema(Nodes) of
		ok -> io:format("database ~p has been create ~n", [Nodes]);
		{error, Reason} -> io:format("create database failed beacause ~p~n", [Reason])
	end.

%% uninstall db
uninstall() ->
	uninstall([node()]).
uninstall(backup) ->
	uninstall(get_nodes());
uninstall(Nodes) ->
	rpc:multicall(Nodes, mnesia, stop, []), % stop mnesia on master and backup node
	mnesia:delete_schema(Nodes).            % delete schema in master  and backup node


table_fileds(user) -> record_info(fields, user);
table_fileds(game_record) -> record_info(fields, game_record);
table_fileds(game) -> record_info(fields, game).


create_table(TableName) ->
	create_table(TableName, [node()]).
create_table(TableName, backup) ->
	create_table(TableName, get_nodes());
create_table(TableName, Nodes) ->
	rpc:multicall(Nodes, mnesia, start, []),  % start mnesia on master and backup node
	case mnesia:create_table(TableName, 
		[{disc_copies, Nodes}, {attributes, table_fileds(TableName)}]) of
		{atomic, ok} -> io:format("create table ~p with fileds ~p successfully on ~p.~n", [TableName, table_fileds(TableName), Nodes]);
		{aborted, Reason} -> io:format("create table failed beacause ~p~n", [Reason])
	end.

delete_table(TableName) ->
	delete_table(TableName, [node()]).
delete_table(TableName, backup) ->
	delete_table(TableName, get_nodes());
delete_table(TableName, Nodes) ->
	rpc:multicall(Nodes, mnesia, start, []),  % start mnesia on master and backup node
	case mnesia:delete_table(TableName) of
		{atomic, ok} -> io:format("delete table ~p successfully.~n", [TableName]);
		{aborted, Reason} -> io:format("delete table failed beacause ~p~n", [Reason])
	end,
	rpc:multicall(Nodes, mnesia, stop, []). % stop mnesia on master and backup node
