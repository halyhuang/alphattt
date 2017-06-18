-module(db_backup_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    DBBackup = {db_backup, {db_backup,start_link,[]},
            				permanent,2000,worker,[db_backup]},       				          				
    {ok,{{one_for_one, 4, 3600}, [DBBackup]}}.

