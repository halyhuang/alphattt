-module(game_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Ranklist = {ranklist, {ranklist,start,[]},
            				permanent,2000,worker,[ranklist]},
    Rommmgr = {room_mgr, {room_mgr,start,[board, 32]},
            				permanent,2000,worker,[room_mgr]},
    Auth = {game_auth, {game_auth,start,[]},
            				permanent,2000,worker,[game_auth]},  
    TcpServer = {tcp_server, {tcp_server, start,[8011, player_agent]},
            				permanent,2000,worker,[tcp_server]},        				          				
    {ok,{{one_for_one, 4, 3600}, [Ranklist, Rommmgr, Auth, TcpServer]}}.
