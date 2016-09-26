-module(ybed_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	WebAgentMgr = {web_agent_mgr, {web_agent_mgr,start_link,[]},
            			permanent,2000,worker,[web_agent_mgr]},
	EventStore = {event_store, {event_store,start_link,[]},
            			permanent,2000,worker,[event_store]},            			
    YBed = {ybed, {ybed,start,[]},
            permanent,2000,worker,[ybed]},
    {ok,{{one_for_one, 4, 3600}, [YBed, WebAgentMgr, EventStore]}}.
