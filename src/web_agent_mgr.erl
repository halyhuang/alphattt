-module(web_agent_mgr).
-behaviour (gen_server).

-include("yaws_api.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0, start_agent/0, get_agent_pid/1, show/0, get_online_users/0, stop/1]).

-record(state,  {agents = [],
				 index = 1}).

%% APIs.

start_link() ->
	{ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [],[]),	
	{ok, Pid}.

start_agent() ->
	gen_server:call(?MODULE, start_agent).	

get_agent_pid(undefined) ->
	{ok, Pid} = start_agent(),
	Pid;
get_agent_pid(PidStr) ->
	Pid = list_to_pid(PidStr),
	case process_info(Pid, messages) of
        undefined ->
        	{ok, NewPid} = start_agent(),
        	NewPid;
        _ ->
        	Pid
    end.

get_online_users() ->
	{ok, OnlineUsers} = gen_server:call(?MODULE, get_online_users),
	OnlineUsers.

show() ->
	OnlineUsers = get_online_users(),
	io:format("~n------------- begin web agent state -----------~n"),	
	[ io:format("id:~p, user:~p, room:~p, status:~p pid:~p~n", [ID, UserName, RoomID, Status, Pid])
		|| {ID, UserName, RoomID, Status, Pid} <- OnlineUsers],
	io:format("~n------------- end web agent state -----------~n"),
	ok.

stop(ID) ->
	gen_server:cast(?MODULE, {stop, ID}).				

init([]) ->
	{ok, #state{}}.

handle_info({'DOWN', _, process, Pid, Reason}, State=#state{agents=Agents}) ->
	case lists:keyfind(Pid, 2, Agents) of
		{_ID, Pid, _Ref} ->
			error_logger:format("web agent ~p down, reason ~p~n", [Pid, Reason]),
			NewAgents = lists:keydelete(Pid, 2, Agents),
		    {noreply, State#state{agents = NewAgents}};
		_ ->
			error_logger:format("web agent ~p down, reason ~p, can't find web agent~n", [Pid, Reason]),
			{noreply, State}
	end.

handle_cast({stop, ID}, State=#state{agents=Agents}) ->
	case lists:keyfind(ID, 1, Agents) of
		{ID, Pid, _Ref} ->
			web_agent:stop(Pid),
			io:format("web agent ~p quit~n", [Pid]),
			NewAgents = lists:keydelete(ID, 1, Agents),
		    {noreply, State#state{agents = NewAgents}};
		_ ->
			io:format("can't find web agent id ~p~n", [ID]),
			{noreply, State}
	end.
	
handle_call(get_online_users, _From, State=#state{agents=Agents}) ->
	WebAgentStatus = [ begin
		{Status, UserName, RoomID, _Player, _WebPlayer, _RobotPlayer} = web_agent:get_state(Pid),
		{ID, UserName, RoomID, Status, Pid}
	   end || {ID, Pid, _Ref} <- Agents],
	{reply, {ok, WebAgentStatus}, State};

handle_call(start_agent, _From, State=#state{agents=Agents, index = Index}) ->
	{ok, Pid} = web_agent:start(),
	Ref = erlang:monitor(process, Pid),
	{reply, {ok, Pid}, State#state{agents = [{Index, Pid, Ref} | Agents], index = Index + 1}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.