-module(web_agent_mgr).
-behaviour (gen_server).

-include("yaws_api.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/0, start_agent/0, get_agent_pid/1, show/0, stop/1]).

-record(state,  {agents = [],
				 index = 1}).

%% APIs.

start() ->
	{ok, Pid} = gen_server:start({local, ?MODULE}, ?MODULE, [],[]),	
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

show() ->
	gen_server:cast(?MODULE, show).		

stop(ID) ->
	gen_server:cast(?MODULE, {stop, ID}).				

init([]) ->
	{ok, #state{}}.

handle_info({'DOWN', _, process, Pid, Reason}, State=#state{agents=Agents}) ->
	case lists:keyfind(Pid, 2, Agents) of
		{_ID, Pid, _Ref} ->
			io:format("web agent ~p down, reason ~p~n", [Pid, Reason]),
			NewAgents = lists:keydelete(Pid, 2, Agents),
		    {noreply, State#state{agents = NewAgents}};
		_ ->
			io:format("web agent ~p down, reason ~p, can't find web agent~n", [Pid, Reason]),
			{noreply, State}
	end.

handle_cast(show, State=#state{agents=Agents}) ->
	io:format("~n------------- begin web agent state -----------~n"),	
	[ begin
		{ok, WebAgetState} = web_agent:show(Pid),
		io:format("id:~p, state:~p~n", [ID, WebAgetState])
	   end || {ID, Pid, _Ref} <- Agents],
	io:format("~n------------- end web agent state -----------~n"),	
	{noreply, State};

handle_cast({stop, ID}, State=#state{agents=Agents}) ->
	case lists:keyfind(ID, 1, Agents) of
		{ID, Pid, _Ref} ->
			web_agent:stop(),
			io:format("web agent ~p quit~n", [Pid]),
			NewAgents = lists:keydelete(ID, 1, Agents),
		    {noreply, State#state{agents = NewAgents}};
		_ ->
			io:format("can't find web agent id ~p~n", [ID]),
			{noreply, State}
	end.
	

handle_call(start_agent, _From, State=#state{agents=Agents, index = Index}) ->
	{ok, Pid} = web_agent:start(),
	Ref = erlang:monitor(process, Pid),
	{reply, {ok, Pid}, State#state{agents = [{Index, Pid, Ref} | Agents], index = Index + 1}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.