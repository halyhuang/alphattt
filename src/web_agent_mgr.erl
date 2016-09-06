-module(web_agent_mgr).
-behaviour (gen_server).

-include("yaws_api.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/0, start_agent/0, get_agent_pid/1, show/0]).

-record(state,  {agents = []}).

%% APIs.

start() ->
	{ok, Pid} = gen_server:start({local, ?MODULE}, ?MODULE, [],[]),	
	{ok, Pid}.

start_agent() ->
	gen_server:call(?MODULE, start_agent).	

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

init([]) ->
	{ok, #state{}}.

handle_info({'DOWN', _, process, Pid, Reason}, State=#state{agents=Agents}) ->
	case lists:keyfind(Pid, 1, Agents) of
		{Pid, _Ref} ->
			io:format("web agent ~p down, reason ~p~n", [Pid, Reason]),
			NewAgents = lists:keydelete(Pid, 1, Agents),
		    {noreply, State#state{agents = NewAgents}};
		_ ->
			io:format("web agent ~p down, reason ~p, can't find web agent~n", [Pid, Reason]),
			{noreply, State}
	end.

handle_cast(show, State=#state{agents=Agents}) ->
	io:format("web agents:~n~p~n", [Agents]),
	{noreply, State}.

handle_call(start_agent, _From, State=#state{agents=Agents}) ->
	{ok, Pid} = web_agent:start(),
	Ref = erlang:monitor(process, Pid),
	{reply, {ok, Pid}, State#state{agents = [{Pid, Ref} | Agents]}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.