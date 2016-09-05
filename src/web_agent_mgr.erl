-module(web_agent_mgr).
-behaviour (gen_server).

-include("yaws_api.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/0, get_agent/1, show/0]).

-record(state,  {agents = []}).

%% APIs.

start() ->
	{ok, Pid} = gen_server:start({local, ?MODULE}, ?MODULE, [],[]),	
	{ok, Pid}.

get_agent(A) ->
    {ok, Peer} = case yaws_api:get_sslsocket(A#arg.clisock) of
               {ok, SslSocket} ->
                   ssl:peername(SslSocket);
               _ ->
                   inet:peername(A#arg.clisock)
           end,
	get_agent_pid(Peer).

get_agent_pid(Peer) ->
	gen_server:call(?MODULE, {get_agent_pid, Peer}).		

show() ->
	gen_server:cast(?MODULE, show).		

init([]) ->
	{ok, #state{}}.

handle_info({'DOWN', _, process, Pid, Reason}, State=#state{agents=Agents}) ->
	case lists:keyfind(Pid, 2, Agents) of
		{Peer, Pid, _Ref} ->
			io:format("web agent ~p down, reason ~p~n", [Peer, Reason]),
			NewAgents = lists:keydelete(Pid, 2, Agents),
		    {noreply, State#state{agents = NewAgents}};
		_ ->
			io:format("web agent ~p down, reason ~p, can't find web agent~n", [Pid, Reason]),
			{noreply, State}
	end.

handle_cast(show, State=#state{agents=Agents}) ->
	io:format("web agents:~n~p~n", [Agents]),
	{noreply, State}.

handle_call({get_agent_pid, Peer}, _From, State=#state{agents=Agents}) ->
	case lists:keyfind(Peer, 1, Agents) of
		{Peer, Pid, _Ref} ->
			{reply, Pid, State};
		_ ->
			{ok, Pid} = web_agent:start(),
			Ref = erlang:monitor(process, Pid),
			{reply, Pid, State#state{agents = [{Peer, Pid, Ref} | Agents]}}
	end.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.