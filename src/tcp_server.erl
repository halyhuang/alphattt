-module(tcp_server).
-behaviour(gen_server).

%%APIs
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {port, lsock, agent}).

%%APIs

start_link(LPort, Agent) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [LPort, Agent], []).

init([LPort, Agent]) ->
	case gen_tcp:listen(LPort, [binary, {packet, 2}, {active, true}]) of
		{ok, LSock} ->
			spawn_acceptor(LSock, Agent, 20),
			io:format("tcp_server started @ [~p]~n", [LPort]),
			{ok, #state{port=LPort, lsock=LSock, agent=Agent}};
		{error, Reason} ->
			io:format("tcp_server not started, reason:~p~n", [Reason]),
			{stop, Reason}
	end.

spawn_acceptor(LSock, Agent, Num) ->
	[tcp_acceptor:start(LSock, Agent) || _ <-lists:seq(1, Num)].

handle_call(_Msg, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}. 