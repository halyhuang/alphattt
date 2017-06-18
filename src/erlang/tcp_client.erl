-module(tcp_client).
-export([start/2, echo/2]).

-record(state, {socket}).

start(SIp, SPort) ->
	Pid = spawn(fun() -> init(SIp, SPort) end),
	{ok, Pid}.

echo(Pid, Msg) ->
	Pid ! {echo, Msg},
	ok.

init(SIp, SPort) ->
	io:format("connect to ~p~n", [{SIp, SPort}]),
	{ok, Sock} = gen_tcp:connect(SIp, SPort, [binary, {active, true}, {packet, 2}]),
	loop(#state{socket = Sock}).

loop(State = #state{socket = Sock}) ->
	receive
		{echo, Msg} ->
			gen_tcp:send(Sock, term_to_binary({echo, Msg})),
			loop(State);
		{tcp, _, TcpData} ->
			case binary_to_term(TcpData) of
				{echo, Msg} ->
					io:format("ECHO: ~p~n", [Msg])
			end,
			loop(State)
	end.	
