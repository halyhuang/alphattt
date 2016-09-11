-module(java_client).

-export([start/2, start/0, transport/1]).

-record(state, {socket}).

start() ->
	java_client:start("127.0.0.1", 8011).

start(SIp, SPort) ->
	Pid = spawn(fun() -> init(SIp, SPort) end),
	register(?MODULE, Pid),
	{ok, Pid}.

init(SIp, SPort) ->
	transport("Welcome java"),
	io:format("connect to ~p~n", [{SIp, SPort}]),
	{ok, Sock} = gen_tcp:connect(SIp, SPort, [binary, {active, true},
													  {packet, 2}]),
	loop(#state{socket=Sock}).

loop(State = #state{socket=Sock}) ->
	receive
		{tcp, _, TcpData} ->
			io:format("TcpData received: ~p~n", [TcpData]),
			transport(binary_to_term(TcpData)),
			loop(State);
		Msg ->
			io:format("Mail box msg received: ~p~n", [Msg]),
			gen_tcp:send(Sock, term_to_binary(Msg)),
			loop(State)
	end.

transport(Msg) ->
	io:format("tcp cast received: ~p~n", [Msg]),
	{java_ttt, 'java_ttt_node@127.0.0.1'} ! Msg.



