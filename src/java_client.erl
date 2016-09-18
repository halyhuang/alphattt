-module(java_client).

-export([go/0]).

-record(state, {socket, java_pid, java_node, remote_ip, remote_port}).

go() ->
	Pid = spawn(fun() -> init() end),
	register(?MODULE, Pid),
	{ok, Pid}.

init() ->
	loop(#state{}).

loop(State = #state{socket=Socket, java_pid = JavaPid, java_node= JavaNode}) ->
	receive
		{connect, NewRemoteIP, NewRemotePort, NewJavaPid, NewJavaNode} ->
			{ok, NewSocket} = gen_tcp:connect(NewRemoteIP, NewRemotePort, 
												 [binary, {active, true}, 
												 {packet, 2}]),
			io:format("(~p~n) connected", [{NewRemoteIP, NewRemotePort}]),
			loop(#state{socket=NewSocket, java_pid=NewJavaPid, java_node=NewJavaNode,
				remote_ip=NewRemoteIP, remote_port=NewRemotePort});
		{tcp, _, TcpData} ->
			io:format("TcpData received: ~p~n", [TcpData]),
			transport(binary_to_term(TcpData), JavaPid, JavaNode),
			loop(State);
		Msg ->
			io:format("Mail box msg received: ~p~n", [Msg]),
			gen_tcp:send(Socket, term_to_binary(Msg)),
			loop(State)
	end.

transport(Msg, JavaPid, JavaNode) ->
	io:format("Send msg ~p to ~p@~p ~n", [Msg, JavaPid, JavaNode]),
	{JavaPid, JavaNode} ! Msg.



