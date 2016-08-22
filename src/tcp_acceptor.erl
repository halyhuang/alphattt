-module(tcp_acceptor).
-behaviour(gen_server).

%%APIs
-export([start/2]).

%%gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
			handle_info/2, terminate/2, code_change/3]).

%%callbacks:
%% init(Socket) -> {ok, CBState}
%% handle_tcp_data(TcpData, CbState) -> {ok, NewCbState} | stop
%% handle_info(Msg, CbState) -> {ok, NewCbState} | stop

-record(state, {lsock, cb_module, cb_state}).

%%APIs
start(LSock, CallbackModule) ->
	gen_server:start(?MODULE, [LSock, CallbackModule], []).
%% callbacks

init([LSock, CallbackModule]) ->
	{ok, #state{lsock = LSock, cb_module=CallbackModule}, 0}.


handle_call(_Msg, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(timeout, #state{lsock = LSock,
							cb_module = CallbackModule} = State) ->
	{ok, Sock} = gen_tcp:accept(LSock),
	tcp_acceptor:start(LSock, CallbackModule),
	{ok, CbState} = CallbackModule:init(Sock),	
	{noreply, State#state{cb_state=CbState}};
handle_info({tcp, _, RcvData}, State=#state{cb_module = CBM, cb_state = CBS}) ->
	case CBM:handle_tcp_data(RcvData, CBS) of
		{ok, NewSBS} ->
			{noreply, State#state{cb_state = NewSBS}};
		stop ->
			{stop, normal, State}
	end;
handle_info(Msg, State = #state{cb_module = CBM, cb_state = CBS}) ->
	case CBM:handle_info(Msg, CBS) of
		{ok, NewSBS} ->
			{noreply, State#state{cb_state=NewSBS}};
		stop ->
			{stop, normal, State}
	end.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}. 	

	