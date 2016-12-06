-module(webagent_ws).
-export([handle_message/1, handle_message/2, init/1, handle_open/2, handle_info/2]).

-record(state, {ws_state = none, web_agent = none}).

handle_message(Message) ->
	io:format("handle_message without state Message is ~p~n.", [Message]),
	noreply.

init(Args) ->
	{ok, #state{}}.

handle_open(WSState, State) ->
	io:format("handle_open ~p ~p~n", [WSState, State]),
	{ok, #state{ws_state = WSState}}.

handle_message({text, Message}, State = #state{ws_state = WSState}) ->
	{Reply, NewState} = 
		case decode_payload(Message) of
			{ok, Method, Args} -> 
				try_handle_call(Method, Args, State);
			{error, Err} ->
				{[{"error", "decode error"}], State}
		end,
	{reply, make_reply(Reply), NewState}.
	% NewMessage = json2:obj_from_list([{"is_get_move", true},
 %                                    {"player", 0}]),
	% NewMessage1 = json2:encode(NewMessage),
	% io:format("handle_message NewMessage is ~p~n", [NewMessage1]),
	% R = yaws_api:websocket_send(WSState, {text, list_to_binary(NewMessage1)}),
	% io:format("handle_message send back message~p~n", [R]),
	%{noreply, State}.

handle_info(Info, State) ->
	io:format("handle_info ~p ~p~n", [Info, State]),
	{noreply, State}.

try_handle_call(Method, Args, State) ->
	try 
		handle_call(Method, Args, State)
	catch
		error:Err -> {[{"method", atom_to_list(Method)}, {"error", "handle_call error"}], State}
	end.

handle_call(login, [UserName, Password], State = #state{web_agent = none}) ->
	{ok, Pid} = web_agent_mgr:start_agent(),
	web_agent:login(Pid, UserName, Password),
	IsLogin = web_agent:is_login(Pid),
	{[{"method", "login"}, {"value", IsLogin}], State#state{web_agent=pid_to_list(Pid)}};
handle_call(login, [UserName, Password], State = #state{web_agent = PidStr}) ->
	Pid = web_agent_mgr:get_agent_pid(PidStr),
	web_agent:login(Pid, UserName, Password),
	IsLogin = web_agent:is_login(Pid),
	{[{"method", "login"}, {"value", IsLogin}], State#state{web_agent=pid_to_list(Pid)}};
handle_call(Method, Args, State) ->
	io:format("unkown handle_call ~p ~p ~n", [Method, Args]),
	{[{"method", atom_to_list(Method)}, {"error", "unkown method"}], State}.

%% decode_payload
decode_payload(JSonBinary) ->
    try
    	JSonStr = binary_to_list(JSonBinary),
        {ok, JSON} = json2:decode_string(JSonStr),
        Method = list_to_atom(jsonrpc:s(JSON, method)),
        {array, Args} = jsonrpc:s(JSON, params),

        {ok, Method, Args}
    catch
        error:Err -> {error, Err}
    end.

make_reply(Reply) when is_list(Reply) ->
	%io:format("make_reply ~p ~n", [Reply]),
	NewMessage = json2:obj_from_list(Reply),
	%io:format("make_reply obj_from_list is ~p ~n", [NewMessage]),
	NewMessage1 = json2:encode(NewMessage),
	%io:format("make_reply encode is ~p ~n", [NewMessage1]),
	{text, list_to_binary(NewMessage1)};
make_reply(Reply) ->
	io:format("unkown decode reply type ~p~n", [Reply]),
	make_reply([{"error", "unkown decode type"}]).