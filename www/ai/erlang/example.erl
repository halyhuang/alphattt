-module(example).

-compile(export_all).

mcts_vs_ucb1() ->
	robot_vs_robot(mcts, mcts_ucb1).

pall_vs_ucb1() ->
	robot_vs_robot(mcts_pall, mcts_ucb1).	

robot_vs_robot(Mcts, Mcts_ucb1) ->	
	SIP = "34.192.54.97",
	NickName = atom_to_list(Mcts),
	{ok, Pid} = player_client:start(NickName, Mcts, board, SIP, 8011),
	player_client:login(Pid, ""),
	RobotNickName = atom_to_list(Mcts_ucb1),
	{ok, RobotPid} = player_client:start(RobotNickName, Mcts_ucb1, board, SIP, 8011),
	player_client:login(RobotPid, ""),

	Rooms = player_client:show_room(Pid),
	RoomID = get_empty_room(Rooms),
	io:format("enter room ~p~n", [RoomID]),

	player_client:enter_room(Pid, RoomID),
	io:format("player ~p enter room ~p~n", [NickName, RoomID]),

	player_client:enter_room(RobotPid, RoomID),
	io:format("player ~p enter room ~p~n", [RobotNickName, RoomID]),
	{Pid, RobotPid, RoomID}.

get_empty_room(Rooms) ->
	io:format("rooms ~p~n", [Rooms]),
	[EmptyRoomID | _T] = [ RoomID || {RoomID, Status, Players} <- Rooms, (Status == waiting) and (length(Players) == 0)],
	EmptyRoomID.

human_vs_robot(Type, RoomID) ->	
	{ok, RobotPid} = player_client:start(atom_to_list(Type), Type, board, "10.8.39.80", 8011),
	player_client:login(RobotPid, ""),
	player_client:enter_room(RobotPid, RoomID).

connect() ->
	io:format("ping gameserver ~p~n", [net_adm:ping('gameserver@localhost')]),
	io:format("ping webserver ~p~n", [net_adm:ping('webserver@localhost')]).

test_robot() ->
	spawn(fun() -> init() end).

init() ->
	timer:send_interval(1 * 30 * 1000, robot_interval),
	{Pid, RobotPid, RoomID} = robot_vs_robot(mcts, mcts_ucb1),
	loop([Pid, RobotPid, RoomID]).

loop([Pid, RobotPid, RoomID]) ->
	receive 
		robot_interval ->
			player_client:enter_room(Pid, RoomID),
			player_client:enter_room(RobotPid, RoomID),			
			loop([Pid, RobotPid, RoomID])
		after 2 * 60 * 1000 ->
			exit(time_out)			
	end.


			


	









