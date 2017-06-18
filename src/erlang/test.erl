-module(test).

-compile(export_all).

mcts_vs_mcts() ->
	robot_vs_robot(mcts, mcts).

robot_vs_robot(Player1, Player2) ->	
	SIP = "127.0.0.1", %% AlphaTTT平台的IP地址
	NickName = atom_to_list(Player1),
	%%连接平台，端口号是8011。用注册的用户登录，默认的机器人用户名是 robot_guest,密码是空。Player1是AI的模块名
	{ok, Pid} = player_client:start(NickName, Player1, board, SIP, 8011),
	%% 登录
	player_client:login(Pid, ""),
	RobotNickName = atom_to_list(Player2),
	{ok, RobotPid} = player_client:start(RobotNickName, Player2, board, SIP, 8011),
	player_client:login(RobotPid, ""),

	%% 显示当前房间的状态
	Rooms = player_client:show_room(Pid),
	RoomID = get_empty_room(Rooms),
	io:format("enter room ~p~n", [RoomID]),

	%% Player1进入房间
	player_client:enter_room(Pid, RoomID),
	io:format("player ~p enter room ~p~n", [NickName, RoomID]),
	%% Player2进入房间，有两个玩家进入房间后就可以开始对弈
	player_client:enter_room(RobotPid, RoomID),
	io:format("player ~p enter room ~p~n", [RobotNickName, RoomID]),
	{Pid, RobotPid, RoomID}.

%% 获取一个空闲房间
get_empty_room(Rooms) ->
	io:format("rooms ~p~n", [Rooms]),
	[EmptyRoomID | _T] = [ RoomID || {RoomID, Status, Players} <- Rooms, (Status == waiting) and (length(Players) == 0)],
	EmptyRoomID.



			


	









