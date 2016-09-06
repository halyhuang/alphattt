-module(test).

-export([robot_vs_robot/0]).

robot_vs_robot() ->
	NickName = "mcts",
	{ok, Pid} = player_client:start(NickName, mcts, board, "127.0.0.1", 8011),
	player_client:login(Pid, ""),
	RobotNickName = "mcts_ucb1",
	{ok, RobotPid} = player_client:start(RobotNickName, mcts_ucb1, board, "127.0.0.1", 8011),
	player_client:login(RobotPid, ""),

	Rooms = player_client:show_room(Pid),
	RoomID = get_empty_room(Rooms),

	player_client:enter_room(Pid, RoomID),
	io:format("player ~p enter room ~p~n", [NickName, RoomID]),

	player_client:enter_room(RobotPid, RoomID),
	io:format("player ~p enter room ~p~n", [RobotNickName, RoomID]),
	ok.

get_empty_room(Rooms) ->
	io:format("rooms ~p~n", [Rooms]),
	[EmptyRoomID | _T] = [ RoomID || {RoomID, Status, Players} <- Rooms, (Status == waiting) and (length(Players) == 0)],
	EmptyRoomID.


			


	









