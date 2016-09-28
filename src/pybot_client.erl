-module(pybot_client).

-export([connect/4]).


connect(SIP, NickName, Password, RoomID) ->
    % SIP = "127.0.0.1",
    {ok, Pid} = player_client:start(NickName, pybot, board, SIP, 8011),
    player_client:login(Pid, Password),
    player_client:enter_room(Pid, RoomID),
    ok.
