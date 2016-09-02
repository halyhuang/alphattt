%0 存放用户
-record(user, {name, password="", type=human, win=0, draw=0, total=0}).
%0

%2 存放历史，历史用对手和对战时间作为索引，去查询文件中的数据回放
-record(game, {player, opponent, time, result, steps}).
%2