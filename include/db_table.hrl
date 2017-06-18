%0 存放用户
-record(user, {name, password="", type=human, win=0, draw=0, total=0}).
%0

%1 存放比赛数据，因为服务器已经运行了一段时间了，暂时不动原有的数据库表结构
%  后续最好是将原有的战绩作为练习拆出来
-record(game_record, {name, type=human, win=0, draw=0, total=0}).
%1

%2 存放历史，历史用对手和对战时间作为索引，去查询文件中的数据回放
-record(game, {player, opponent, time, result, steps}).
%2