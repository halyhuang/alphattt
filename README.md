Alpha Tic Tac Toe Ultimate
=====


About
-----

Alpha Tic Tac Toe Ultimate(下称 AlphaTTT)项目是一个开源的棋类在线对战平台项目。支持人工智能程序（下称AI）和人类同时接入并且对弈，能够提供排名和观战功能。

本项目包括一个使用[Yaws](http://yaws.hyber.org)框架实现的Web服务器，以及一个使用[Erlang](http://www.erlang.org/downloads)语言实现的对战服务框架，对战平台主要使用Erlang语言实现。

Install
-----

本项目依赖[Yaws](http://yaws.hyber.org)和[Erlang](http://www.erlang.org/downloads)。
请根据您的操作系统，下载Yaws1.97以后版本和Erlang的Otp16R以后版本。

请先clone本项目代码到本地。然后在cmd／shell界面执行如下命令

```bash
cd src
```

```bash
erl -make
```

这样就完成了系统的安装和编译。


Config & Run
-----

打开一个命令行终端，进入 ebin 文件夹，

```bash
cd ebin
```

####初始化数据库

在window环境下，执行

```bash
deploy.bat
```

在unix/bash环境下，执行

```bash
bash deploy.bat
```

这个脚本用于初始化数据库，仅需执行一遍。

####启动对战服务

重新打开一个命令行终端，进入 ebin 文件夹，在window环境下，执行

```
start_game_server.bat
```

在unix/bash环境下，执行

```
bash start_game_server.bat
````

如果启动服务器时报错如端口冲突，请打开 src/game_sup.erl 进行修改，如下

```erlang
TcpServer = {tcp_server, {tcp_server, start_link,[8011, player_agent]},
            				permanent,2000,worker,[tcp_server]},
```
            				
请将默认的8011端口修改称为你服务器的可用端口即可。
修改完毕后，请执行erl -make 重新编译。

####启动web服务器

重新打开一个命令行终端，进入 ebin 文件夹，在window环境下，执行

```bash
start_web_server.bat
```

在unix/bash环境下，执行

```bash
bash start_web_server.bat
```

如果启动服务器时报错如端口和IP冲突，请打开 src/ybed.erl 进行修改，如下

```erlang
SconfList = [{port, 8888},
                 {servername, "alphattt_web_server"},
                 {listen, {127,0,0,1}},
                 {docroot, Docroot}],
```
           
其中，port属性为web服务器端口，默认为8888；listen监听IP地址，默认为127.0.0.1。
修改完毕后，请执行erl -make 重新编译。

####建立集群

在Web服务器的命令行终端中，输入

```erlang
net_adm:ping(gameserver@yourhostname).
```

其中，yourhostname为你记起的主机名，你可以在gameserver的命令行终端的提示符中查看到。

####访问主页

一起就绪，请在Web浏览器中访问http://localhost/login.html即可。


Working With Erlang
-----

使用Erlang语言可以非常轻松的接入平台，因为这个平台本身就是适用 erlang 语言编写的。

####实现接口

只需要编写一个模块，实现如下接口，

```erlang

-export([start/0]).
-export([get_move/1, update/2, display/3, notify/2, stop/1]).

update(Pid, GameState) -> tobeimplemented.

display(Pid, GameState, Move) -> tobeimplemented.

get_move(Pid) -> tobeimplemented.

notify(Pid, Info) -> tobeimplemented.
	
stop(Pid) -> tobeimplemented.	

```
打开一个命令行终端，进入src目录，键入 erl -make 重新编译。

然后，运行如下erlang语句, 适当修改SIP, 端口号和RoomID即可接入对战平台，
```erlang
	SIP = "10.8.39.80",
	NickName = "your_registered_name",
	{ok, Pid} = player_client:start(NickName, your_implementation, board, SIP, 8011),
	player_client:login(Pid, "your_registered_password"),

	Rooms = player_client:show_room(Pid),
	RoomID = get_empty_room(Rooms),
	io:format("enter room ~p~n", [RoomID]),

	player_client:enter_room(Pid, RoomID),
```

>> 在哪里注册用户名？ 很简单，在前面提供的Web页面上注册就可以了。

Working With Java
-----

####运行ErlangBridge节点
打开一个命令行终端，在windows系统，运行

```bash
ebin/jerlang.bat
```

在unix/bash系统，运行
```bash
bash ebin/jerlang.bat
```

####编译Java代码
将src/java/src下java代码编译打包称jar文件，注意依赖 erlang/otp安装目录/lib/Jinterface-1.x.x/priv/OtpErlang.jar文件,然后运行即可。

#####运行
适用Java环境，运行com.zte.alphattt.Boot的main函数。
如下定义,
```java
    PlayerClient playerClient = new PlayerClient(new Mcts(), new Board());
	...
    playerClient.connect("127.0.0.1", 8011);
    playerClient.login("javarobot", "password");
    playerClient.enterRoom("javarobot", 6);
```
#####扩展
仅需实现com.zte.alphattt.game.Player接口，并在初始化时传入（a步骤中初始化PlayerClient里传入,如默认已经实现的Mcts类），
即可加入人机，机机对战。

#####棋盘
提供了一个较快速度的com.zte.alphattt.game.Board，供自己训练时参考。

#####训练
提供训练台com.zte.alphattt.Bench,供平时训练和测试时使用。


Working With Python
-----

#####编写自己的python机器人
在src/python下增加python对弈程序pybot.py，实现3个接口 init/get_move/set_move；

已提供一个python版本的board程序可供调用。

#####运行
在ebin下运行run_erlport.sh；

执行
```erlang
pybot_client:connect("SERVER_IP", "NICKNAME", "PASSWORD", ROOMID).
```
进入对战房间。

（其中SERVER_IP为对战服务器ip，NICKNAME/PASSWORD为已注册的机器人帐号，ROOMID为房间号）


Working with C/C++
-----

