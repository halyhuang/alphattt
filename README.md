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
start_game_server.bat
```

在unix/bash环境下，执行

```bash
bash start_game_server.bat
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



Working With Java
-----

Working With Python
-----


