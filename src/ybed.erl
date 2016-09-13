-module(ybed).
-compile(export_all).

start() ->
    {ok, spawn(?MODULE, run, [])}.

run() ->
    Id = "embedded",
    {ok, Dir} = file:get_cwd(),
    Index = string:rstr(Dir, "ebin"),
    YarwsDir = string:sub_string(Dir, 1, Index - 1),
    GconfList = [                 
                 {id, Id},
                 {logdir, YarwsDir ++ "logs"},
                 {ebin_dir, [YarwsDir ++ "ebin"]},
                 {include_dir, [YarwsDir ++ "include"]}],
    Docroot = YarwsDir ++ "www",
    SconfList = [{port, 80},
                 {servername, "alphattt_web_server"},
                 {listen, {127,0,0,1}},
                 {docroot, Docroot}],
    {ok, SCList, GC, ChildSpecs} =
        yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),
    [supervisor:start_child(ybed_sup, Ch) || Ch <- ChildSpecs],
    yaws_api:setconf(GC, SCList),
    {ok, self()}.
