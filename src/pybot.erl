-module(pybot).
-export([start/0, start/3]).
-export([get_move/1, update/2, display/3, notify/2, stop/1]).

-record(state,  {board = board,
                 max_time = 1000,  % milliseconds
                 exploration_factor = 1.4,
                 game_states = [],
                 from
                 }).

% APIs
start() ->
    start(board, 1000, 1.4).

start(Board, MaxTime, ExplorationFactor) ->
    spawn(fun() -> 
                init([Board, MaxTime, ExplorationFactor])
          end).

update(Pid, GameState) ->
    call(Pid, {update, GameState}).

display(Pid, GameState, Move) ->
    call(Pid, {display, GameState, Move}).

get_move(Pid) ->
    call(Pid, get_move).

notify(Pid, Info) ->
    call(Pid, {notify, Info}).  
    
stop(Pid) ->
    call(Pid, stop).    

%%
init([Board, MaxTime, ExplorationFactor]) ->
    {ok, Ppy} = python:start([{python_path,"../src/python"},{python, "python2"}]),
    State = #state{board = Board,
                     max_time = MaxTime,
                     exploration_factor = ExplorationFactor
                     },
    loop(State, Ppy).

loop(State, Ppy) ->
    receive
        {call, Ref, From, Msg} ->
            case handle_call(Msg, State#state{from = From}, Ppy) of
                {reply, Reply, NewState} ->
                    From ! {Ref, Reply},
                    loop(NewState, Ppy);
                stop ->
                    From ! {Ref, ok}
            end
    end.

call(Pid, Msg) ->
    Ref = make_ref(),
    Pid ! {call, Ref, self(), Msg},
    receive
        {Ref, Reply} ->
            Reply
    end.

handle_call({update, GameState}, State=#state{game_states=GSs}, Ppy) -> 
    {reply, ok, State#state{game_states=[GameState | GSs]}};
handle_call({display, GameState, Move}, State=#state{board=Board, max_time=MaxTime}, Ppy) ->
    case Move of
        none ->
            python:call(Ppy,pybot,init,[MaxTime]);
        _ ->
            python:call(Ppy, pybot, set_move, [Move])
    end,
    io:format("player move ~p~n", [Move]),
    {reply, ok, State};
handle_call({notify, _Info}, State, _) ->
    {reply, ok, State};
handle_call(get_move, State=#state{board=Board, game_states=GSs, from = From}, Ppy) ->
    {NextMove, Msg_time, Msg_pro} = python:call(Ppy, pybot, get_move, []),
    GS = hd(GSs),
    CurrentPlayerID = Board:current_player(GS),
    notify_room(From, CurrentPlayerID, [Msg_time]),
    notify_room(From, CurrentPlayerID, [Msg_pro]),
    From ! {play, NextMove},
    {reply, {ok, NextMove}, State};
handle_call(stop, _State, Ppy) ->
    python:stop(Ppy),
    stop.

notify_room(PlayerClient, PlayerID, Info) ->
	PlayerClient ! {notify, PlayerID, lists:flatten(Info)}.
