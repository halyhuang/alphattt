-module(player).
-export([start/2, update/3, display/4, get_move/2, show/2, stop/2, notify/3]).
-export([get_legal_move/2, set_move/3, is_move/2, get_opponent_move/2]).

start(Module, Board) ->
	Module:start(Board, 1000, 1.4).

update(Module, Player, GameState) ->
	Module:update(Player, GameState).

display(Module, Player, GameState, Move) ->
	Module:display(Player, GameState, Move).

get_move(Module, Player) ->
	Module:get_move(Player).			

notify(Module, Player, Msg) ->
	Module:notify(Player, Msg).

stop(Module, Player) ->
	Module:stop(Player).

show(Module, Player) ->
	Module:show(Player).

get_legal_move(Module, Player) ->	
	Module:get_legal_move(Player).

set_move(Module, Player, Move) ->	
	Module:set_move(Player, Move).

is_move(Module, Player) ->
	Module:is_move(Player).	

get_opponent_move(Module, Player) ->
	Module:get_opponent_move(Player).	