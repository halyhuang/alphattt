# -*- coding: UTF-8 -*-
from multiprocessing import Pool
from client.client import Player, run
import time
import random

def random_game((legal_game_state, max_depth, board, tree)):
    expand, need_update, iter_count = [], [], 1
    while True:
        state = select_one(legal_game_state)
        if tree.has_key(tuple(state)):
            need_update.append(state)
        elif len(expand) == 0:
            expand.append(state)
            max_depth = iter_count
        winner = board.winner(state)
        if winner < 4:
            return expand, need_update, max_depth, winner
        iter_count += 1
        legal_game_state = player_legal_states(state, board)

def select_one(legal_game_state):
    return random.choice(legal_game_state)[1]

def player_legal_states(state, board):
    legal_moves = board.legal_moves(state)
    legal_states = map(
        lambda move: board.next_state(state, move),
        legal_moves)
    return list(zip(legal_moves, legal_states))

class MctsParalleAI(Player):
    def __init__(self, cal_time, board):
        super(MctsParalleAI, self).__init__(cal_time, board)
        self.tree = {}
        self.processor_num = 4
        self.pool = Pool(self.processor_num)

    def get_move(self):
        legal_game_state = player_legal_states(self.cur_opponent_state, self.board)
        if len(legal_game_state) == 1:
            return legal_game_state[0][0]
        games, max_depth, spent_time = self.run_simulation(legal_game_state)
        #print "Simulate [%d] games, using [%f] seconds, max depth [%d] ==" % (games, spent_time, max_depth)
        move_stats = self.make_state(legal_game_state)
        return self.choose_best(move_stats)
    
    def run_simulation(self, legal_game_state):
        expect_winner = self.board.currentPlayer(self.cur_opponent_state)
        begin_time, games, max_depth, spent_time = time.time(), 0, 0, 0
        while spent_time < self.cal_time:
            res = self.pool.map(random_game, [(legal_game_state, max_depth, self.board, self.tree)] * self.processor_num * 3)
            for expand, need_update, new_depth, winner in res:
                self.propagate_back(expand, need_update, expect_winner, winner)
                max_depth = max(max_depth, new_depth)
            spent_time = time.time() - begin_time
            games += 12
        return games, max_depth, spent_time
    
    def propagate_back(self, expand, need_update, expect_winner, winner):
        for expand_state in expand:
            self.tree[tuple(expand_state)] = {"win": 0, "total": 0, "per": 0}
            need_update.append(expand_state)
        win = 1 if winner == expect_winner else 0
        for update_state in need_update:
            node = self.tree[tuple(update_state)]
            node["win"] += win
            node["total"] += 1
            node["per"] = node["win"] * 1.0 / node["total"]
    
    def make_state(self, legal_game_state):
        return [(move, self.get_per(self.tree.get(tuple(state), None))) for move, state in legal_game_state]
        
    def get_per(self, node):
        return (-1, 0, 0) if node == None else (node["per"], node["win"], node["total"])
            
    def choose_best(self, move_stats):
        #print "move_stats:", [(move_stat[0], "%.2f" % (100 * move_stat[1][0]), "%d/%d" % (move_stat[1][1], move_stat[1][2])) for move_stat in move_stats]
        best = max(move_stats, key=lambda x : x[1][0])
        #print "chooosed best:", best
        return best[0]

    def choice(self, legal_moves, state):
        return self.random.choice(legal_moves)


if __name__ == '__main__':
    run("127.0.0.1", 8011, "TT", "123456", 3, MctsParalleAI)
