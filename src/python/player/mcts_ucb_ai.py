# -*- coding: UTF-8 -*-
import time
from client.client import run, Player
import random
from math import log, sqrt

class Mcts_UCB_AI(Player):
    def __init__(self, cal_time, board):
        super(Mcts_UCB_AI, self).__init__(cal_time, board)
        self.tree = {}
        self.EF = 0.5

    def get_move(self):
        legal_game_state = self.player_legal_states(self.cur_opponent_state)
        if len(legal_game_state) == 1:
            return legal_game_state[0][0]
        games, max_depth, spent_time = self.run_simulation(legal_game_state)
        # print "Simulate [%d] games, using [%f] seconds, max depth [%d]" % (games, spent_time, max_depth)
        move_stats = self.make_state(legal_game_state)
        return self.choose_best(move_stats)
    
    def run_simulation(self, legal_game_state):
        expect_winner = self.board.currentPlayer(self.cur_opponent_state)
        begin_time, games, max_depth, spent_time = time.time(), 0, 0, 0
        while spent_time < self.cal_time:
            expand, need_update, new_depth, winner = self.random_game(legal_game_state, max_depth)
            self.propagate_back(expand, need_update, expect_winner, winner)
            max_depth = max(max_depth, new_depth)
            spent_time = time.time() - begin_time
            games += 1
        return games, max_depth, spent_time

    def random_game(self, legal_game_state, max_depth):
        expand, need_update, iter_count = [], [], 1
        while True:
            state = self.select_one(legal_game_state)
            if self.tree.has_key(tuple(state)):
                need_update.append(state)
            elif len(expand) == 0:
                expand.append(state)
                max_depth = iter_count
            winner = self.board.winner(state)
            if winner < 4:
                return expand, need_update, max_depth, winner    
                
            iter_count += 1
            legal_game_state = self.player_legal_states(state)

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
    
    def select_one(self, legal_game_state):
        state_statistics = [self.tree.get(tuple(state), None) for _, state in legal_game_state]
        if None in state_statistics:
            return (random.choice(legal_game_state))[1]
        else:
            return self.ucb(state_statistics, legal_game_state)
    
    def ucb(self, state_statistics, legal_game_state):
        total = sum([statis["total"] for statis in state_statistics])
        merge_list = list(zip(legal_game_state, state_statistics))
        ucb_list = [ ((statistic["per"] + self.EF * sqrt(log(total) / statistic["total"])), state)   for (_, state), statistic in merge_list]
        best = max(ucb_list, key=lambda x : x[0])
        return best[1]
    
    def make_state(self, legal_game_state):
        return [(move, self.get_per(self.tree.get(tuple(state), None))) for move, state in legal_game_state]
        
    def get_per(self, node):
        return (-1, 0, 0) if node == None else (node["per"], node["win"], node["total"])
            
    def choose_best(self, move_stats):
        # print "move_stats:", [(move_stat[0], "%.2f" % (100 * move_stat[1][0]), "%d/%d" % (move_stat[1][1], move_stat[1][2])) for move_stat in move_stats]
        best = max(move_stats, key=lambda x : x[1][0])
        # print "chooosed best:", best
        return best[0]

    def player_legal_states(self, state):
        legal_moves = self.board.legal_moves(state)
        that = self
        legal_states = map(
            lambda move: that.board.next_state(state, move),
            legal_moves)
        return list(zip(legal_moves, legal_states))

if __name__ == '__main__':
    run("127.0.0.1", 8011, "TT", "123456", 3, Mcts_UCB_AI)
