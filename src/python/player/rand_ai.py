# -*- coding: UTF-8 -*-
import random
from client.client import Player, run

class RandomAI(Player):
    def get_move(self):
        move = random.choice(self.board.legal_moves(self.cur_opponent_state))
        return move

if __name__ == '__main__':
    run("127.0.0.1", 8011, "TT", "123456", 5, RandomAI)
