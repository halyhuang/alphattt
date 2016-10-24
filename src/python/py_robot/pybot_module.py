# -*- coding: UTF-8 -*-


class PybotModule(object):
    def __init__(self, cal_time, board):
        super(PybotModule, self).__init__()
        self.cal_time = cal_time
        self.board = board

    def get_move(self, state):
        import random

        move = random.choice(self.board.legal_moves(state))
        msg_time = "== calculate 1 paths using 1.0 seconds =="
        msg_pro = "== probability is 100. 1/1 =="
        return move, msg_time, msg_pro
