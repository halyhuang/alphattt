# -*- coding: UTF-8 -*-


class PybotModule(object):
    def __init__(self, cal_time, board):
        super(PybotModule, self).__init__()
        self.cal_time = cal_time
        self.board = board

    def get_move(self, state):
        move = (0, 0, 0, 0)
        msg_time = "== calculate 999 paths using 1.0 seconds =="
        msg_pro = "== probability is 100. 999/999 =="
        return move, msg_time, msg_pro
