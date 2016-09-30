# -*- coding: UTF-8 -*-
import time
import random
import math


from board import Board

cal_time = 1
board = None
tree = None

# ==== interfaces ====


def init(max_time):
    global tree
    global board
    global cal_time
    cal_time = max_time / 1000
    tree = TreeSearch()
    board = Board()


def set_move(move):
    if move is not "none":
        board.move(move)


def get_move():
    move, msg_time, msg_pro = tree.get_move(board)
    print "pybot move: ", move
    board.display()
    return move, msg_time, msg_pro
# =================


class TreeSearch(object):
    def __init__(self):
        super(TreeSearch, self).__init__()
        self.tree = {}

    def __random_choice(self, legal_moves, _):
        return random.choice(legal_moves)

    def __ucb1_choice(self, legal_moves, board):
        EF = 1.4
        node = self.tree.get(board.get_board())
        if node is None or node["total"] < len(legal_moves) * 5:
            return self.__random_choice(legal_moves, board)
        logTotal = math.log(node["total"])
        selected = {"move": None, "cal": 0}
        for _move in legal_moves:
            _board = Board(board)
            _board.move(_move)
            node = self.tree.get(_board.get_board())
            if node is None:
                return self.__random_choice(legal_moves, board)
            cal = node["per"] + EF * math.sqrt(logTotal / node["total"])
            if cal >= selected["cal"]:
                selected = {"move": _move, "cal": cal}
        return selected["move"]

    def __choice(self, legal_moves, board):
        return self.__ucb1_choice(legal_moves, board)

    def get_move(self, board):
        paras = {"begin": time.time(), "num": 0, "time": 0}
        legal_moves = board.legal_moves()
        if len(legal_moves) == 0:
            return None
        expect_winner = Board.NEXT_PLAYER[board.current_player()]
        while True:
            paras["num"] += 1
            self.__inc_tree(self.__tree_path(board, legal_moves), expect_winner)
            paras["time"] = time.time() - paras["begin"]
            if paras["time"] > cal_time:
                break
        msg_time = "== calculate %d paths using %f seconds ==" % (paras["num"], paras["time"])
        print msg_time
        move, msg_pro = self.__search_tree(board, legal_moves)
        return move, msg_time, msg_pro

    def __tree_path(self, board, legal_moves):
        _board = Board(board)
        _legal_moves = legal_moves
        move_trace = []
        while True:
            _board.move(self.__choice(_legal_moves, _board))
            winner = _board.winner()
            move_trace.append(_board.get_board())
            if winner is not None:
                return (move_trace, winner)
            _legal_moves = _board.legal_moves()

    def __inc_tree(self, (move_trace, winner), expect_winner):
        inc = {"win": 0, "total": 1}
        if winner == expect_winner:
            inc["win"] = 1
        for item in move_trace:
            node = None
            try:
                node = self.tree[item]
            except Exception:
                self.tree[item] = {"win": 0, "total": 0, "per": 0}
                node = self.tree[item]
            node["win"] += inc["win"]
            node["total"] += inc["total"]
            node["per"] = node["win"] / node["total"]

    def __search_node(self, board, move):
        _board = Board(board)
        _board.move(move)
        node = self.tree.get(_board.get_board(), None)
        return node

    def __search_tree(self, board, legal_moves):
        final = {"per": 0, "win": 0, "total": 0, "move": None}
        for move in legal_moves:
            node = self.__search_node(board, move)
            if node is None:
                continue
            wins = node["win"] * 100 / node["total"]
            if wins >= final["per"]:
                final["per"], final["win"], final["total"], final["move"] = \
                    wins, node["win"], node["total"], move
        msg_pro = "== probability is %d. %d/%d ==" % (final["per"], final["win"], final["total"])
        print msg_pro
        return final["move"], msg_pro
