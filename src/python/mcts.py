# -*- coding: UTF-8 -*-
import requests
import json
import time
import random
import copy

from board import Board


cookie = {}
DEBUG = True
cal_time = 1


def send(data):
    global cookie
    url = "http://10.8.39.80/alphattt.yaws"
    # url = "http://10.9.88.20:8080/alphattt.yaws"
    # cookie = {"SID": "nonode@nohost-180628681594120732172449048974498269359"}
    data = json.dumps(data)
    r = requests.post(url, data, cookies=cookie)
    if r.cookies.get("SID", None) is not None:
        cookie["SID"] = r.cookies.get("SID", None)
        print cookie
    # print r.json()
    return r.json()


def start():
    if DEBUG:
        return True
    return send({"id": "httpReq", "method": "start_game", "params": []})["result"] == "ok"


def do_move(move):
    if DEBUG:
        return True
    return send({"id": "httpReq", "method": "set_move", "params": list(move)})["result"] == "ok"


def check(board=None):
    if DEBUG:
        move = random.choice(board.get_legal_moves())
        return move, [0]
    result = send({"id": "httpReq", "method": "get_state", "params": []})
    legal_moves = result.get("result", {}).get("legal_moves", [])
    move = result.get("result", {}).get("move", [])
    legal_moves = [(item["R"], item["C"], item["r"], item["c"]) for item in legal_moves]
    if len(move) > 0:
        move = (move["R"], move["C"], move["r"], move["c"])
    return move, legal_moves


class TreeSearch(object):
    def __init__(self):
        super(TreeSearch, self).__init__()
        self.tree = {}

    def get_move(self, board):
        paras = {"begin": time.time(), "num": 0, "time": 0}
        legal_moves = board.get_legal_moves()
        if len(legal_moves) == 0:
            return None
        while True:
            paras["num"] += 1
            self.__inc_tree(self.__tree_path(board, legal_moves))
            paras["time"] = time.time() - paras["begin"]
            if paras["time"] > cal_time:
                break
        print "== calculate %d paths using %f seconds ==" % (paras["num"], paras["time"])
        return self.__search_tree(board, legal_moves)

    def __tree_path(self, board, legal_moves):
        _board = copy.deepcopy(board)
        _legal_moves = legal_moves
        curr_player = Board.PLAYER_ME
        move_trace = []
        while True:
            winner = _board.move((random.choice(_legal_moves), curr_player))
            move_trace.append(_board.get_board())
            if winner is not None:
                return (move_trace, winner)
            _legal_moves = _board.get_legal_moves()
            curr_player = (curr_player + 1) % 2

    def __inc_tree(self, (move_trace, winner)):
        inc = {"win": 0, "total": 1}
        if winner == Board.PLAYER_ME:
            inc["win"] = 1
        for item in move_trace:
            node = None
            try:
                node = self.tree[item]
            except Exception:
                self.tree[item] = {"win": 0, "total": 0}
                node = self.tree[item]
            node["win"] += inc["win"]
            node["total"] += inc["total"]

    def __search_tree(self, board, legal_moves):
        final = {"per": 0, "win": 0, "total": 0, "move": None}
        for move in legal_moves:
            _board = copy.deepcopy(board)
            _board.move((move, Board.PLAYER_ME))
            node = self.tree.get(_board.get_board(), None)
            wins = node["win"] * 100 / node["total"]
            if wins >= final["per"]:
                final["per"], final["win"], final["total"], final["move"] = \
                    wins, node["win"], node["total"], move
        print "== probability is %d. %d/%d ==" % (final["per"], final["win"], final["total"])
        return final["move"]


def main(tree):
    if start():
        print "start"
        time.sleep(3)
        board = Board()
        while True:
            winner = None
            move, legal_moves = check(board)
            if len(move) == 0 and len(legal_moves) == 0:
                time.sleep(1)
                continue
            if len(move) > 0:
                winner = board.move((move, Board.PLAYER_AI))
                print "A move:"
                board.paint()
            if len(legal_moves) > 0:
                i_move = tree.get_move(board)
                if i_move is not None:
                    winner = board.move((i_move, Board.PLAYER_ME))
                    do_move(i_move)
                    print "I move:"
                    board.paint()
            if winner is not None:
                return winner

if __name__ == '__main__':
    import cProfile

    wins = {"win": 0, "total": 0, "draw": 0}
    players = ["I", "A"]
    num = 0
    while True:
        num += 1
        if num > 30:
            break
        tree = TreeSearch()
        if DEBUG:
            print cProfile.run("main(tree)")
            break
        winner = main(tree)
        if winner == Board.PLAYER_ME:
            wins["win"] += 1
        if winner == Board.PLAYER_NO:
            wins["draw"] += 1
        else:
            wins["total"] += 1
        print "winner is " + players[winner]
        print "total: %d/%d draw: %d nodes: %d" % \
            (wins["win"], wins["total"], wins["draw"], len(tree.tree))
