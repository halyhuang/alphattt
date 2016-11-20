# -*- coding: UTF-8 -*-


class Board(object):

    NEXT_PLAYER = (1, 0)
    PLAYER_DRAW = 2
    BOARDS = (1, 2, 4, 8, 16, 32, 64, 128, 256)
    RS = (0, 0, 0, 1, 1, 1, 2, 2, 2)
    CS = (0, 1, 2, 0, 1, 2, 0, 1, 2)
    WINS = (7, 56, 448, 73, 146, 292, 273, 84)
    MAX_BOARD = 511
    POINTS = ((0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2))
    RC2S = {(0, 0): 0, (0, 1): 1, (0, 2): 2,
            (1, 0): 3, (1, 1): 4, (1, 2): 5,
            (2, 0): 6, (2, 1): 7, (2, 2): 8}

    @staticmethod
    def start():
        return [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  # 17
                0, 0, 1, 0, 0,  # 22
                511, 511, 511, 511, 511, 511, 511, 511, 511,  # 31 legals
                0, 0, 0, 0, 0, 0, 0, 0, 0,  # 40 overs
                None]  # 41 winner

    @staticmethod
    def is_legal(game_state, move):
        return True

    @staticmethod
    def next_state(game_state, (R, C, r, c)):
        __game_state = list(game_state)
        player = Board.NEXT_PLAYER[__game_state[20]]
        s, n = Board.RC2S[(R, C)], Board.RC2S[(r, c)]
        S, N = Board.BOARDS[s], Board.BOARDS[n]
        ssp = s + s + player
        # move
        __game_state[ssp] += N
        __game_state[23 + s] -= N
        __game_state[20], __game_state[21], __game_state[22] = player, s, n
        # calculate
        if Board.is_win(__game_state[ssp]):
            __game_state[18 + player] += S
            __game_state[32 + s] = 1
            if Board.is_win(__game_state[18 + player]):
                __game_state[41] = player
        elif not __game_state[23 + s]:
            __game_state[32 + s] = 1
        if sum(__game_state[32:41]) == 9 \
                and __game_state[41] is None:
            __game_state[41] = Board.PLAYER_DRAW
        return __game_state

    @staticmethod
    def winner(game_state):
        return game_state[41]

    @staticmethod
    def max_moves():
        return 81

    @staticmethod
    def current_player(game_state):
        return game_state[20]

    @staticmethod
    def next_player(game_state):
        return Board.NEXT_PLAYER[game_state[20]]

    @staticmethod
    def legal_moves(game_state):
        def append_points(legal_moves, m, s):
            R, C = Board.RS[s], Board.CS[s]
            for i in xrange(9):
                if (m | Board.BOARDS[i]) == m:
                    legal_moves.append((R, C, Board.RS[i], Board.CS[i]))
        legal_moves = []
        n = game_state[22]
        if game_state[32 + n]:
            for index in xrange(9):
                if not game_state[32 + index]:
                    append_points(legal_moves, game_state[23 + index], index)
        else:
            append_points(legal_moves, game_state[23 + n], n)
        return legal_moves

    @staticmethod
    def display(game_state):
        line = [["-" for i in xrange(9)] for i in xrange(9)]
        for N in xrange(9):
            I = game_state[N * 2]
            A = game_state[N * 2 + 1]
            for n in xrange(9):
                if ((I >> n) & 1) == 1:
                    line[int(N / 3) * 3 + int(n / 3)][(N % 3) * 3 + n % 3] = "X"
                if ((A >> n) & 1) == 1:
                    line[int(N / 3) * 3 + int(n / 3)][(N % 3) * 3 + n % 3] = "O"
        for i in xrange(9):
            for j in xrange(9):
                if j == 8:
                    print line[i][j] + " "
                else:
                    print line[i][j] + " ",
                    if (j + 1) % 3 == 0:
                        print " ",
            if (i + 1) % 3 == 0:
                print "---"

    @staticmethod
    def is_win(n):
        for i in Board.WINS:
            if (n & i) == i:
                return True
        return False
