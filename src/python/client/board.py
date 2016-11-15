# -*- coding: UTF-8 -*-


class Board(object):
    PlAYER_1_WIN = 1
    PLAYER_2_WIN = 2
    DRAW = 3
    ON_GOING = 4
    
    game_state = [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, None, None, 1 ]
    wins = [1 + 2 + 4, 8 + 16 + 32, 64 + 128 + 256, 1 + 8 + 64, 2 + 16 + 128, 4 + 32 + 256,
            1 + 16 + 256, 4 + 16 + 64]
    player1_pos = {(0, 0) : 0,
                       (0, 1) : 2,
                       (0, 2) : 4,
                       (1, 0) : 6,
                       (1, 1) : 8,
                       (1, 2) : 10,
                       (2, 0) : 12,
                       (2, 1) : 14,
                       (2, 2) : 16}
    player2_pos = {(0, 0) : 1,
                       (0, 1) : 3,
                       (0, 2) : 5,
                       (1, 0) : 7,
                       (1, 1) : 9,
                       (1, 2) : 11,
                       (2, 0) : 13,
                       (2, 1) : 15,
                       (2, 2) : 17}
    pos_value_list = [1, 2, 4, 8, 16, 32, 64, 128, 256]
    pos_value = {(0, 0) : 1,
                 (0, 1) : 2,
                 (0, 2) : 4,
                 (1, 0) : 8,
                 (1, 1) : 16,
                 (1, 2) : 32,
                 (2, 0) : 64,
                 (2, 1) : 128,
                 (2, 2) : 256}
    lineIndex2grid = {0 : 2, 1 : 4, 2 : 6, 3 : 9, 4 : 11, 5 : 13, 6 : 16, 7 : 18, 8 : 20}
    index2pos = {0: (0, 0),
                 1: (0, 1),
                 2: (0, 2),
                 3: (1, 0),
                 4: (1, 1),
                 5: (1, 2),
                 6: (2, 0),
                 7: (2, 1),
                 8: (2, 2)}
    def start(self): 
        return list(self.game_state)

    def next_state(self, state, (lr, lc, r, c)):
        res = list(state) 
        player = res[22]
        gridIndex = self.player1_pos[(lr, lc)] if player == 1 else self.player2_pos[(lr, lc)]
        gridValue = self.pos_value[(r, c)]
        res[gridIndex] = res[gridIndex] | gridValue
        res[18] = self.judgeBigWin(1, res, lr, lc)
        res[19] = self.judgeBigWin(2, res, lr, lc)
        res[20] = r
        res[21] = c
        res[22] = 2 if player == 1 else 1
        return res
    
    def judgeBigWin(self, player, state, lr, lc):
        res = 0
        if player == 1:
            if(self.isWin(state[self.player1_pos[(lr, lc)]])):
                res = res | self.pos_value[(lr, lc)]
        if player == 2:
            if(self.isWin(state[self.player2_pos[(lr, lc)]])):
                res = res | self.pos_value[(lr, lc)]
        return res

    def currentPlayer(self, state):
        return state[22]
    
    def legal_moves(self, state):
        if (type(state[20]) is not int or type(state[21]) is not int):
            return self.findAllLegal(state)

        player1GridIndex = self.player1_pos[(state[20], state[21])]
        player2GridIndex = self.player2_pos[(state[20], state[21])]
        player1GirdValue = state[player1GridIndex]
        player2GridValue = state[player2GridIndex]
        if (self.isWin(player1GirdValue) or self.isWin(player2GridValue) or self.isGridFull(player1GirdValue | player2GridValue)) :
            return self.findAllLegal(state)

        return self.findGirdLegalMoves(state[player1GridIndex], state[player2GridIndex], state[20], state[21])

    def max_moves(self):
        return 81;
    
    def winner(self, state):
        if(self.isWin(state[18])):
            return self.PlAYER_1_WIN
        
        if(self.isWin(state[19])):
            return self.PLAYER_2_WIN
        
        if(self.isFull(state)):
            return self.DRAW
        
        return self.ON_GOING
    
    def isFull(self, state):
        for i in range(0, 9):
            total = state[i * 2] + state[i * 2 + 1];
            if((not self.isGridFull(total)) and (not self.isWin(state[i * 2])) and (not self.isWin(state[i * 2 + 1]))):
                return False;
        return True;
    
    def isGridFull(self, i):
        return i == 511;

    def findGirdLegalMoves(self, player1value, player2value, gridx, gridy):
        res = []
        total = player1value | player2value
        for r, c in self.pos_value.keys():
            if total & self.pos_value[(r, c)] == 0 :
                res.append([gridx, gridy, r, c])
        return res;

    def findAllLegal(self, state):
        res = []
        for i in range(0, 9):
            player1GirdValue = state[i*2]
            player2GridValue = state[i*2 + 1]
            if (self.isWin(player1GirdValue) or self.isWin(player2GridValue) or self.isGridFull(player1GirdValue | player2GridValue)):
                continue
            r, c = self.index2pos[i]
            res +=self.findGirdLegalMoves(player1GirdValue, player2GridValue, r, c);
        return res

    def isWin(self, i):
        for win in self.wins:
            if ((i & win) == win):
                return True;
        return False
    
    def display(self, state):
        wholeGrid = [[0 for x in range(0, 9)] for x in range(0, 9)]
        for i in range(0, 9):
            grid = self.parseGrid(state[2 * i], state[2 * i + 1]);
            self.copyGrid(wholeGrid, grid, i);

        for line in range(0, len(wholeGrid)):
            if (line % 3 == 0):
                self.displayBorder();
            self.displayOneLine(wholeGrid[line]);
        self.displayBorder();

    def copyGrid(self, wholeGrid, grid, i):
        offsetX = (i / 3) * 3;
        offsetY = (i % 3) * 3;
        for xi in range(0, 3):
            for yi in range(0, 3):
                wholeGrid[xi + offsetX][yi + offsetY] = grid[xi][yi]

    def parseGrid(self, player1, player2):
        res = [[0, 0, 0 ], [ 0, 0, 0 ], [ 0, 0, 0 ]]
        for i in range(len(self.pos_value_list) - 1, -1, -1):
            player1 = self.matchStep(player1, self.pos_value_list[i], i, 1, res);
            player2 = self.matchStep(player2, self.pos_value_list[i], i, 2, res);
        return res

    def matchStep(self, player, value, pos, desc, res):
        if (player / value > 0) :
            player = player - value;
            res[pos / 3][pos % 3] = desc;
        return player;

    def displayOneLine(self, line):
        s = list("||     ||     ||     ||")
        for i in range(0, len(line)):
            if (line[i] == 1):
                s[self.lineIndex2grid.get(i)] = 'x';
            elif (line[i] == 2):
                s[self.lineIndex2grid.get(i)] = 'o';
        print "".join(s)

    def displayBorder(self):
        print("----------------------")
