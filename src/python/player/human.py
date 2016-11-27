# -*- coding: UTF-8 -*-
from client.client import Player
from client.client import run

class Human(Player):
    def get_move(self):
        move = raw_input("Enter your input like (r,c,r,c):")
        try:
            res_move = map(lambda x : int(x), move.strip().split(","))
            return res_move
        except:
            self.get_move()

if __name__ == '__main__':
    run("127.0.0.1", 8011, "TT", "123456", 3, Human)