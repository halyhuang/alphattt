import random
from py_robot.pybot_module import PybotModule


class RandomBot(PybotModule):

    def get_move(self, state):

        move = random.choice(self.board.legal_moves(state))
        msg_time = "== calculate 1 paths using 1.0 seconds =="
        msg_pro = "== probability is 100. 1/1 =="
        return move, msg_time, msg_pro


if __name__ == '__main__':
    from py_robot.board import Board
    from py_robot.client import Client

    random_bot = RandomBot(1, Board)
    state = {"state": Board.start()}
    client = Client("10.9.88.20", 8011, random_bot, state)
    client.play("pybot", "1234", 3)
