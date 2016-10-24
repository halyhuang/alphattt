from py_robot.pybot_module import PybotModule


class RandomBot(PybotModule):
    pass


if __name__ == '__main__':
    from py_robot.board import Board
    from py_robot.client import Client

    random_bot = RandomBot(1, Board)
    state = {"state": Board.start()}
    client = Client("10.9.88.20", 8011, random_bot, state)
    client.play("pybot", "1234", 3)
