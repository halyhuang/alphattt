# -*- coding: UTF-8 -*-
import time
from comm import SocketClient
from py_interface.erl_term import BinaryToTerm
from py_interface.erl_term import ErlAtom
from py_interface.erl_term import TermToBinary
from py_interface.erl_term import ErlTuple
from py_interface.erl_term import ErlString
from py_interface.erl_term import ErlNumber

from board import Board

class Client(object):
    def __init__(self, ip, port, user_name, pass_word):
        socket_client = SocketClient(ip, port)
        socket_client.setDaemon(True)
        socket_client.start()
        self.socket_client = socket_client
        self.user_name = user_name
        self.pass_word = pass_word
    
    def login(self):
        self.socket_client.send(TermToBinary(
            ErlTuple([ErlAtom("login"), ErlString(self.user_name), ErlString(self.pass_word)])))
    
    
    def enter_room(self, room_id):
        self.socket_client.send(TermToBinary(
            ErlTuple([ErlAtom("enter_room"), ErlString(self.user_name), ErlNumber(room_id)])))
    
    
    def send_move(self, move):
        self.socket_client.send(TermToBinary(ErlTuple([ErlAtom("play"), ErlTuple(move)])))
    
    def set_recv_callback(self, player):
        self.socket_client.set_recv_callback(self.make_recv(player))
    
    def make_recv(self, player):
        def _recv(data):
            msg = BinaryToTerm(data)
            print msg
            if type(msg) is tuple and msg[0].equals(ErlAtom("update")):
                if type(msg[1]) is tuple:
                    player.update(msg[1], msg[2])
                else:
                    player.update(None, msg[2])
                player.display(msg[2])  
            elif type(msg) is tuple:
                pass
            elif msg.equals(ErlAtom("play")):
                move = player.get_move()
                print "My move:", move, type(move)
                self.send_move(move)
        return _recv

from abc import abstractmethod
class Player(object):
    def __init__(self, cal_time, board):
        self.cur_opponent_state = board.start()
        self.cur_opponent_move = None
        self.board = board
        self.cal_time = cal_time
    
    def update(self, move, game_state):
        self.cur_opponent_state = game_state
        
    def display(self, game_state):
        self.board.display(game_state)
    
    @abstractmethod
    def get_move(self):
        pass
    
def run(remote_ip, remote_port, user_name, pass_word, room_id, player, cal_time=1, board=Board(), wait_sec=1000):
    player = player(cal_time, board)
    client = Client(remote_ip, remote_port, user_name, pass_word)
    client.set_recv_callback(player)
    client.login()
    client.enter_room(room_id)
    time.sleep(wait_sec)
if __name__ == '__main__':
    board = Board()
    board.display((0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2))
