# -*- coding: UTF-8 -*-
import time
from tcp_client_ttt import SocketClient
from py_interface.erl_term import BinaryToTerm
from py_interface.erl_term import ErlAtom
from py_interface.erl_term import TermToBinary
from py_interface.erl_term import ErlTuple
from py_interface.erl_term import ErlString
from py_interface.erl_term import ErlNumber

from pybot import Pybot
from board import Board


def init(ip, port):
    socket_client = SocketClient(ip, port)
    socket_client.setDaemon(True)
    socket_client.start()
    return socket_client


def login(socket_client, user_name, password):
    socket_client.send(TermToBinary(
        ErlTuple([ErlAtom("login"), ErlString(user_name), ErlString(password)])))


def enter_room(socket_client, user_name, id):
    socket_client.send(TermToBinary(
        ErlTuple([ErlAtom("enter_room"), ErlString(user_name), ErlNumber(id)])))


def send_move(socket_client, move):
    socket_client.send(TermToBinary(ErlTuple([ErlAtom("play"), ErlTuple(move)])))


def make_recv(player, state, socket_client):
    def _recv(data):
        msg = BinaryToTerm(data)
        print msg
        if type(msg) is tuple and msg[0].equals(ErlAtom("update")):
            print "pybot update: ", msg
            if type(msg[1]) is tuple:
                state["state"] = player.board.next_state(state["state"], msg[1])
        elif type(msg) is tuple:
            pass
        elif msg.equals(ErlAtom("play")):
            print "pybot move start"
            move, _, _ = player.get_move(state["state"])
            print "pybot move: ", move
            player.board.display(state["state"])
            send_move(socket_client, move)
    return _recv


if __name__ == '__main__':
    pybot = Pybot(1, Board)
    state = {"state": Board.start()}
    socket_client = init("10.9.88.20", 8011)
    socket_client.set_recv_callback(make_recv(pybot, state, socket_client))
    login(socket_client, "pybot", "1234")
    enter_room(socket_client, "pybot", 3)
    time.sleep(10000)
