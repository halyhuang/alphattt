# -*- coding: UTF-8 -*-
import time
from tcp_client_ttt import SocketClient
from py_interface.erl_term import BinaryToTerm
from py_interface.erl_term import ErlAtom
from py_interface.erl_term import TermToBinary
from py_interface.erl_term import ErlTuple
from py_interface.erl_term import ErlString
from py_interface.erl_term import ErlNumber


class Client(object):
    def __init__(self, ip, port, player, state):
        super(Client, self).__init__()
        self.ip = ip
        self.port = port
        self.socket_client = self.__init(ip, port)
        self.socket_client.set_recv_callback(self.__make_recv(player, state, self.socket_client))

    def play(self, username, password, roomID):
        self.__login(self.socket_client, username, password)
        self.__enter_room(self.socket_client, username, roomID)
        time.sleep(10000)

    def __init(self, ip, port):
        socket_client = SocketClient(ip, port)
        socket_client.setDaemon(True)
        socket_client.start()
        return socket_client

    def __login(self, socket_client, user_name, password):
        socket_client.send(TermToBinary(
            ErlTuple([ErlAtom("login"), ErlString(user_name), ErlString(password)])))

    def __enter_room(self, socket_client, user_name, id):
        socket_client.send(TermToBinary(
            ErlTuple([ErlAtom("enter_room"), ErlString(user_name), ErlNumber(id)])))

    def __send_move(self, socket_client, move):
        socket_client.send(TermToBinary(ErlTuple([ErlAtom("play"), ErlTuple(move)])))

    def __make_recv(self, player, state, socket_client):
        def _recv(data):
            msg = BinaryToTerm(data)
            print msg
            if type(msg) is tuple and msg[0].equals(ErlAtom("update")):
                # print "robot update: ", msg
                if type(msg[1]) is tuple:
                    state["state"] = player.board.next_state(state["state"], msg[1])
                    player.board.display(state["state"])
            elif type(msg) is tuple:
                pass
            elif msg.equals(ErlAtom("play")):
                move, msg_time, msg_pro = player.get_move(state["state"])
                print "robot move: ", move
                print msg_time
                print msg_pro
                self.__send_move(socket_client, move)
        return _recv
