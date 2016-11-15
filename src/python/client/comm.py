# -*- coding: UTF-8 -*-
import socket
import select
import threading
import sys
import Queue
from struct import unpack, pack

class SocketClient(threading.Thread):
    def __init__(self, ip, addr):
        super(SocketClient, self).__init__()
        self.addr = (ip, addr)
        self.running = True
        self.write_message_queue = Queue.Queue()
        self.rec_callback = None

    def init_listen_socket(self):
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.connect(self.addr)

    def run(self):
        try:
            self.init_listen_socket()
        except:
            print('Error happened!' + str(sys.exc_info()))

        while self.running:
            try:
                readable, writable, exceptional = select.select(
                    [self.socket], [self.socket], [self.socket], 0.01)
                if not (readable or writable or exceptional):
                    continue
                self.handle_readable(readable)
                self.handle_writable(writable)
                self.handle_exception(exceptional)
            except SystemExit:
                self.stop_now()
                print('System Exit, close channel [port]=' + self.port)

    def handle_readable(self, readable):
        for s in readable:
            self.recv_data(s)

    def handle_writable(self, writable):
        for s in writable:
            while True:
                try:
                    next_msg = self.write_message_queue.get_nowait()
                    s.send(next_msg)
                except Queue.Empty:
                    break

    def handle_exception(self, exceptional):
        for s in exceptional:
            self.remove_socket(s)

    def send(self, msg):
        length = len(msg)
        msg_with_length = pack('>1H', length) + msg
        self.write_message_queue.put(msg_with_length)

    def recv_data(self, recv_socket):
        head = recv_socket.recv(2)
        if len(head) > 0:
            length, = unpack('>1H', head)
            recv_data = recv_socket.recv(length)
            self.rec_callback(recv_data)

    def remove_socket(self, s):
        try:
            s.shutdown(socket.SHUT_RDWR)
        except:
            pass
        try:
            s.close()
        except:
            pass

    def stop_now(self):
        print('Channel closed, close socket, [port]=' + str(self.port))
        self.running = False
        self.release_socket(self.socket)

    def set_recv_callback(self, recv_callback):
        self.rec_callback = recv_callback

