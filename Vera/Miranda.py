from __future__ import print_function

import sys
import socket

import Config

host = None
port = None

def init(h, p):
    global host, port
    if h is None:
        host = Config.miranda_host
    else:
        host = h
    if p is None:
        port = Config.miranda_port
    else:
        port = p
    if host is None or port is None:
        print(
            'You have to specify Miranda host and port, either in '
            '`.veraconf` or on the command line.'
        )
        sys.exit(1)

class MirandaConnection():
    def __init__(self):
        self.s = socket.create_connection((host, port, ))
        self.f = self.s.makefile('rw')

    def send(self, msg):
        self.send_part(msg)
        self.send_part_finish()

    def send_part(self, msg):
        self.f.write(msg)

    def send_part_finish(self):
        self.f.write('\n')
        self.f.flush()

    def recv(self):
        msg = self.f.readline()
        # Remove new line character from msg.
        return msg[:-1]

    def recv_all(self):
        return self.f.read()

    def close(self):
        self.f.close()
        self.s.close()

def connect():
    return MirandaConnection()
