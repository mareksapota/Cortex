host = None
port = None

import socket

def init(h, p):
    global host, port
    host = h
    port = p

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

    def close(self):
        self.f.close()
        self.s.close()

def connect():
    return MirandaConnection()
