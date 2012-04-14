from __future__ import print_function

import sys

import Miranda
import Config

def perform(port):
    try:
        m = Miranda.connect()
        m.send('set')
        m.send('app::port::{0}'.format(Config.name))
        m.send(port)
        m.close()
    except Exception as e:
        print('Miranda communication error: {0}'.format(e))
        sys.exit(1)
