from __future__ import print_function

import sys

import Miranda

def perform(node_info):
    hp = node_info.split(':')
    try:
        assert len(hp) == 2
        host = hp[0]
        port = int(hp[1])
    except:
        print('Couldn\'t parse given node information')
        sys.exit(1)

    try:
        m = Miranda.connect()
        m.send('set')
        m.send('host::availability::{0}:{1}'.format(host, port))
        m.send('online')
        m.close()
    except Exception as e:
        print('Miranda communication error: {0}'.format(e))
