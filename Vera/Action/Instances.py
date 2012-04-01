from __future__ import print_function

import sys

import Miranda
import Config

def perform(instances):
    try:
        m = Miranda.connect()
        m.send('set')
        m.send('app::instances::{0}'.format(Config.name))
        m.send(instances)
        m.close()
    except Exception as e:
        print('Miranda communication error: {0}'.format(e))
        sys.exit(1)
