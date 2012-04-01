from __future__ import print_function

import sys

import Miranda
import Config

def lookup(key):
    m = Miranda.connect()
    m.send('lookup')
    m.send('app::{0}::{1}'.format(key, Config.name))
    response = m.recv()
    m.close()
    if response == 'Nothing':
        return None
    else:
        # Drop the 'Just '.
        return response[5:]

def perform():
    try:
        print(Config.name, 'settings')
        print()
        print('Application type:', lookup('type'))
        print('Instances:', lookup('instances'))
    except Exception as e:
        print('Miranda communication error: {0}'.format(e))
        sys.exit(1)
