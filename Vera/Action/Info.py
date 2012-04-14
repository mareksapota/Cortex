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

def runningInstances():
    m = Miranda.connect()
    m.send('lookup all')
    m.send('app::instance::{0}'.format(Config.name))
    response = m.recv_all()
    m.close()
    return len(response.split('\n'))

def perform():
    try:
        print(Config.name, 'settings')
        print()
        print('Application type:', lookup('type'))
        print('Port:', lookup('port'))
        print('Instances:', lookup('instances'))
        print('Running:', runningInstances())
    except Exception as e:
        print('Miranda communication error: {0}'.format(e))
        sys.exit(1)
