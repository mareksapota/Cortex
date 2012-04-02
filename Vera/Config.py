from __future__ import print_function

import sys
import os.path
import ConfigParser

name = None
app_type = None
miranda_host = None
miranda_port = None

def init():
    global name, app_type, miranda_host, miranda_port

    if not (os.path.exists('.veraconf') and os.path.isfile('.veraconf')):
        print('You have to create ".veraconf" first.')
        sys.exit(1)

    p = ConfigParser.SafeConfigParser()
    p.read('.veraconf')

    try:
        name = p.get('app', 'name')
    except:
        print('Application name is not defined.')
        sys.exit(1)

    try:
        app_type = p.get('app', 'type')
    except:
        pass

    try:
        miranda_host = p.get('miranda', 'host')
    except:
        pass

    try:
        miranda_port = p.getint('miranda', 'port')
    except:
        pass
