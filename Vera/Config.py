from __future__ import print_function

import sys
import os.path
import ConfigParser

name = None

def init():
    global name

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
