from __future__ import print_function

import sys
import os.path

import Config
import Miranda
import Action.Instances

def perform(app_type):
    # Perform application type discovery if needed.
    if app_type == 'auto':
        if Config.app_type is not None:
            print('Using application type from `.veraconf`.')
            app_type = Config.app_type
        elif os.path.exists('config.ru'):
            print('Discovered Rails application')
            app_type = 'rails'
        else:
            print('Could not determine application type.')
            sys.exit(1)

    try:
        m = Miranda.connect()
        m.send('set')
        m.send('app::type::{0}'.format(Config.name))
        m.send(app_type)
        m.close()
    except Exception as e:
        print('Miranda communication error: {0}'.format(e))
        sys.exit(1)

    Action.Instances.perform(0)

    print('Successfully set up {0}.'.format(Config.name))
