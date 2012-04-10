#!/usr/bin/env python

from __future__ import print_function

import tempfile
import os
import subprocess
import sys
import argparse
import socket

#####
# Command line options.
parser = argparse.ArgumentParser()
parser.add_argument(
    'host',
    help = 'Host on which Miranda runs.'
)
parser.add_argument(
    'port',
    type = int,
    help = 'Port on which Miranda runs.'
)
parser.add_argument(
    'app_name',
    help = 'Which application source should be downloaded.'
)
parser.add_argument(
    'location',
    help = 'Where the application should be downloaded.'
)
#####

#####
# Parse the command line options.
args = parser.parse_args()
#####

# Create a temporary file to store the repo.
(fd, tmp, ) = tempfile.mkstemp(suffix = '.tar')
os.close(fd)
f = open('{0}.base64'.format(tmp), 'w')
f.close()

try:
    key = 'app::source::{0}'.format(args.app_name)

    s = socket.create_connection((args.host, args.port, ))
    f = s.makefile('rw')

    f.write('lookup\n')
    f.write(key)
    f.write('\n')
    f.flush()

    # Drop the 'Just ' prefix.
    f.read(5)

    t = open('{0}.base64'.format(tmp), 'w')
    while True:
        data = f.read(4096)
        t.write(data)
        if not data:
            break
    t.close()

    f.close()
    s.close()

    subprocess.check_call(
        'base64 -d "{0}.base64" > "{0}"'.format(tmp),
        shell = True
    )

    subprocess.check_call(
        'tar -xf "{0}" -C {1}'.format(tmp, args.location),
        shell = True
    )
except Exception as e:
    print('Source download error: {0}'.format(e))
    sys.exit(1)
finally:
    # Remove the temporary file.
    os.unlink('{0}'.format(tmp))
    os.unlink('{0}.base64'.format(tmp))
