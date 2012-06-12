#!/usr/bin/env python

import os
import os.path
import sys

storage = sys.argv[1]
leave = set(sys.argv[2:])
leave.add('data')

files = os.listdir(storage)
for f in files:
    if f not in leave:
        os.unlink(os.path.join(storage, f))
