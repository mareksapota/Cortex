#!/usr/bin/env python
#
# This script runs a sub-process and translates SIGTERM and SIGINT to a SIGKILL.

import signal
import subprocess
import sys
import os

location = sys.argv[1]

p = subprocess.Popen(sys.argv[2:], cwd = location, preexec_fn = os.setsid)

def signal_handler(singnal, frame):
    os.killpg(p.pid, signal.SIGKILL)
signal.signal(signal.SIGINT, signal_handler)
signal.signal(signal.SIGTERM, signal_handler)

r = p.wait()
sys.exit(r)
