#!/usr/bin/env python

from __future__ import print_function

import os
import multiprocessing

# Load in last five minutes.
load = os.getloadavg()[1]

cpus = float(multiprocessing.cpu_count())

# Normalized CPU load.
print(load / cpus)
