#!/usr/bin/env python

from __future__ import print_function

import os
import multiprocessing

# Load in last minute.
load = os.getloadavg()[0]

cpus = float(multiprocessing.cpu_count())

# Normalized CPU load.
print(load / cpus)
