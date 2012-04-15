#!/usr/bin/env python

from __future__ import print_function

import os
import multiprocessing
import psutil

# Normalized CPU load in last minute.
cpus = float(multiprocessing.cpu_count())
cpu_load = os.getloadavg()[0] / cpus
# Memory usage.
mem_load = psutil.phymem_usage()[3] / 100.0

load = max(cpu_load, mem_load)
print(load)
