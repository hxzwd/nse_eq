#!/usr/local/bin/ipython
#-*- coding: utf-8 -*-


import os
import sys
import re

import subprocess

import inspect
import datetime
import argparse

from IPython import embed

rep_fn = sys.argv[1]
curr_dir = os.getcwd()

os.chdir("reports")

f = open(rep_fn, "r")
d = f.read()
f.close()

new_d = re.sub(r"(\\text{a)([0-9]+)(})", "a_{\\2}", d)

f = open(rep_fn, "w")
f.write(new_d)
f.close()


cmd_str = "echo -e \"\n\n\n\n\" | pdflatex {}".format(rep_fn)
os.system(cmd_str)

os.chdir(curr_dir)




