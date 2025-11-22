#!/bin/python

import numpy as np
import matplotlib.pyplot as plt
import sys
import shutil
import os
import subprocess

pref = "bezNumeru-"
dst = pref+os.path.basename(sys.argv[1])
#print(dst)

img = plt.imread(sys.argv[1])

srednia = img.mean(1)
#print(list(srednia))
#print(srednia.shape)
#print(img.shape)

do_uciecia = 0
do_uciecia2 = 0
licznik1 = 0
while srednia[-1 - do_uciecia] == 1.0:
    do_uciecia += 1
while srednia[-1 - do_uciecia] >0.97 and srednia[-1 - do_uciecia] != 1.0:
    do_uciecia += 1
while srednia[-1 - do_uciecia - do_uciecia2] >0.83 and srednia[-1 - do_uciecia - do_uciecia2] != 1.0:
    do_uciecia2 += 1

#print(srednia[-21 - do_uciecia : -1 - do_uciecia])
#print(srednia[-21 - do_uciecia : -1 - do_uciecia]==1.0)

if np.all(srednia[-101 - do_uciecia - do_uciecia2 : -1 - do_uciecia - do_uciecia2]==1.0):
    subprocess.run(f"magick {sys.argv[1]} +repage -gravity South -chop 0x{do_uciecia + do_uciecia2} -trim {dst}", shell = True)
elif np.all(srednia[-21 - do_uciecia : -1 - do_uciecia]==1.0):
    subprocess.run(f"magick {sys.argv[1]} +repage -gravity South -chop 0x{do_uciecia} -trim {dst}", shell = True)
else:
    shutil.copy(sys.argv[1], dst)
