#!/usr/bin/pypy

import os
import sys
import math

param = 'none'
if len(sys.argv) > 1:
    param = sys.argv[1]

i=0
snum = 0
nums=[]
for row in sys.stdin:
    if 'piece' in row:
	num = float(row.split(';')[1].split()[0].replace(',', '.'))
	nums.append(num)
	snum += num
	i+=1

avg = snum/float(i)

var = 0
for num in nums:
    var += (num-avg)**2

var = math.sqrt(var/float(i-1))

print "----> %s (%s) ----> AVG: %.3f VAR: %.5f RELVAR: %.2f %%"%(os.getcwd().split('/')[-1], param, avg, var, 100.*var/avg)
