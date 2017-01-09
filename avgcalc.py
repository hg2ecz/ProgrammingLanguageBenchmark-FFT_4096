#!/usr/bin/python

import os
import sys

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

awg = snum/float(i)

var = 0
for num in nums:
    var += (num-awg)**2

var /= float(i)

print "----> %s (%s): AWG: %.3f VAR: %.5f"%(os.getcwd().split('/')[-1], param, awg, var)
