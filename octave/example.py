#!/usr/bin/env python2

import sys
import math

def ffloor(v):
    xs = [math.floor(x + 0.5) ** 2 for x in v]
    return sum(xs)

logfh = open("log.txt", "wt")

while True:
    l = sys.stdin.readline()
    print >>logfh, "IN \"" + l + "\""
    ls = l.split()
    if ls == []:
        exit(1)
    kwd = ls[0]
    if kwd == "parameter":
        #      parameter NAME   N NOISE Tol x0  s0  x1  s1  ...
        print "parameter ffloor 5 exact 1e-4 10.0 5.0 10.0 0.5 5.0 1.0 10.0 1.0 5.0 1.0"
        sys.stdout.flush()
    elif kwd == "evaluate":
        pts = int(ls[1])
        vvs = []
        for i in range(pts):
            l = sys.stdin.readline()
            print >>logfh, "DATAIN \"" + l + "\""
            lcur = l.split()
            vs = [float(v) for v in lcur]
            vvs.append(vs)
        rets = [ffloor(vs) for vs in vvs]
        print "evaluate " + " ".join([str(x) for x in rets])
        sys.stdout.flush()
        print >>logfh, "OUT evaluate " + " ".join([str(x) for x in rets])
    elif kwd == "exit":
        exit(0)
    else:
        exit(1)
