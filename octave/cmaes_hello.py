#!/usr/bin/env python2
import cma
import numpy as np

def myfunc(xs):
  sum = 0
  for i in xrange(len(xs)):
    sum += (xs[i]**2-i)**2
  return sum


res = cma.fmin(myfunc, [0,0,0,0,0], 0.1)
