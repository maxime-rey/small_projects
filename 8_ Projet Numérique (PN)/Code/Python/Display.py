# -*- coding: utf-8 -*-
"""
Created on Mon Dec 10 21:59:06 2018

@author: Maxime
"""
#from tp_utils import *
from matplotlib.pyplot import *
from numpy import *

i=0
# =============================================================================
# A=[[1,2],[3,4],[5,6]]
# print A.split('\t')
# =============================================================================
A=[]
Img  = open("Test.txt", mode = 'r')
for line in Img:
    i=i+1
    A.append(line.split('\t'))
print A[3][1]

    #print line   #\t = tab, \n = endline
Img.close()

#fig = figure()
#imshow(conjgrad(A, b, maxiter=200, grtol=1e-7), vmin=0, cmap="Greys_r")

















