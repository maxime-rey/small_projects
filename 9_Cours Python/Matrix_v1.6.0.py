#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Display a Matrix-like window.
Can specify characteristics in VarMatrix.py
Special feature (Add-on): umbrella
"""

import tkinter as tk
import numpy as np
import time
import random
import copy

from VarMatrix import *

############################################################################################################################## Def main variables
grid = np.full((m,n),None)
droplength = np.full((m,n,3),0,int)   # 0 = length, 1 = color variable, 2=dropthatwentthrough
colors = colrain + columb

fenetre = tk.Tk()
canvas = tk.Canvas(fenetre, width=width, height=height, background='black')

while True:
    gridtemp = copy.deepcopy(grid)
    droplengthtemp = copy.deepcopy(droplength[:,:,0])

############################################################################################################################## Building evolutive grid
    for j in range (m):
        for i in range (n):
            if isinstance(gridtemp[i,j], int)==True:                #If drop there is
                if droplength[i,j,2]==droplengthtemp[i,j]:           #If end -> No more droplet, length and drops that went through reset
                    grid[i,j] = None
                    droplength[i,j,0] = 0
                    droplength[i,j,2] = 0
                else:                                                   #If not end  -> New value
                    grid[i,j]=random.randint(0,9)
                    droplength[i,j,2] += 1
            else:                                                   #If no drop
                if j==0:                                                # If first line -> Generate one and a length
                    if random.uniform(0,1)<probadrop:                       
                        grid[i,j]=random.randint(0,9)
                        droplength[i,j,0]=random.randint(minsize-1,maxsize-1)
                        droplength[i,j,1]= random.randint(0,len(colors)-2)
                    else:                      
                        grid[i,j]=None
                        droplength[i,j,0]=0
                else:                                                   #If any other line
                    if isinstance(gridtemp[i,j-1], int)==True:              #If drop in previous cell -> new drop and get length
                        grid[i,j]=random.randint(0,9)
                        droplength[i,j,0]=droplength[i,j-1,0]
                        droplength[i,j,1]=droplength[i,j-1,1]
                    else:                      
                        grid[i,j]=None
                        droplength[i,j,0]=0

############################################################################################################################## Umbrella
    if addonumb!=0:                                                                 #Add-on or not ?
        coord_x = fenetre.winfo_pointerx() - fenetre.winfo_rootx()
        coord_y = fenetre.winfo_pointery() - fenetre.winfo_rooty()
        coordpix_x = int(coord_x*n/width)
        coordpix_y = int(coord_y*m/height)
        if (coord_x>0 and coord_x<width and coord_y>0 and coord_y<height):
            for i in range (coordpix_x-umbsize,coordpix_x+umbsize+1):
                for j in range (coordpix_y-umbsize,coordpix_y+umbsize):
                    if j==coordpix_y-umbsize:
                        if (0<i<n and 0<j<m):                                       #Out of boundaries ?
                            if (i==coordpix_x-umbsize):                             #Left side of the umbrella
                                grid[i,j]='{¯¯'
                            elif (i==coordpix_x+umbsize):                           #Right side of the umbrella
                                grid[i,j]='¯¯}'
                            elif (i==coordpix_x):                                   #Junction btw top and middle of the umbrella
                                grid[i,j]='¯T¯'
                            else:
                                grid[i,j]='¯¯¯¯'
                            droplength[i,j,1]=len(colors)-1
                    elif (0<i<n and 0<j<m):
                        grid[i,j]=None
                if i==coordpix_x:                                                   # Middle of umbrella
                    for j in range (coordpix_y-umbsize+1,coordpix_y+3):
                        if (0<i<n and 0<j<m):
                            grid[i,j]='||'
                            droplength[i,j,1]=len(colors)-1
                    
############################################################################################################################## Display
    for i in range (n):
        for j in range (m):
            canvas.create_text(i*width/n, j*height/m+5, text=grid[i,j], font="Arial 8", fill=colors[droplength[i,j,1]])
    canvas.pack()
    time.sleep(rainspeed)
    fenetre.update()
    canvas.delete("all")
fenetre.mainloop()