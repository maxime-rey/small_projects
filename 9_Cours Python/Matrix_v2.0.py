#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import tkinter as tk
import numpy as np
import time
import random
import copy

from VarMatrix2 import *

"""
Display a Matrix-like window.
Can specify characteristics in VarMatrix.py
Special feature (Add-on): umbrella
"""

def init_color_grid(grid):        # as numpy array initialised as None, need to put all the color variables to an int
    for i in range (pix_width):
        for j in range (pix_height):
            grid[i,j,1] = 0

def create_newdrop(grid, colors, drop_list, pix_width, probadrop, drops_nb, max_drops_nb, minsize, maxsize):
    """
    Creates a new drop on the first line of the grid
    """

    # from VarMatrix import *      # DO I ??? Check other works
    for i in range (pix_width):
        drop_id = np.where(drop_list[:,0] == 0)
        if len(np.ravel(np.ravel(drop_id)))!=0:
            drop_id = np.ravel(np.ravel(drop_id))[0]
        if isinstance(drop_id,np.int64):

            if is_drop_here(grid, i, 0)==False and random.uniform(0,1)<probadrop/50: 
                # if there's no drop, droplist not full & proba is ok
                #drop_id = np.where(drop_list[:,0] == 0)      ############################################################### please, alternative qui prends le premier st I can rm following if
                drop_list[drop_id, :] = (random.randint(minsize-1,maxsize-1), i, 0)  # assign (length, col, row)
                drops_nb+=1
                put_drop(drop_list, grid, colors, drop_id)



def drops_forward(drop_list, grid, pix_height):
    """
    Makes the drop (i.e. all the droplets composing it) go down by one pixel. 
    Remove the droplet on the tail and if the head is at the end of the grid, delete it. 
    """

    for n in range (len(drop_list)):
        if drop_list[n,0]!=0:
            if drop_list[n,2]+1==pix_height:                       # if at end
                drop_list[n,0] -= 1                       # lose 1 pixel of length
            else:                                     # else
                drop_list[n,2] += 1                        # front of drop moves forward

            length_drop = drop_list[n,0]
            head_drop = drop_list[n,2]
            
            if head_drop-length_drop-1>=0:           # removes tail, otherwise preious numbers stay ############""""""potentially 0
                tail = head_drop-length_drop-1
                col_pos = drop_list[n,1]
                grid[col_pos,tail,0] = None
            put_drop(drop_list, grid, colors, n)


def is_drop_here(grid, i, j):
    """ Returns True if there is a drop and False otherwise """

    if grid[i,j,0]==None:          # valeur du pixel
        return False
    else:
        return True


def put_drop(drop_list, grid, colors, n):     # a drop is composed of droplets (1 per pix)
    """ Add the droplets corresponding to a drops (in drop_list) on the grid """

    length_drop = drop_list[n,0]
    col_pos = drop_list[n,1]       # column of the drop
    head_drop = drop_list[n,2]
    for i in range (max(0,head_drop-length_drop), head_drop):   # max needed when drop isn't fully shown yet
        grid[col_pos,i,0] = random.randint(0,9)                # Assign random value
        grid[col_pos,i,1] = random.randint(0,len(colors)-2)    # Assign random color

def add_umbrella(grid, colors):

    from VarMatrix import pix_width, win_width, pix_height, win_height, umbsize

    if addonumb!=0:                                                                 #Add-on or not ?
        coord_x = fenetre.winfo_pointerx() - fenetre.winfo_rootx()
        coord_y = fenetre.winfo_pointery() - fenetre.winfo_rooty()
        coordpix_x = int(coord_x*pix_width/win_width)
        coordpix_y = int(coord_y*pix_height/win_height)
        if (coord_x>0 and coord_x<win_width and coord_y>0 and coord_y<win_height):
            for i in range (coordpix_x-umbsize,coordpix_x+umbsize+1):
                for j in range (coordpix_y-umbsize,coordpix_y+umbsize):
                    if j==coordpix_y-umbsize:
                        if (0<i<pix_width and 0<j<pix_height):                                       #Out of boundaries ?
                            if (i==coordpix_x-umbsize):                             #Left side of the umbrella
                                grid[i,j,0]='{¯¯'
                            elif (i==coordpix_x+umbsize):                           #Right side of the umbrella
                                grid[i,j,0]='¯¯}'
                            elif (i==coordpix_x):                                   #Junction btw top and middle of the umbrella
                                grid[i,j,0]='¯T¯'
                            else:
                                grid[i,j,0]='¯¯¯¯'
                            grid[i,j,1]=len(colors)-1
                    elif (0<i<pix_width and 0<j<pix_height):
                        grid[i,j,0]=None
                if i==coordpix_x:                                                   # Middle of umbrella
                    for j in range (coordpix_y-umbsize+1,coordpix_y+3):
                        if (0<i<pix_width and 0<j<pix_height):
                            grid[i,j,0]='||'
                            grid[i,j,1]=len(colors)-1


if __name__ == "__main__":

    grid = np.full((pix_height,pix_width,2),None)  # 0 = value, 1 = color variable
    init_color_grid(grid)
    colors = colrain + columb

    fenetre = tk.Tk()
    canvas = tk.Canvas(fenetre, width=win_width, height=win_height, background='black')

    max_drops_nb = 50                        # size of np.array
    drops_nb = 0
    drop_list = np.zeros((max_drops_nb,3), dtype=int)  # list[dropnumber, x], x=0:length, x=1:pos_i/width, x=2:pos_j/height of front

    fuckedsettings=False
    if probadrop>1: 
        fuckedsettings = True


    while True:
        drops_forward(drop_list, grid, pix_height)
        create_newdrop(grid, colors, drop_list, pix_width, probadrop, drops_nb, max_drops_nb, minsize, maxsize)
        add_umbrella(grid, colors)
        for i in range (pix_width):
            for j in range (pix_height):
                canvas.create_text(i*win_width/pix_width, j*win_height/pix_height+5, text=grid[i,j,0], font="Arial 8", fill=colors[grid[i,j,1]])
        canvas.pack()
        time.sleep(rainspeed)
        fenetre.update()
        canvas.delete("all")
    fenetre.mainloop()

