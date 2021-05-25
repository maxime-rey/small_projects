from tkinter import *
import numpy as np
import time
import random
import copy


from VarMatrix import *

grid = np.full((m,n),None)
dropthrough = np.full((m,n),0,int)
droplength = np.full((m,n,2),0,int)   # 1 = length, 2 = color, 

fenetre = Tk()
canvas = Canvas(fenetre, width=width, height=height, background='black')
while True:
    gridtemp = copy.deepcopy(grid)
    droplengthtemp = copy.deepcopy(droplength)
    for j in range (m):
        for i in range (n):
            if isinstance(gridtemp[i,j], int)==True:                #If drop there is
                if dropthrough[i,j]==droplengthtemp[i,j,0]:           #If end -> No more droplet, length and dropthrough reset
                    grid[i,j] = None
                    droplength[i,j,0] = 0
                    dropthrough[i,j] = 0
                else:                                                   #If not end  -> New value
                    grid[i,j]=random.randint(0,9)
                    dropthrough[i,j] += 1
            else:                                                   #If no drop
                if j==0:                                                # If first line -> Generate one and a length
                    if random.uniform(0,1)<probadrop:                       
                        grid[i,j]=random.randint(0,9)
                        droplength[i,j,0]=random.randint(minsize-1,maxsize-1)
                        droplength[i,j,1]= random.randint(0,len(colors)-1)
                    else:                      
                        grid[i,j]=None
                        droplength[i,j,0]=0


                else:                                                   #If any other line
                    if isinstance(gridtemp[i,j-1], int)==True:              #If drop in previous cell -> new drop and get length
                        grid[i,j]=random.randint(0,9)
                        droplength[i,j]=droplength[i,j-1]
                    else:                      
                        grid[i,j]=None
                        droplength[i,j,0]=0

    for i in range (n):
        for j in range (m):
            canvas.create_text(i*width/n, j*height/m+5, text=grid[i,j], font="Arial 8", fill=colors[droplength[i,j,1]])
    canvas.pack()
    time.sleep(0.1)
    fenetre.update()
    canvas.delete("all")
fenetre.mainloop()