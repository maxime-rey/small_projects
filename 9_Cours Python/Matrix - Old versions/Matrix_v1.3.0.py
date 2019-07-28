from tkinter import *
import numpy as np
import time
import random
import copy


m,n=50,50
height,width = 108*5, 192*5
#colors = ["white", "green", 'yellow', 'blue', 'red']                   #http://www.science.smith.edu/dftwiki/index.php/Color_Charts_for_TKinter
colors = ["darkgreen", "green",'lightgreen', 'sea green', 'dark olive green', 'dark sea green', 'sea green', 'medium sea green', 'light sea green',
         'pale green', 'spring green','lawn green', 'medium spring green', 'green yellow', 'lime green', 'yellow green', 'forest green', 'olive drab', 
         'dark khaki', 'khaki', 'pale goldenrod','DarkSeaGreen4', 'SeaGreen1', 'SeaGreen2', 'SeaGreen3', 'PaleGreen1', 'PaleGreen2', 'PaleGreen3', 
         'PaleGreen4', 'SpringGreen2', 'SpringGreen3', 'SpringGreen4', 'green2', 'green3', 'green4', 'chartreuse2', 'chartreuse3', 'chartreuse4', 
         'OliveDrab1', 'OliveDrab2', 'OliveDrab4', 'DarkOliveGreen1', 'DarkOliveGreen2','DarkOliveGreen3', 'DarkOliveGreen4']
minsize,maxsize=3,9
probadrop = 0.05

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