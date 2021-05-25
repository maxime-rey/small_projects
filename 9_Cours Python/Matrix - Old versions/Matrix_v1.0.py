from tkinter import *
import numpy as np
import time
import random
import copy

def Newvalue(dropthrough, droplength,k,l, grille):
    if dropthrough[k,l]==droplength:        #If end of drop
        return None                             #No more drop, reset dropthrough
    else:
        return random.randint(0,9)              #else add nb


height,width = 108*5, 192*5
m,n=50,50
grid = np.full((m,n),None)
dropthrough = np.full((m,n),0,int)
droplength=3
probadrop = 0.05

fenetre = Tk()
canvas = Canvas(fenetre, width=width, height=height, background='black')
while True:
    gridtemp=copy.deepcopy(grid)
    for j in range (m):
        for i in range (n):
            if j==0:                                                        #If on 1st line
                if isinstance(gridtemp[i,j], int)==False:                       #If no droplet
                    if random.uniform(0,1)<probadrop:                                    #add one with proba x%
                        grid[i,j]=random.randint(0,9)
                        
                else:                                                           #If droplet
                    dropthrough[i,j]+=1                                             #+1 went through
                    grid[i,j]=Newvalue(dropthrough, droplength,i,j, gridtemp)    
                    if isinstance(grid[i,j], int)==False:                           #If no more drop afterwards -> dropthrough counter reset
                        dropthrough[i,j]=0 
                    if j+1<m:                                                       #If can go down, go down
                        grid[i,j+1] = random.randint(0,9)
            else:
                if isinstance(gridtemp[i,j], int)==True:                    #If droplet
                    dropthrough[i,j]+=1
                    grid[i,j]=Newvalue(dropthrough, droplength,i,j, gridtemp)  
                    if isinstance(grid[i,j], int)==False:                       #If no more drop afterwards -> dropthrough counter reset
                        dropthrough[i,j]=0
                    if j+1<m:                                                   #If can go down, go down
                        grid[i,j+1] = random.randint(0,9)
    for i in range (n):
        for j in range (m):
            canvas.create_text(i*width/n, j*height/m, text=grid[i,j], font="Arial 8", fill="green")
    canvas.pack()
    time.sleep(0.1)
    fenetre.update()
    canvas.delete("all")
fenetre.mainloop()