import random
import time
import copy

def sumvoisin(k,l,grille,n,m):
    sum=0
    for i in range (k-1,k+2):
        for j in range (l-1,l+2):
            if (i,j)!=(k,l):
                sum +=  grille[i%n][j%m]
    return sum

def nextstep(k,l,grille,n,m):
    if grille[k][l] == 0:
        if sumvoisin(k,l,grille,n,m)==3:
            return 1
        else:
            return 0
    else:
        if (sumvoisin(k,l,grille,n,m)==2 or sumvoisin(k,l,grille,n,m)==3):
            return 1
        else:
             return 0

if __name__ == '__main__':
    n,m,k=10,50,0                                                           #Size of the grid
    grid=[ [ 0 for i in range(m) ] for j in range(m) ]
    for i in range (n):
        for j in range (m):
            if random.uniform(0,1)<0.1:
                grid[i][j] = 1
    while True:
        print(k)
        gridTemp=copy.deepcopy(grid)
        for i in range (n):
            for j in range (m):
                grid[i][j]=nextstep(i,j,gridTemp,n,m)

        for i in range (n):
            for j in range (m):
                if grid[i][j]==0:
                    print ('.',end='')
                else: 
                    print ('#',end='')
            print ('\n')
        k+=1
        time.sleep(0.5)