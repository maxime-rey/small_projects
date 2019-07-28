n=13
L=[ [ 0 for i in range(n) ] for j in range(n) ]
k,l=n-1,int((n+1)/2-1)
if (n%2!=0):
    L[k][l]=1
    for i in range (1,n**2):
        if (L[(k+i)%n][(l+i)%n] == 0):
            L[(k+i)%n][(l+i)%n] = i+1
        else:
            L[(k+i-2)%n][(l+i-1)%n]=i+1
            k,l=(k+i-2)%n,(l+i-1)%n

for i in range(n):
    print (L[i],'\n') 