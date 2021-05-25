a,b=306,756
r=1
if (a<0) or (b<0):
    print('You fail')
while (r!=0):
    if (a>b):
        r=a%b
        if (r==0):
            print ('PGCD =', b)
        else:
            a,b=b,r
    else:
        a,b=b,a

    

