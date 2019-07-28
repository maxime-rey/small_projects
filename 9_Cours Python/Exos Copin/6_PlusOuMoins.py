import random
nmin, nmax = 0,3
n=random.randint(nmin,nmax)
m=101
Step=0
while (1):
    m=input('Enter int')
    try:
        m2=int(m)
        if (m2>100)or(m2<0)or(isinstance(m2,int)):
            raise ValueError
    except ValueError:
        print('Wrong value')
    Step+=1
    if (n>m2):
        print('higher')
    elif (n<m2):
        print('lower')
    else:
        print('Yep, la solution est ',n,'\n','Tu as mis ',Step, 'iteration(s)')
        break


