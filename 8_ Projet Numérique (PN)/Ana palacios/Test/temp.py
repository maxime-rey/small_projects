from numpy  import *
from matplotlib.pyplot import *
from math import *


data=np.loadtxt('momentum.dat') 
Y= data[:,0]
X= data[:,2]
plot(np.log10(X),np.log10(Y))
#ax.set_yscale('log10')
#ax.set_xscale('log10')