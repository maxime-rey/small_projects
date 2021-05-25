import numpy as np
import matplotlib.pyplot as plt

L=np.loadtxt('C:/Users/Maxime/Desktop/Python/Exos Copin/anscombe.dat')


x=[L[:,0],L[:,2],L[:,4],L[:,6]]
y=[L[:,1],L[:,3],L[:,5],L[:,7]]


fig = plt.figure()
for i in range (len(x)):
    ax = fig.add_subplot(2, 2, i+1,  xticks=[], yticks=[])
    ax.scatter(x[i], y[i], c='b', label="Blah")    # Courbe y = sin(x)
    ax.set_xlabel("x")          # Nom de l'axe des x
    ax.set_ylabel("y")                # Nom de l'axe des y
    ax.set_title("Kiwi !"+str(i+1))  # Titre de la figure
    ax.legend()                       # Légende
fig.suptitle("Anscombe's Quartet", fontsize='x-large')
fig.tight_layout()
plt.show()

print(x)