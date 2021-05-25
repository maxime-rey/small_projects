#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Dec 12 13:48:52 2018

@author: adrienauriol
"""

#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Dec  4 13:01:29 2018

@author: adrienauriol
"""

import numpy as np 
import matplotlib.pyplot as plt

#####################################
#ISOCHRONES
#####################################
filename1b = "data/iso/iso1Myr.txt"
filename2b = "data/iso/iso10Myr.txt"
filename3b = "data/iso/iso100Myr.txt"
filename4b = "data/iso/iso1Gyr.txt"
filename5b = "data/iso/iso10Gyr.txt"

data1 = np.transpose(np.loadtxt(filename1b))
data2 = np.transpose(np.loadtxt(filename2b))
data3 = np.transpose(np.loadtxt(filename3b))
data4 = np.transpose(np.loadtxt(filename4b))
data5 = np.transpose(np.loadtxt(filename5b))


Bmag_points1=data1[24]
Vmag_points1=data1[25]
BV_points1=Bmag_points1-Vmag_points1

Bmag_points2=data2[24]
Vmag_points2=data2[25]
BV_points2=Bmag_points2-Vmag_points2

Bmag_points3=data3[24]
Vmag_points3=data3[25]
BV_points3=Bmag_points3-Vmag_points3

Bmag_points4=data4[24]
Vmag_points4=data4[25]
BV_points4=Bmag_points4-Vmag_points4

Bmag_points5=data5[24]
Vmag_points5=data5[25]
BV_points5=Bmag_points5-Vmag_points5

#####################################
#####################################
filename1 = "data/NGC1245R2.txt"
filename2 = "data/NGC1245V2.txt"
filename3 = "data/NGC1245B2.txt"

R = np.transpose(np.loadtxt(filename1))
V = np.transpose(np.loadtxt(filename2))
B = np.transpose(np.loadtxt(filename3))

Rx=R[1]
Vx=V[1]
Bx=B[1]

Ry=R[2]
Vy=V[2]
By=B[2]

Rmag=R[3]
Vmag=V[3]
Bmag=B[3]

Rxc=[]
Ryc=[]
Rmagc=[]

Vxc=[]
Vyc=[]
Vmagc=[]

Bxc=[]
Byc=[]
Bmagc=[]

##########################
####SELECTION FENETRE#####
##########################
xup=1300
xdown=0
yup=2140
ydown=1300

##########################
##########################
t=1

for i in range(len(Rx)):
    if (Rx[i]<xup) and (Rx[i]>xdown) and (Ry[i]<yup) and (Ry[i]>ydown):
        t=1
    else :
        Rxc.append(Rx[i])
        Ryc.append(Ry[i])
        Rmagc.append(Rmag[i])
        
for i in range(len(Vx)):
    if (Rx[i]<xup) and (Vx[i]>xdown) and (Vy[i]<yup) and (Vy[i]>ydown):
        t=1
    else :
        Vxc.append(Vx[i])
        Vyc.append(Vy[i])
        Vmagc.append(Vmag[i])
        
for i in range(len(Bx)):
    if (Bx[i]<xup) and (Bx[i]>xdown) and (By[i]<yup) and (By[i]>ydown):
        t=1
    else :
        Bxc.append(Bx[i])
        Byc.append(By[i])
        Bmagc.append(Bmag[i])

Rc=np.transpose(np.array([Rxc,Ryc,Rmagc]))
Vc=np.transpose(np.array([Vxc,Vyc,Vmagc]))
Bc=np.transpose(np.array([Bxc,Byc,Bmagc]))

np.savetxt('data/Rc.txt',Rc)
np.savetxt('data/Vc.txt',Vc)
np.savetxt('data/Bc.txt',Bc)


R2 = np.transpose(np.loadtxt('data/Rc.txt'))
V2 = np.transpose(np.loadtxt('data/Vc.txt'))
B2 = np.transpose(np.loadtxt('data/Bc.txt'))

Rmag2=R2[2]
Vmag2=V2[2]
Bmag2=B2[2]

############# Extinction #############
Av=0#0.961 #E=0.310 => Av=0.961 apparemment
Ab=1.337*Av
Ar=0.7505*Av

Vmag3=Vmag2+5.839
Bmag3=Bmag2+6.914
Rmag3=Rmag2+5.97   

VRmag3=Vmag3-Av - (Rmag3-Ar)
BVmag3=Bmag3-Ab - (Vmag3-Av)

xmin=-1.1
xmax=-0.2
ymin=-0.5
ymax=2.5

AxeX = VRmag3
AxeY = BVmag3

# =============================================================================
# xmin=-0.4
# xmax=0.8
# ymin=-0.5
# ymax=2.5
# =============================================================================

plt.scatter(AxeX,AxeY,c='black',edgecolor='none',s=1,marker='o')
plt.plot(AxeX,16*AxeX/9+1.5,c='red',linewidth=0.1) #formule de Ballestero
plt.plot(AxeX,16*AxeX/9+2.55,c='blue',linewidth=0.1)
# =============================================================================
# plt.scatter(BV_points1,Vmag_points1,c='blue',edgecolor='none',s=1,marker='o',label='1 Myr')
# plt.scatter(BV_points2,Vmag_points2,c='red',edgecolor='none',s=1,marker='o',label='10 Myr')
# plt.scatter(BV_points3,Vmag_points3,c='green',edgecolor='none',s=1,marker='o',label='100 Myr')
# plt.scatter(BV_points4,Vmag_points4,c='black',edgecolor='none',s=1,marker='o',label='1 Gyr')
# plt.scatter(BV_points5,Vmag_points5,c='grey',edgecolor='none',s=1,marker='o',label='10 Gyr')
# =============================================================================

plt.legend(scatterpoints=100,fontsize=7.5)
plt.xlim(xmin,xmax)
plt.ylim(ymax,ymin)
plt.title('HRD NGC1245')
plt.xlabel('V-R')
plt.ylabel('B-V')
plt.savefig('HRD.png',dpi=500)
plt.show()