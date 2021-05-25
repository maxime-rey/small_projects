import matplotlib as mpl
mpl.use("Agg")

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.colors import LogNorm
from mpl_toolkits.axes_grid1.inset_locator import inset_axes

# color-bar 
def add_cb(ax,im,label,loc='right',label_size=15):
    if loc=='right':
        axins = inset_axes(ax,width="3%",height="100%",loc=1,borderpad=0)
        cbar = plt.colorbar(im,cax=axins,orientation='vertical')
    if loc=='top':
        axins = inset_axes(ax,width="100%",height="3%",loc=1,borderpad=0)
        cbar = plt.colorbar(im,cax=axins,orientation='horizontal')

    cbar.set_label(label,size=label_size)
    return cbar


def readresults(timestep):
    f = open('results_%.5i'%timestep,'r')
    npix = int(f.readline())
    time, gamma, rho0, E0,tailleSimu =  np.array(f.readline().split(),dtype=np.float64)
    rho,u,v,P = np.zeros((npix,npix)),np.zeros((npix,npix)),np.zeros((npix,npix)),np.zeros((npix,npix))
    for i in range(npix):
        for j in range(npix):
            rho[i,j],u[i,j],v[i,j],P[i,j] = np.array(f.readline().split(),dtype=np.float64)
            
    f.close()
    return rho,u,v,P,time, gamma, rho0, E0,tailleSimu

def readresultsbinary(timestep):
    from scipy.io import FortranFile as ff
    f = ff('results_%.5i'%timestep,'r')
    npix = f.read_ints()[0]
    time, gamma, rho0, E0,tailleSimu = f.read_reals('d')
    rho,u,v,P = np.zeros((npix,npix)),np.zeros((npix,npix)),np.zeros((npix,npix)),np.zeros((npix,npix))
    for i in range(npix):
        for j in range(npix):
            rho[i,j],u[i,j],v[i,j],P[i,j] = f.read_reals('d')
            
    f.close()
    return rho,u,v,P,time, gamma, rho0, E0,tailleSimu

def add_circle(x,y,r,color='black',alpha=1,linestyle='--'):
    fig=plt.gcf()
    c = plt.Circle((x,y),radius=r,fill=False,color=color,alpha=alpha,linestyle=linestyle)
    fig.gca().add_artist(c)

def show_4_pannels(p1,p2,p3,p4,Rs,tailleSimu):
    plt.figure(figsize=(10,10))

    xc = 0.5 * (1 - 1./p1.shape[0]) * tailleSimu
    yc = xc
    # density
    ax = plt.subplot(2,2,1)
    im = plt.imshow(p1,origin='lower',interpolation='nearest',vmin=0.,vmax=4.,extent=(0,tailleSimu,0,tailleSimu),cmap='pink_r')
    print(tailleSimu,Rs)
    add_circle(xc,yc,Rs,color='white')
    cbar=add_cb(ax,im,r'$\rho/\rho_0$',loc='top')

    # P/rho
    ax = plt.subplot(2,2,2)
    im = plt.imshow(p2,origin='lower',interpolation='nearest',cmap='RdYlBu_r',extent=(0,tailleSimu,0,tailleSimu))
    add_circle(xc,yc,Rs,color='limegreen')
    cbar=add_cb(ax,im,r'$P \ {\rm [cgs]}$',loc='top')

    # v_x
    ax = plt.subplot(2,2,3)
    im = plt.imshow(p3,origin='lower',interpolation='nearest',extent=(0,tailleSimu,0,tailleSimu),cmap='RdBu')#,vmin=-300,vmax=300)
    add_circle(xc,yc,Rs,color='grey')
    cbar=add_cb(ax,im,r'$u\ {\rm [km\ s^{-1}]}$',loc='top')

    # v_z
    ax = plt.subplot(2,2,4)
    im = plt.imshow(p4,origin='lower',interpolation='nearest',cmap='RdBu',extent=(0,tailleSimu,0,tailleSimu))
    add_circle(xc,yc,Rs,color='grey')
    cbar=add_cb(ax,im,r'$v \ {\rm [km\ s^{-1}]}$',loc='top')

    plt.tight_layout()

def show_2_pannels(p1,p2,Rs,tailleSimu):
    plt.figure(figsize=(12,6))

    xc = 0.5 * (1 - 1./p1.shape[0]) * tailleSimu
    yc = xc
    # density
    ax = plt.subplot(1,2,1)
    im = plt.imshow(p1,origin='lower',interpolation='nearest',vmin=0.,vmax=4.,extent=(0,tailleSimu,0,tailleSimu),cmap='pink_r')
    print(tailleSimu,Rs)
    add_circle(xc,yc,Rs,color='white')
    cbar=add_cb(ax,im,r'$\rho/\rho_0$',loc='top')

    # v_norm 
    ax = plt.subplot(1,2,2)
    im = plt.imshow(p2,origin='lower',interpolation='nearest',extent=(0,tailleSimu,0,tailleSimu),cmap='BuPu',vmin=0,vmax=300)
    add_circle(xc,yc,Rs,color='grey')
    cbar=add_cb(ax,im,r'$u\ {\rm [km\ s^{-1}]}$',loc='top')

    plt.tight_layout()

def show_3_pannels(p1,p2,p3,Rs,tailleSimu):
    plt.figure(figsize=(18,6))
    xc = 0.5 * (1 - 1./p1.shape[0]) * tailleSimu
    yc = xc
    # density
    ax = plt.subplot(1,3,1)
    im = plt.imshow(p1,origin='lower',interpolation='nearest',vmin=0.,vmax=4.,extent=(0,tailleSimu,0,tailleSimu),cmap='pink_r')
    print(tailleSimu,Rs)
    add_circle(xc,yc,Rs,color='white')
    cbar=add_cb(ax,im,r'$\rho/\rho_0$',loc='top')
    # v_norm 
    ax = plt.subplot(1,3,2)
    im = plt.imshow(p2,origin='lower',interpolation='nearest',extent=(0,tailleSimu,0,tailleSimu),cmap='BuPu',vmin=0,vmax=300)
    add_circle(xc,yc,Rs,color='black')
    cbar=add_cb(ax,im,r'$u\ {\rm [km\ s^{-1}]}$',loc='top')
    # energy 
    ax = plt.subplot(1,3,3)
    im = plt.imshow(p2,origin='lower',interpolation='nearest',extent=(0,tailleSimu,0,tailleSimu),cmap='jet',norm=LogNorm(),vmin=1e1,vmax=1e4)
    add_circle(xc,yc,Rs,color='grey')
    cbar=add_cb(ax,im,r'$T\ {\rm [K]}$',loc='top')
    plt.tight_layout()

def show_1_pannel(p1,Rs,tailleSimu,cbtitle=r'$\rho/\rho_0$',cmap='pink_r',vmin=0,vmax=4.):
    plt.figure(figsize=(6,6))
    xc = 0.5 * (1 - 1./p1.shape[0]) * tailleSimu
    yc = xc
    ax = plt.subplot(1,1,1)
    im = plt.imshow(p1,origin='lower',interpolation='nearest',vmin=vmin,vmax=vmax,extent=(0,tailleSimu,0,tailleSimu),cmap=cmap)
    add_circle(xc,yc,Rs,color='white')
    cbar=add_cb(ax,im,cbtitle,loc='top')
    plt.tight_layout()



def sedovrad(E,t,rho0):
    r = E**0.25 * t**0.5 / rho0**0.25
    return r

def plotprofile(map,tailleSimu,E0,time,rho0):
    xp, yp = np.indices(map.shape)   # map pixel indices
    xp = (xp+0.5)/map.shape[0]*tailleSimu  # pixel x-coords, from -npix/2 to npix/2
    yp = (yp+0.5)/map.shape[1]*tailleSimu
    xc = 0.5 * (1 - 1./map.shape[0]) * tailleSimu
    yc = xc
    rp = np.sqrt((xp-xc)**2 + (yp-yc)**2)
    rp = np.ravel(rp)
    pc2cm = 3.086e18
    cm2pc = 1./pc2cm
    rp = rp*cm2pc
    m  = np.ravel(map)
    plt.plot(rp,m,'.',color='grey',alpha=0.5,markersize=0.5)
    plt.ylim(0,5)
    plt.xlabel(r'$r\ {\rm [pc]}$',fontsize=15)
    plt.ylabel(r'$\rho / \rho_0$',fontsize=15)
    import sedov_plots as sp
    dim = 2. 
    r,d,u,p=sp.sedovana(dim=2)
    n = len(r)
    r=r[0:n-1]
    d=d[0:n-1]
    u=u[0:n-1]
    p=p[0:n-1]
    E_0= E0
    t_sec=time #3.156e13 * 1e0 # 1 Million years
    rho_0 = rho0 #1e0 * 1.67e-24
    # #E_0=1.
    # #t_sec=1.
    # #rho_0=1.
    r_cgs = r * (E_0/rho_0)**(1./(dim+2.)) * t_sec**(2./(dim+2.))
    d_cgs = d * rho_0
    plt.plot(np.append(r_cgs,[0.5*tailleSimu])*cm2pc,np.append(d_cgs/rho0,[1.]),alpha=0.5,color='red')
    # u_cgs = u * (E_0/rho_0)**(1./(dim+2.)) * t_sec**(-dim/(dim+2.)) 
    # p_cgs = p * (E_0/rho_0)**(2./(dim+2.)) * t_sec**(-2.*dim/(dim+2.)) * rho_0
    plt.xlim(0,0.5*tailleSimu*cm2pc)
    
for i in range(1,1000,1):
    print(i)
    rho,u,v,P,time, gamma, rho0, E0,tailleSimu = readresultsbinary(i)
    T = P/rho
    u = u /rho* 1e-5 # km/s
    v = v /rho* 1e-5 # km/s
    vnorm = np.sqrt(u**2+v**2) # km/s
    # sound speed of the background
    bknd_P = 6e9*rho0
    cs = np.sqrt(gamma*bknd_P/rho0) * 1e-5 # km/s
    cs_local = np.sqrt(gamma * T) * 1e-5
    # Sedov radius
    Rs = sedovrad(E0,time,rho0)
    if False:
        #show_4_pannels(rho/1.67e-24,T,vnorm/cs,vnorm/cs_local,Rs,tailleSimu)
        show_4_pannels(rho/1.67e-24,P,u,v,Rs,tailleSimu)
        plt.savefig('image_%.5i.png'%i)
        plt.clf()
        plt.close()
    if False:
        show_2_pannels(rho/1.67e-24,vnorm,Rs,tailleSimu)
        plt.savefig('2p_image_%.5i.png'%i)
        plt.clf()
        plt.close()

    if False:
        show_1_pannel(rho/rho0,Rs,tailleSimu,cbtitle=r'$\rho/\rho_0$',cmap='pink_r',vmin=0,vmax=4.)
        plt.savefig('overdensity_%.5i.png'%i)
        plt.clf()
        plt.close()

    if True:
        show_3_pannels(rho/rho0,vnorm,T,Rs,tailleSimu)
        plt.savefig('3panels_%.5i.png'%i)
        plt.clf()
        plt.close()

    if False:
        plotprofile(rho/rho0,tailleSimu,E0,time,rho0)
        plt.savefig('rho_prof_%.5i.png'%i)
        plt.clf()
        plt.close()

        
