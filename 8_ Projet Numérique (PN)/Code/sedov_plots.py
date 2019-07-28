##########################################################################
def plotSedovAna(dim=3):
    import matplotlib
    #matplotlib.use('TkAgg')
    from matplotlib import pyplot as plt
    import numpy as np
    #---------------------------------------------------------------------
    #fig = plt.figure()
    f, axarr = plt.subplots(2, 2)
    #ax = fig.add_subplot(111)
    #ax.minorticks_on()

    #plt.axis([xrg[0],xrg[1],yrg[0],yrg[1]])
    #plt.xlabel('time [Myr]')
    #plt.ylabel('Shock position [kpc]')

    r,d,u,p=sedovana(dim=dim)
    n = len(r)
    r=r[0:n-1]
    d=d[0:n-1]
    u=u[0:n-1]
    p=p[0:n-1]

    E_0=1e51
    t_sec=3.156e13 * 1e0 # 1 Million years
    rho_0 = 1e0 * 1.67e-24
    #E_0=1.
    #t_sec=1.
    #rho_0=1.
    
    print('rho = ',rho_0)

    r_cgs = r * (E_0/rho_0)**(1./(dim+2.)) * t_sec**(2./(dim+2.))
    d_cgs = d * rho_0
    u_cgs = u * (E_0/rho_0)**(1./(dim+2.)) * t_sec**(-dim/(dim+2.))
    p_cgs = p * (E_0/rho_0)**(2./(dim+2.)) * t_sec**(-2.*dim/(dim+2.)) * rho_0
    
    axarr[0, 0].scatter(r_cgs, d_cgs)
    axarr[0, 0].set_xlabel('r')
    axarr[0, 0].set_ylabel('d')
    axarr[0, 0].set_ylim(0,max(d_cgs))
    

    axarr[0, 1].scatter(r_cgs, u_cgs)
    axarr[0, 1].set_xlabel('r')
    axarr[0, 1].set_ylabel('u')
    axarr[0, 1].set_ylim(0,max(u_cgs))

    axarr[1, 0].scatter(r_cgs, p_cgs)
    axarr[1, 0].set_xlabel('r')
    axarr[1, 0].set_ylabel('p')
    axarr[1, 0].set_ylim(0,max(p_cgs))

    T_kelvin = p_cgs/d_cgs/1.3806e-16
    axarr[1, 1].scatter(r_cgs, T_kelvin)
    axarr[1, 1].set_xlabel('r')
    axarr[1, 1].set_ylabel('T')
    axarr[1, 1].set_ylim(0,1e6)

    r=(E_0*t_sec**2/rho_0)**(1./5.)
    axarr[0,0].plot([r,r],[0,1e10])

    plt.tight_layout()

##########################################################################
#       The routine returns 4 arrays of unknown size (determined
#       automatically by the routine). The output variables are
#       the following dimensionless quantities: r (position from the
#       point like explosion, d (density), u (velocity) and p
#       (pressure). To recover the true value, you have to rescale
#       these dimensionless values to the true values, defining first
#       the total energy E_0, the initial mass density rho_0 and the
#       time t you consider and finally computing the true values
#       using  the following scaling laws:
#
#       r = r * (E_0/rho_0)^(1./(dim+2.)) * t^(2./(dim+2.))
#       d = d * rho_0
#       u = u * (E_0/rho_0)^(1./(dim+2.)) * t^(-dim/(dim+2.))
#       p = p * (E_0/rho_0)^(2./(dim+2.)) * t^(-2.*dim/(dim+2.)) * rho_0
#
# This routine is adapted from an IDL routine by R Teyssier
def sedovana(gamma=1.4,dim=1):
    import numpy as np
    #---------------------------------------------------------------------

    n = int(dim)

    print('gamma=',gamma)
    g=float(gamma)
    n=float(n)
    n1=1000
    n2=1000

    vmax=4.e0/(n+2.)/(g+1.)
    vmin=2.e0/(n+2.)/g
    temp = np.linspace(0,n1-1,n1)
    v=vmin+10.e0**(-10.e0*(1.0e0-(temp+1)/float(n1)))*(vmax-vmin)
    a2 = (1.-g)/(2.*(g-1.)+n)
    a1 = (n+2.)*g/(2.+n*(g-1.)) * ( 2.*n*(2.-g)/g/(n+2.)**2 - a2 )
    a3 = n/(2.*(g-1)+n)
    a4 = a1*(n+2.)/(2.-g)
    a5 = 2./(g-2.)
    a6 = g/(2.*(g-1.)+n)
    a7 = a1*(2.+n*(g-1.))/(n*(2.-g))

    r1 = ((n+2.)*(g+1.)/4.*v)**(-2./(2.+n)) \
          *((g+1.)/(g-1.) *( (n+2.)*g/2.*v-1.) )**(-a2) \
          *( (n+2.)*(g+1.) / ( (n+2)*(g+1)-2.*(2.+n*(g-1.)) ) * (1.-(2.+n*(g-1.))/2.*v) )**(-a1)
        
    u1 = (n+2.)*(g+1.)/4.*v*r1

    d1 = ((g+1.)/(g-1.)*((n+2.)*g/2.*v-1.))**(a3)   \
         *((g+1.)/(g-1.)*(1.-(n+2.)/2.*v  ))**(a5)  \
         *((n+2.)*(g+1.)/( (n+2)*(g+1)-2.*(2.+n*(g-1.))) *(1.-(2.+n*(g-1.))/2.*v) )**(a4)
     
    p1 = ((n+2.)*(g+1.)/4.*v)**(2.*n/(2.+n))        \
      *((g+1.)/(g-1.)*(1.-(n+2.)/2.*v  ))**(a5+1.)  \
      *((n+2.)*(g+1.)/( (n+2)*(g+1)-2.*(2.+n*(g-1.))) *(1.-(2.+n*(g-1.))/2.*v) )**(a4-2.*a1)

    temp=np.linspace(0,n2-1,n2)
    r2=r1[0]*(temp+0.5e0)/float(n2)
    u2=u1[0]*r2/r1[0]
    d2=d1[0]*(r2/r1[0])**(n/(g-1.0e0))
    p2=p1[0]*(r2/r2)

    r=np.zeros(len(r1)+len(r2)+2)
    r[0:len(r2)]=r2
    r[len(r2):len(r1)+len(r2)]=r1
    r[len(r1)+len(r2)]=r1.max()
    r[len(r1)+len(r2)+1]=r1.max()+1000.
    d=np.zeros(len(r))
    d[:]=r[:]
    d[0:len(r2)]=d2
    d[len(r2):len(r1)+len(r2)]=d1
    d[len(r1)+len(r2)]=1./((g+1.)/(g-1.))
    d[len(r1)+len(r2)+1]=1./((g+1.)/(g-1.))
    u=np.zeros(len(r))
    u[:]=r[:]
    u[0:len(r2)]=u2
    u[len(r2):len(r1)+len(r2)]=u1
    u[len(r1)+len(r2)]=0.
    u[len(r1)+len(r2)+1]=0.
    p=np.zeros(len(r))
    p[:]=r[:]
    p[0:len(r2)]=p2
    p[len(r2):len(r1)+len(r2)]=p1
    p[len(r1)+len(r2)]=0.
    p[len(r1)+len(r2)+1]=0.

    d=d*(g+1.)/(g-1.)
    u=u*4./(n+2.)/(g+1.)
    p=p*8./(n+2.)**2./(g+1.)

    nn=len(r)
    vol=np.zeros(nn)
    vol[:]=r[:]
    #for i=1,nn-1 do vol(i)=r(i)**n-r(i-1)**n
    for i in range(1,nn):
        vol[i]=r[i]**n-r[i-1]**n
    vol[0]=r[0]**n
    const=1.
    if n == 1.: const=2.0e0
    if n == 2.: const=3.1415926535897931
    if n == 3.: const=4.*3.1415926535897931/3.

    vol=vol*const
    int1=(d*u*u/2.e0)*vol
    int2=p/(g-1.e0)*vol
    sum1=sum(int1)
    sum2=sum(int2)
    summ=sum1+sum2
    print('chi0=',summ**(-1./(2.+n)))
    chi0=summ**(-1./(2.+n))
    r=r*chi0
    u=u*chi0
    p=p*chi0**2
    return r,d,u,p
