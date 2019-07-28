***************************************************************
      PROGRAM ROTATION_BOUVIER
***************************************************************
***   Program computing the evolution of the surface angular 
***   velocity following Bouvier, Forestini & Allain (1997) method
***  
***  This program uses the subroutine BOREAS.F, an adaptation
***   of the IDL routine provided by Cranmer & Saar (2011) to 
***   compute the mass loss rate.
*** 
***  Two prescriptions for the AM losses by wind torques are treated
***      Kawaler 1988 as in Irwin & Bouvier 2009
***      Matt et al. 2012
***      For Matt et al. 2012, prescriptions mass loss comes from
***      Cranmer & Saar (2011) and saturates with an exponent
***      a = 1.3 (see Gallet & Bouvier , private communication)
***      prescription for magnetic field from Reiners et al. 2009
***      which saturates with an exponent b = 1.65
***
***   The angular velocity of saturation is the same for Mdot and B
***    and is taken to 20 * omega_sun.
***
***   In version 1.0, input files are from the PMS grid by Siess
***   http://www.astro.ulb.ac.be/~siess/StellarModels/PMS
***
*** Version 1.0 : A. Palacios July 2012
***
*****************************************************************


      implicit none

      integer i,j,n,icrazy,niter,niter1,brake,ntdisc,nom0,sat
      parameter (niter = 264,niter1 = niter-1)

      double precision tdisc,vesc
      double precision omega_sun,omega_0
      double precision K1, K2, m, Bsun, Mdotsun, b, Bstar, mdot,
     $     Mdotsat, Bsat,a
      double precision G,msun,rsun,lsun,sec,seci,pi,pim2
      double precision torque,omega,inertia,prot,omeganotorque
      double precision R,Mass,t,L,dt,Teff
      double precision mloss,f,fstar
      double precision k2conv,k2rad
      double precision cte1, cte2, fourm,expm,expr,factor


      character*36 directory
      character*1 answer

      dimension torque(niter),inertia(niter),omega(niter),Mass(niter),
     $     t(niter),k2conv(niter),k2rad(niter),R(niter),L(niter),
     $     prot(niter),dt(niter1),Teff(niter),omeganotorque(niter)

c...  Global constants
      pi = 3.1415926535d0
      pim2 = 2.d0*pi
      G = 6.67259d-8
      msun = 1.9891d33
      rsun = 6.9599d10
      lsun = 3.846d33
      omega_sun = 2.8d-6
      Bsun = 3.d0
      Mdotsun = 1.31d12
      sec = 3.1557807d7
      seci = 1.d0/sec

c...  Setting the frame for computation

      write(*,*)'Enter disc lifetime in Myrs'
      read(*,'(i2)') ntdisc

      write(*,*)'Enter initial angular velocity (in solar units)'
      read(*,'(i2)') nom0

      write(*,*)'Enter angular velocity of saturation (in solar units)'
      read(*,'(i2)') sat


      write(*,*)'Choose torque prescription'
      write(*,*)'Kawaler according to Irwin & Bouvier 2009   [1]'
      write(*,*)'Matt et al. 2012                            [2]'
      read(*,'(i1)') brake

      if (brake.eq.1) then
         write(*,*)'Enter value for constant K'
         read(*,'(1pe5.0)') factor
      else
         write(*,*)'Do you want to modify the constants K1 and K2?'
         read(*,'(a1)')answer
	if (answer.eq.'y') then
            write(*,*)'Enter new value for K1'
            read(*,'(1pe8.2)') K1
            write(*,*)'Enter new value for K2'
            read(*,'(1pe8.2)') K2	
	else
            K1 = 1.30d0
            K2 = 0.0506d0
        endif
      endif

c...  Constants needed for the torque formulation
      tdisc = ntdisc*1.d6
      omega_0 = omega_sun*nom0
c...  When omega_0 = 3 * omega_sun, m needs to be multiplied by 2.15 to fit the solar value
      m = 0.2177d0
      fourm = 4.d0*m
      expm = 1.d0-2.d0*m
      expr = 5.d0*m+2.d0
      a = 1.3d0
      b = 1.65d0
      f = 0.1d0
      omega_sun = 2.8d-6
      cte1 =  K1**2/(2.d0*G)**m
      cte2 = 1.d0/(K2**2+0.5d0*f**2)**m

c...  Reading the input files from Siess PMS stellar models @ Z = 0.01
      
c...  radius mass, luminosity, age from .hr
      directory = 'C:\Users\Maxime\Desktop\TP\Test\'
      open (unit = 01, file = trim(directory) // 'm1.0z01.hrd', status
     $     ='old',action='read')

      read(01,'(//)')
      do i = 1,niter
         read(01,'(11x,f9.6,8x,f8.5,12x,f5.0,34x,1pe16.10)')L(i),R(i)
     $        ,Teff(i),t(i)
      enddo

c...  gyration radii of CE and RZ from .var1

      open (unit = 02, file = trim(directory) // 'm1.0z01.var1',status
     $     ='old',action='read')


      read(02,'(//)')
      do i = 1,niter
         read(02,'(82x,f7.5,1x,f7.5)')k2conv(i),k2rad(i)
      enddo

c...  Output file
      open (unit = 10, file = trim(directory) // 'momentum.dat',
     $     status='unknown')



      Mass(1:niter) = 1.d0
      omega(1) = omega_0
      omeganotorque(1) = omega_0
      inertia(1:niter) = msun*(R(1:niter)*rsun)**2*(k2rad(1:niter)
     $     +k2conv(1:niter))
      dt(1) = t(1)*sec
c.. Loop over all the timesteps
      do i = 2,niter
         if (t(i).lt.tdisc) then
            omega(i) = omega_0
            omeganotorque(i) = omega_0
         endif
        dt(i) = (t(i)-t(i-1))*sec
c.. Compute rotation period
         prot(i-1) = pim2/(omega(i-1)*24.d0*3.6d3)
c.. Compute mass loss according to Cranmer & Saar 2011 (subroutine boreas)
         call boreas(Mass(i-1),R(i-1),L(i-1),prot(i-1),0.d0,mloss,Bstar,
     $        fstar,icrazy)
c.. Accounting for the mass loss an computing the new mass (will be inconsistent with Siess grid)
         Mass(i) = Mass(i-1)-mloss*dt(i)*seci
         vesc = sqrt(2.d0*G*Mass(i-1)*msun/(R(i-1)*rsun))
         if (t(i).ge.tdisc) then

c..   Computing the torque from Matt et al. 2012 equation 9
c...  This equation gives tau_w = (dJ/dt)_w so that the expression needs to be x by dt
c...  if we only want the AM variation as is the case here.

            if (brake.eq.2) then
               
               mdot = mloss*msun*seci
	       Bstar = Bstar*fstar
               torque(i-1) = -cte1*Bstar**fourm*mdot**expm*(R(i-1)
     $              *rsun)**expr/(Mass(i-1)*msun)**m*omega(i-1)*cte2
     $              *dt(i)
                             
c..   Computing the torque from Irwin & Bouvier 2009
            else if (brake.eq.1) then
               if (omega(i-1).lt.sat*omega_sun) then

                  torque(i-1) = -dt(i)*factor*omega(i-1)**3*
     $                 sqrt(R(i-1))
               else

                  torque(i-1) = -factor*dt(i)*omega(i-1)*(sat*
     $                 *omega_sun)**2*sqrt(R(i-1))
               endif
            endif

            omega(i) = omega(i-1) + (torque(i-1)-omega(i-1)*(inertia(i)
     $           -inertia(i-1)))/inertia(i-1)
            omeganotorque(i) = omeganotorque(i-1)*(2.d0-inertia(i)
     $           /inertia(i-1))
         endif

c... Save results

         write(10,'(2(1x,1pe16.10),1x,1pe16.10,1x,i4,1x,1pe16.9)')
     $        omega(i),omeganotorque(i),t(i),
     $        int(Teff(i)),torque(i-1)


      enddo

      close(01)
      close(02)
      close(10)

      
      end
