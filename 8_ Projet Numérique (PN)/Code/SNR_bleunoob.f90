program cocoricode

! cd D:/Life/Studies/5_M2\ -\ Magist?re\ 3/6_\ Projet\ Num?rique\ \(PN\)/Code
! gfortran SNR_bleu.f90 -static
! gfortran -fcheck=bounds -g -fbacktrace SNR.f90 -static

  implicit none
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Initialisation paramètres
  real(kind=8),parameter :: Taille_simu=1d20 
  integer(kind=4),parameter :: nx = 100, ny = 100
  real(kind=8)    :: dx = Taille_simu/nx, gamma = 2., courant_factor = 0.8       
  
  real(kind=8)    :: state(0:nx+1,0:ny+1,1:4)
  real(kind=8)    :: flux(1:2, 0:nx, 0:ny, 1:4)

  real(kind=8)    :: dt, dt_cell, tmax=1d40, t=0
  integer(kind=4) :: i,j,istep=0
 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Initialisation tableaux
   state(:,:,1) = 1.67d-24                                  !Rho0
   state(:,:,2) = 0                                         !u0*Rho
   state(:,:,3) = 0                                         !v0*Rho
   state(:,:,4) = state(:,:,1)*6d9/((gamma-1))              !E0*Rho  (6d9~kT/mu*m(H)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Release the beast
   ! do j = ny/2,ny/2+1
   !    do i = nx/2,nx/2+1
   state(50,50,4) = 1d30/(dx**2) !/4
   !    end do
   ! end do

   dt = courant_factor/3. * dx/(sqrt(gamma*pression(50,50)/state(50,50,1)))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Calcul des flux
   do while(t<tmax)       
      if(istep==1000)EXIT
      do j = 0,ny
        do i = 0,nx
            flux(1,i,j,1) = (subflux1(i+1,j) + subflux1(i,j))/2 - maxspeed(i,j,1)*(state(i+1,j,1)-state(i,j,1))
            flux(2,i,j,1) = (subflux2(i,j+1) + subflux2(i,j))/2 - maxspeed(i,j,2)*(state(i,j+1,1)-state(i,j,1))

            flux(1,i,j,2) = (subflux3(i+1,j) + subflux3(i,j))/2 - maxspeed(i,j,1)*(state(i+1,j,2)-state(i,j,2))   
            flux(2,i,j,2) = (subflux4_5(i,j+1) + subflux4_5(i,j))/2 - maxspeed(i,j,2)*(state(i,j+1,2)-state(i,j,2))

            flux(1,i,j,3) = (subflux4_5(i+1,j) + subflux4_5(i,j))/2 - maxspeed(i,j,1)*(state(i+1,j,3)-state(i,j,3))
            flux(2,i,j,3) = (subflux6(i,j+1) + subflux6(i,j))/2 - maxspeed(i,j,2)*(state(i,j+1,3)-state(i,j,3))
            
            flux(1,i,j,4) = (subflux7(i+1,j) + subflux7(i,j))/2 - maxspeed(i,j,1)*(state(i+1,j,4)-state(i,j,4))
            flux(2,i,j,4) = (subflux8(i,j+1) + subflux8(i,j))/2 - maxspeed(i,j,2)*(state(i,j+1,4)-state(i,j,4))
         end do
      end do
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Compute cells' state
      do j = 1,ny
         do i = 1,nx
            state(i,j,:) = state(i,j,:) - dt/dx*(flux(1,i,j,:) - flux(1,i-1,j,:) + flux(2,i,j,:) - flux(2,i,j-1,:))
         end do
      end do
      t = t + dt
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Boundaries
      state(:,0,:) = state(:,1,:)    
      state(:,ny+1,:) = state(:,ny,:) 
      state(0,:,:) = state(1,:,:)    
      state(nx+1,:,:) = state(nx,:,:)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! COmpute new dt
      dt=1d40
      do j = 1,ny
         do i = 1,nx
            dt_cell = dx/(sqrt(state(i,j,2)**2+state(i,j,3)**2)/state(i,j,1) + sqrt(gamma*pression(i,j)/state(i,j,1))) &
            *courant_factor/3.
            if(dt_cell < dt)then      
               dt = dt_cell 
            endif
         end do
      end do

      istep = istep +1
      ! print*,istep,dt
      ! if(mod(istep,10)==0)then 
          call save
      ! end if
  end do

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Save
   subroutine save
      CHARACTER(LEN=256) :: filename1, filename2, filename3, filename4

      write(filename1,"(A5,I5.5)")"data_",istep
      open(10,file="Data\Rho\"//TRIM(filename1)//".dat",form='formatted')

      write(filename2,"(A,I5.5,a)")"data_",istep 
      open(11,file="Data\u\"//TRIM(filename2)//".dat",form='formatted')

      write(filename3,"(A,I5.5,a)")"data_",istep
      open(12,file="Data\v\"//TRIM(filename3)//".dat",form='formatted')

      write(filename4,"(A,I5.5,a)")"data_",istep
      open(13,file="Data\E\"//TRIM(filename4)//".dat",form='formatted')


      do j=1,ny                           
         do i=1,nx
            write(10,*)(dble(i)-0.5)/dble(nx),(dble(j)-0.5)/dble(ny),state(i,j,1)
            !write(11,*)(dble(i)-0.5)/dble(nx),(dble(j)-0.5)/dble(ny),sqrt((state(i,j,2)/state(i,j,1))**2&
            !+(state(i,j,3)/state(i,j,1))**2)
            write(11,*)(dble(i)-0.5)/dble(nx),(dble(j)-0.5)/dble(ny),state(i,j,2)/state(i,j,1)
            write(12,*)(dble(i)-0.5)/dble(nx),(dble(j)-0.5)/dble(ny),state(i,j,3)/state(i,j,1)
            write(13,*)dble(i),dble(j),state(i,j,4)*dx**2
            !write(13,*)dble(i),dble(j),pression(i,j)/state(i,j,1)
         end do                              
         write(10,*) " "  
         write(11,*) " " 
         write(12,*) " " 
         write(13,*) " " 
      end do
      write(10,*) " " 
      write(11,*) " " 
      write(12,*) " " 
      write(13,*) " "     
      close(10)
      close(11)
      close(12)
      close(13)
   end subroutine Save

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Pression
   function pression(k,l)
      real(kind=8) :: pression
      integer(kind=4) :: k,l
      real(kind=8) :: rho,u,v,E
      rho=state(k,l,1)
      u=state(k,l,2)/rho
      v=state(k,l,3)/rho
      E=state(k,l,4)/rho
      pression = (E - 0.5d0*(u**2 + v**2))*(gamma-1)*rho
      return
   end function

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Subflux
   function subflux1(k,l)
      real(kind=8) :: subflux1
      integer(kind=4) :: k,l
      subflux1 = state(k,l,2)
      return
   end function

   function subflux2(k,l)
      real(kind=8) :: subflux2
      integer(kind=4) :: k,l
      subflux2 = state(k,l,3)
      return
   end function
   
   function subflux3(k,l)
      real(kind=8) :: subflux3
      integer(kind=4) :: k,l
      real(kind=8) :: rho,u
      rho=state(k,l,1)
      u=state(k,l,2)/rho
      subflux3 = rho*u**2 + pression(k,l)
      return
   end function

   
   function subflux4_5(k,l)
      real(kind=8) :: subflux4_5
      integer(kind=4) :: k,l
      real(kind=8) :: rho,u,v
      rho=state(k,l,1)
      u=state(k,l,2)/rho
      v=state(k,l,3)/rho
      subflux4_5 = rho*u*v
      return
   end function

   function subflux6(k,l)
      real(kind=8) :: subflux6
      integer(kind=4) :: k,l
      real(kind=8) :: rho,v
      rho=state(k,l,1)
      v=state(k,l,3)/rho
      subflux6 = rho*v**2 + pression(k,l)
      return
   end function 
   
   function subflux7(k,l)
      real(kind=8) :: subflux7
      integer(kind=4) :: k,l
      real(kind=8) :: rho,u,E
      rho=state(k,l,1)
      u=state(k,l,2)/rho
      E=state(k,l,4)/rho
      subflux7 = rho*u*(E + pression(i,j)/rho)
      return
   end function

   function subflux8(k,l)
      real(kind=8) :: subflux8
      integer(kind=4) :: k,l
      real(kind=8) :: rho,v,E
      rho=state(k,l,1)
      v=state(k,l,3)/rho
      E=state(k,l,4)/rho
      subflux8 = rho*v*(E + pression(i,j)/rho)
      return
   end function

   function maxspeed(k,l,m)
      real(kind=8) :: maxspeed
      integer(kind=4) :: k,l,m
      if(m==1)then
         maxspeed = max(sqrt(state(k,l,2)**2+state(k,l,3)**2)/state(k,l,1) + sqrt(gamma*pression(k,l)/state(k,l,1)), &
                       sqrt(state(k+1,l,2)**2+state(k+1,l,3)**2)/state(k+1,l,1) + sqrt(gamma*pression(k+1,l)/state(k+1,l,1)))

      elseif(m==2)then
         maxspeed = max(sqrt(state(k,l,2)**2+state(k,l,3)**2)/state(k,l,1) + sqrt(gamma*pression(k,l)/state(k,l,1)), &
                       sqrt(state(k,l+1,2)**2+state(k,l+1,3)**2)/state(k,l+1,1) + sqrt(gamma*pression(k,l+1)/state(k,l+1,1)))
      endif
      return
   end function

end program cocoricode

! cd D:/Life/Studies/5_M2\ -\ Magist?re\ 3/6_\ Projet\ Num?rique\ \(PN\)/Code
! gfortran SNR_bleu.f90 -static
! ./a.exe

! makevid1.txt

! ./a.exe
! makeimg.txt
! makevidfromimg1.txt

! gfortran -fcheck=bounds -g -fbacktrace SNR.f90 -static

! diff 01.dat 02.dat

!Ctrl + /  &  Ctrl + k + u