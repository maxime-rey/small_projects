program cocoricode

  ! cd C:/Users/Maxime/Desktop/Projet\ Num/Code/
  ! gfortran SNR_bleu.f90 -static
  ! gfortran -fcheck=bounds -g -fbacktrace SNR.f90 -static

  implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Initialisation paramètres
  real(kind=8),parameter    :: Taille_simu=1d20
  integer(kind=4),parameter :: nx = 100, ny = 100
  real(kind=8)              :: dx = Taille_simu/nx,  courant_factor = 0.8
  real(kind=8),parameter    :: gamma = 1.4, rho0 = 1.67d-24, E0 = 1d30
  real(kind=8)    :: state(0:nx+1,0:ny+1,1:4)
  real(kind=8)    :: flux(1:2, 0:nx, 0:ny, 1:4)
  real(kind=8)    :: dt, dt_cell, tmax=1d40, t=0
  integer(kind=4) :: i,j,istep=0,k
  real(kind=8)    :: maxspeed1, maxspeed2, cs, dens, cmax
  integer(kind=4) :: fifty = nx/2
  
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Initialisation tableaux
  state(:,:,1) = rho0                                      !Rho0
  state(:,:,2) = 0                                         !u0*Rho
  state(:,:,3) = 0                                         !v0*Rho
  state(:,:,4) = state(:,:,1)*6d9/((gamma-1))              !E0*Rho  (6d9~kT/mu*m(H)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Release the beast
  state(fifty,fifty,4) = E0/(dx**2) !/4


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Calcul des flux
!!$  cs = sqrt(gamma*pression(fifty,fifty)/state(fifty,fifty,1))
!!$  dt = courant_factor / 3. * dx / cs
  do while(t<tmax)       
     if(istep==100)EXIT

     dt=1d10
     
     do j = 0,ny
        do i = 0,nx
           maxspeed1 = maxspeed(i,j,1)
           maxspeed2 = maxspeed(i,j,2)
           dt = min(dt,dx / max(maxspeed1,maxspeed2))
           
           flux(1,i,j,1) = (subflux1(i+1,j) + subflux1(i,j))/2     - 0.5d0*maxspeed1*(state(i+1,j,1)-state(i,j,1))
           flux(2,i,j,1) = (subflux2(i,j+1) + subflux2(i,j))/2     - 0.5d0*maxspeed2*(state(i,j+1,1)-state(i,j,1))

           flux(1,i,j,2) = (subflux3(i+1,j) + subflux3(i,j))/2     - 0.5d0*maxspeed1*(state(i+1,j,2)-state(i,j,2))   
           flux(2,i,j,2) = (subflux4_5(i,j+1) + subflux4_5(i,j))/2 - 0.5d0*maxspeed2*(state(i,j+1,2)-state(i,j,2))                 !!!!!!!!Green-Ostrogradsky ? 

           flux(1,i,j,3) = (subflux4_5(i+1,j) + subflux4_5(i,j))/2 - 0.5d0*maxspeed1*(state(i+1,j,3)-state(i,j,3))
           flux(2,i,j,3) = (subflux6(i,j+1) + subflux6(i,j))/2     - 0.5d0*maxspeed2*(state(i,j+1,3)-state(i,j,3))

           flux(1,i,j,4) = (subflux7(i+1,j) + subflux7(i,j))/2     -0.5d0*maxspeed1*(state(i+1,j,4)-state(i,j,4))
           flux(2,i,j,4) = (subflux8(i,j+1) + subflux8(i,j))/2     -0.5d0*maxspeed2*(state(i,j+1,4)-state(i,j,4))
        end do
     end do

     dt = dt * courant_factor / 3.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Compute cells' state
     do k=1,4
        do j = 1,ny
           do i = 1,nx
              state(i,j,k) = state(i,j,k) - dt/dx*(flux(1,i,j,k) - flux(1,i-1,j,k) + flux(2,i,j,k) - flux(2,i,j-1,k))
           end do
        end do
     end do
     t = t + dt

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Boundaries
     state(:,0,:) = state(:,1,:)    
     state(:,ny+1,:) = state(:,ny,:) 
     state(0,:,:) = state(1,:,:)    
     state(nx+1,:,:) = state(nx,:,:)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! COmpute new dt

     istep = istep +1
     print*,istep,dt,sum(state(:,:,4))
     if(mod(istep,1)==0)then 
        call SaveJeje !save
     end if
  end do

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Save
  subroutine SaveJeje
    CHARACTER(LEN=256) :: filename
    real(kind=8) :: vx,vy,rho,P

    write(filename,"(A,I5.5)")"results_",istep
    !open(10,file=TRIM(filename),form='formatted')
    open(10,file=TRIM(filename),form='unformatted')
    !write(10,*) nx
    write(10) nx
    !write(10,'(5(e18.12,1x))') t, gamma, rho0, E0,Taille_simu
    write(10) t, gamma, rho0, E0,Taille_simu
    do j=1,ny                           
       do i=1,nx
          rho = state(i,j,1)
          vx = state(i,j,2)!/rho; vx = vx/abs(vx) * max(1d-30,abs(vx))
          vy = state(i,j,3)!/rho; vy = vy/abs(vy) * max(1d-30,abs(vy))
          P   = pression(i,j)            
          !write(10,'(4(e18.12,1x))') rho,vx,vy,P
          write(10) rho,vx,vy,P
       end do
    end do
    close(10)
  end subroutine SaveJeje
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Pression
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
  end function pression

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Subflux
  function subflux1(k,l)
    real(kind=8) :: subflux1
    integer(kind=4) :: k,l
    subflux1 = state(k,l,2)
    return
  end function subflux1

  function subflux2(k,l)
    real(kind=8) :: subflux2
    integer(kind=4) :: k,l
    subflux2 = state(k,l,3)
    return
  end function subflux2

  function subflux3(k,l)
    real(kind=8) :: subflux3
    integer(kind=4) :: k,l
    real(kind=8) :: rho,u
    rho=state(k,l,1)
    u=state(k,l,2)/rho
    subflux3 = rho*u**2 + pression(k,l)
    return
  end function subflux3


  function subflux4_5(k,l)
    real(kind=8) :: subflux4_5
    integer(kind=4) :: k,l
    real(kind=8) :: rho,u,v
    rho=state(k,l,1)
    u=state(k,l,2)/rho
    v=state(k,l,3)/rho
    subflux4_5 = rho*u*v
    return
  end function subflux4_5

  function subflux6(k,l)
    real(kind=8) :: subflux6
    integer(kind=4) :: k,l
    real(kind=8) :: rho,v
    rho=state(k,l,1)
    v=state(k,l,3)/rho
    subflux6 = rho*v**2 + pression(k,l)
    return
  end function subflux6

  function subflux7(k,l)
    real(kind=8) :: subflux7
    integer(kind=4) :: k,l
    real(kind=8) :: rho,u,E
    rho=state(k,l,1)
    u=state(k,l,2)/rho
    E=state(k,l,4)/rho
    subflux7 = rho*u*(E + pression(i,j)/rho)
    return
  end function subflux7

  function subflux8(k,l)
    real(kind=8) :: subflux8
    integer(kind=4) :: k,l
    real(kind=8) :: rho,v,E
    rho=state(k,l,1)
    v=state(k,l,3)/rho
    E=state(k,l,4)/rho
    subflux8 = rho*v*(E + pression(i,j)/rho)
    return
  end function subflux8

  function maxspeed(k,j,m)
    real(kind=8) :: maxspeed,u_L,v_L,d_L,c_L,P_L,E_L,u_R,v_r,d_R,c_R,P_R,E_R
    integer(kind=4) :: k,j,m
    if (m == 1) then 
       d_L = state(k,j,1)     ! density [ g / cm3 ]
       u_L = state(k,j,2)/d_L ! u [ cm / s ]
       v_L = state(k,j,3)/d_L ! v [ cm / s ]
       E_L = state(k,j,4)     !  energy density [ erg / cm3 ]
       P_L = (gamma - 1.0d0) * ( E_L - 0.5d0*d_L*(u_L*u_L+v_L*v_L))  ! Pressure [cgs]
       c_L = sqrt(gamma * P_L / d_L) ! sound speed [ cm / s ]
       ! right state
       d_R = state(k+1,j,1)     ! density [ g / cm3 ]
       u_R = state(k+1,j,2)/d_R ! u [ cm / s ]
       v_R = state(k+1,j,3)/d_R ! v [ cm / s ]
       E_R = state(k+1,j,4)     !  energy density [ erg / cm3 ]
       P_R = (gamma - 1.0d0) * ( E_R - 0.5d0*d_R*(u_R*u_R+v_R*v_R))  ! Pressure [cgs]
       c_R = sqrt(gamma * P_R / d_R) ! sound speed [ cm / s ]
       ! maximum wave speed
       maxspeed = max(abs(u_L)+c_L,abs(u_R)+c_R)
    else
       d_L = state(k,j,1)     ! density [ g / cm3 ]
       u_L = state(k,j,2)/d_L ! u [ cm / s ]
       v_L = state(k,j,3)/d_L ! v [ cm / s ]
       E_L = state(k,j,4)     !  energy density [ erg / cm3 ]
       P_L = (gamma - 1.0d0) * ( E_L - 0.5d0*d_L*(u_L*u_L+v_L*v_L))  ! Pressure [cgs]
       c_L = sqrt(gamma * P_L / d_L) ! sound speed [ cm / s ]
       ! right state
       d_R = state(k,j+1,1)     ! density [ g / cm3 ]
       u_R = state(k,j+1,2)/d_R ! u [ cm / s ]
       v_R = state(k,j+1,3)/d_R ! v [ cm / s ]
       E_R = state(k,j+1,4)     !  energy density [ erg / cm3 ]
       P_R = (gamma - 1.0d0) * ( E_R - 0.5d0*d_R*(u_R*u_R+v_R*v_R))  ! Pressure [cgs]
       c_R = sqrt(gamma * P_R / d_R) ! sound speed [ cm / s ]
       ! maximum wave speed
       maxspeed = max(abs(u_L)+c_L,abs(u_R)+c_R)
    end if
    
!!$    if(m==1)then
!!$       maxspeed = max(sqrt(state(k,l,2)**2+state(k,l,3)**2)/state(k,l,1) + sqrt(gamma*pression(k,l)/state(k,l,1)), &
!!$            sqrt(state(k+1,l,2)**2+state(k+1,l,3)**2)/state(k+1,l,1) + sqrt(gamma*pression(k+1,l)/state(k+1,l,1)))
!!$
!!$    elseif(m==2)then
!!$       maxspeed = max(sqrt(state(k,l,2)**2+state(k,l,3)**2)/state(k,l,1) + sqrt(gamma*pression(k,l)/state(k,l,1)), &
!!$            sqrt(state(k,l+1,2)**2+state(k,l+1,3)**2)/state(k,l+1,1) + sqrt(gamma*pression(k,l+1)/state(k,l+1,1)))
!!$    endif
    return
  end function maxspeed

end program cocoricode

!Ctrl + /  &  Ctrl + k + u

! cd C:/Users/Maxime/Desktop/Projet\ Num/Code/
! gfortran SNR_bleu.f90 -static

! ./a.exe
! makevid1.txt

! ./a.exe
! makeimg.txt
! makevidfromimg1.txt

! gfortran -fcheck=bounds -g -fbacktrace SNR.f90 -static

! diff 01.dat 02.dat
