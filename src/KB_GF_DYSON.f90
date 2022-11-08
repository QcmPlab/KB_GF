MODULE KB_GF_DYSON
  USE KB_VARS_GLOBAL
  USE KB_CONTOUR
  USE KB_AUX
  USE KB_GF_COMMON
  USE KB_GF_FREE
  USE SCIFOR, only: one,xi,zero,pi,zeye,inv,assert_shape
  implicit none
  private


  ! CONVOLUTION:
  interface dyson_kb_gf
     module procedure :: dyson_kb_gf_rank0
     module procedure :: dyson_kb_gf_d1
     module procedure :: dyson_kb_gf_d1_
     module procedure :: dyson_kb_gf_d2     
     module procedure :: dyson_kb_gf_d3
     module procedure :: dyson_kb_gf_d3_
     module procedure :: dyson_kb_gf_d4
     module procedure :: dyson_kb_gf_d5
     module procedure :: dyson_kb_gf_d5_
     module procedure :: dyson_kb_gf_d6
     module procedure :: dyson_kb_gf_d6_
     module procedure :: dyson_kb_gf_d7
     module procedure :: dyson_kb_gf_d7_
     module procedure :: dyson_kb_gf_d7__
  end interface dyson_kb_gf


  public :: dyson_kb_gf


  logical :: imsg=.true.

contains



  !----------------------------------------------------------------------------
  !  This subroutine solves a Volterra integro-differential equation of 
  !  the second kind,
  !              [i*d/dt-h(t)]G(t,t') = delta(t,t') + (K*G)(t,t'),
  !  for t=n*dt or t'=n*dt, using 2^nd implicit Runge-Kutta method.
  !
  ! TYPE: X=RET,LESS,LMIX (R,<,\LMIX)
  !i d/dt G^x(t,:)  = h(t)G^x(t,:) + Q^x(t,:) + int_{t'}^{t}K^R(t,s)G^x(s,:)ds
  !
  !Q^x contains further integrals of the form K^a*G*b as obtained from Langreth
  !rule's decomposition of the convolution.
  !----------------------------------------------------------------------------
  subroutine dyson_kb_gf_rank0(Hk,K,Gk,dGk,dGk_new)
    complex(8),dimension(:)             :: Hk
    type(kb_gf),intent(in)              :: K
    type(kb_gf),intent(inout)           :: Gk
    type(kb_dgf),intent(inout)          :: dGk
    type(kb_dgf)                        :: dGk_new
    integer                             :: N,L,Niw
    real(8)                             :: dt,dtau
    integer                             :: i,j,itau,jtau,s
    complex(8),dimension(:),allocatable :: KxGk
    real(8),dimension(:),allocatable    :: ftau
    complex(8)                          :: dGk_less,A
    !
    N   = cc_params%Nt                 !<== work with the ACTUAL size of the contour
    L   = cc_params%Ntau
    Niw = cc_params%Niw
    dt  = cc_params%dt
    dtau= cc_params%dtau
    !
    call assert_shape(Hk,[cc_params%Ntime],"dyson_kb_gf_rank0","Hk")
    if(K%is_zero())then
       if(imsg)write(*,"(A)") " MSG dyson_kb_gf: K=0 using free_kb_gf"
       imsg=.false.
       call free_kb_gf(Hk,Gk,dGk,dGk_new)
       return
    endif
    !
    allocate(KxGk(0:max(N,L)))
    !
    !Treat here the t=0,0 case:
    !We solve the matsubara part using FFT
    if(N==1)then
       !INITIALIZE THE WEISS FIELD Gk^{x=M,<,R,\lmix} for N=1
       !neq_setup_contour_gf:
       Gk%iw = one/(xi*cc_params%wm(:) + xmu - Hk(1) - K%iw(:))
       allocate(ftau(0:Niw))
       call fft_iw2tau(Gk%iw,ftau(0:),cc_params%beta)
       call fft_extract_gtau(ftau(0:),Gk%mats(0:))
       Gk%less(1,1) = -xi*Gk%mats(L)
       Gk%ret(1,1)  = -xi
       Gk%lmix(1,0:L)=-xi*Gk%mats(L:0:-1)
       deallocate(ftau)
       !
       !neq_setup_contour_dgf:
       !get d/dt G_k^R = -i H(k,0)G_k^R
       dGk_new%ret(1)  = -xi*Hk(1)*Gk%ret(1,1)
       !
       !get d/dt G_k^< = -i H(k,0)G_k^< -xi(-xi)int_0^beta S^\lmix*G_k^\rmix
       dGk_new%less(1) = -xi*Hk(1)*Gk%less(1,1)
       do s=0,L
          KxGk(s)=K%lmix(1,s)*conjg(Gk%lmix(1,L-s))
       end do
       dGk_new%less(1) = dGk_new%less(1) - xi*(-xi)*cc_params%dtau*kb_trapz(KxGk(0:),0,L)
       !
       !get d/dt G_k^\lmix = -xi*H(k,0)*G_k^\lmix - xi*int_0^beta G_k^\lmix*G_k^M
       dGk_new%lmix(0:)= -xi*Hk(1)*Gk%lmix(1,0:)
       do j=0,L
          do s=0,j
             KxGk(s)=K%lmix(1,s)*Gk%mats(s+L-j)
          end do
          dGk_new%lmix(j)=dGk_new%lmix(j)+xi*cc_params%dtau*kb_trapz(KxGk(0:),0,j)
          do s=j,L
             KxGk(s)=K%lmix(1,s)*Gk%mats(s-j)
          end do
          dGk_new%lmix(j)=dGk_new%lmix(j)-xi*cc_params%dtau*kb_trapz(KxGk(0:),j,L) 
       enddo
       !
       return
       !
    end if
    !
    !TYPE: X=RET,LESS,LMIX (R,<,\LMIX)
    !I D/DT Gk^X(T,:)  = Hk(T)Gk^X(T,:) + Q^X(T,:) + INT_{0,T'}^{T}K^R(T,S)Gk^X(S,:)DS
    !
    !Ret component
    ! d/dt Gk^R(t,:) = -i*h(t)*Gk^R(t,:) -i*delta(t,:) - i\int_{:}^t K^R(t,s)*Gk^R(s,:)ds
    Gk%ret(N,N)=-xi
    dGk_new%ret(N)=-xi*Hk(N)*Gk%ret(N,N)
    do j=1,N-1
       Gk%ret(N,j)=Gk%ret(N-1,j) + 0.5d0*dt*dGk%ret(j)
       do s=j,N-1
          KxGk(s)=K%ret(N,s)*Gk%ret(s,j)
       enddo
       dGk_new%ret(j)=-xi*dt*kb_half_trapz(KxGk(0:),j,N-1)
       !
       Gk%ret(N,j)=Gk%ret(N,j) + 0.5d0*dt*dGk_new%ret(j)
       Gk%ret(N,j)=Gk%ret(N,j)/(1.d0 + 0.5d0*xi*dt*Hk(N) + 0.25d0*xi*dt**2*K%ret(N,N))
       !
       dGk_new%ret(j)=dGk_new%ret(j) - xi*Hk(N)*Gk%ret(N,j) - 0.5d0*xi*dt*K%ret(N,N)*Gk%ret(N,j)
    enddo
    !
    !Lmix component
    !d/dt Gk^\lmix(t,:) = -i*Hk(t)*Gk^\lmix(t,:) -i\int_0^\beta K^\lmix(t,s)*Gk^M(s,:)ds -i\int_0^t K^R(t,s)Gk^\lmix(s,:)ds
    do jtau=0,L
       Gk%lmix(N,jtau)=Gk%lmix(N-1,jtau)+0.5d0*dt*dGk%lmix(jtau)
       do s=0,jtau
          KxGk(s)=K%lmix(N,s)*Gk%mats(s+L-jtau)
       end do
       dGk_new%lmix(jtau)=xi*dtau*kb_trapz(KxGk(0:),0,jtau)
       do s=jtau,L
          KxGk(s)=K%lmix(N,s)*Gk%mats(s-jtau)
       end do
       dGk_new%lmix(jtau)=dGk_new%lmix(jtau)-xi*dtau*kb_trapz(KxGk(0:),jtau,L)!<= add -iQ(t)
       !
       do s=1,N-1
          KxGk(s)=K%ret(N,s)*Gk%lmix(s,jtau)
       end do
       dGk_new%lmix(jtau)=dGk_new%lmix(jtau)-xi*dt*kb_half_trapz(KxGk(0:),1,N-1)
       !
       Gk%lmix(N,jtau)=Gk%lmix(N,jtau) + 0.5d0*dt*dGk_new%lmix(jtau)
       Gk%lmix(N,jtau)=Gk%lmix(N,jtau)/(1.d0 + 0.5d0*xi*dt*Hk(N) + 0.25d0*xi*dt**2*K%ret(N,N))
       !
       dGk_new%lmix(jtau)=dGk_new%lmix(jtau)-xi*Hk(N)*Gk%lmix(N,jtau)-0.5d0*xi*dt*K%ret(N,N)*Gk%lmix(N,jtau)
    end do
    !
    !Less component
    !d/dt Gk^<(t,:) = -i*Hk(t)*Gk^<(t,:) -i*[ (-i)*\int_0^\beta K^\lmix(t,s)*Gk^\rmix(s,:)ds + \int_0^{:}K^<(t,s)*Gk^A(s,:)ds ]
    !                                 -i*\int_0^t K^R(t,s)*Gk^<(s,:)ds
    !
    ! Gk^<(t_{N},t_{j}), d/dt Gk^<(t_{N},t_{j}) <== lower-right triangle
    do j=1,N-1
       Gk%less(N,j)=Gk%less(N-1,j) + 0.5d0*dt*dGk%less(j)
       do s=0,L
          KxGk(s)=K%lmix(N,s)*conjg(Gk%lmix(j,L-s))
       end do
       dGk_new%less(j)=-xi*(-xi)*dtau*kb_trapz(KxGk(0:),0,L)
       do s=1,j
          KxGk(s)=K%less(N,s)*conjg(Gk%ret(j,s))
       end do
       dGk_new%less(j)=dGk_new%less(j)-xi*dt*kb_trapz(KxGk(0:),1,j)!<= -iQ(t)
       !
       do s=1,N-1
          KxGk(s)=K%ret(N,s)*Gk%less(s,j)
       end do
       dGk_new%less(j)=dGk_new%less(j)-xi*dt*kb_half_trapz(KxGk(0:),1,N-1)
       !
       Gk%less(N,j)=Gk%less(N,j) + 0.5d0*dt*dGk_new%less(j)
       Gk%less(N,j)=Gk%less(N,j)/(1.d0 + 0.5d0*xi*dt*Hk(N) + 0.25d0*xi*dt**2*K%ret(N,N))
       !
       dGk_new%less(j)=dGk_new%less(j)-xi*Hk(N)*Gk%less(N,j)-0.5d0*xi*dt*K%ret(N,N)*Gk%less(N,j)
    end do
    !
    ! Gk^<(t_{i},t_{N}), d/dt Gk^<(t_{i},t_{N}) <== upper left triangle
    ! Hermitian conjugate Gk
    do i=1,N-1
       Gk%less(i,N)=-conjg(Gk%less(N,i))
    end do
    !
    ! d/dt Gk^<(t_{N-1},t_{N})
    dGk_less=-xi*Hk(N-1)*Gk%less(N-1,N)
    do s=0,L
       KxGk(s)=K%lmix(N-1,s)*conjg(Gk%lmix(N,L-s))
    end do
    dGk_less=dGk_less-xi*(-xi)*dtau*kb_trapz(KxGk(0:),0,L)
    do s=1,N
       KxGk(s)=K%less(N-1,s)*conjg(Gk%ret(N,s))
    end do
    dGk_less=dGk_less-xi*dt*kb_trapz(KxGk(0:),1,N)
    do s=1,N-1
       KxGk(s)=K%ret(N-1,s)*Gk%less(s,N)
    end do
    dGk_less=dGk_less-xi*dt*kb_trapz(KxGk(0:),1,N-1)
    !
    !Gk^<(N,N), d/dt Gk^<(N,N)
    !d/dt Gk <== d/dt Gk_new
    Gk%less(N,N)=Gk%less(N-1,N)+0.5d0*dt*dGk_less
    do s=0,L
       KxGk(s)=K%lmix(N,s)*conjg(Gk%lmix(N,L-s))
    end do
    dGk_new%less(N)=-xi*(-xi)*dtau*kb_trapz(KxGk(0:),0,L)
    !
    do s=1,N
       KxGk(s)=K%less(N,s)*conjg(Gk%ret(N,s))
    end do
    dGk_new%less(N)=dGk_new%less(N)-xi*dt*kb_trapz(KxGk(0:),1,N)
    !
    do s=1,N-1
       KxGk(s)=K%ret(N,s)*Gk%less(s,N)
    end do
    dGk_new%less(N)=dGk_new%less(N)-xi*dt*kb_half_trapz(KxGk(0:),1,N-1)
    !
    Gk%less(N,N)=Gk%less(N,N)+0.5d0*dt*dGk_new%less(N)
    Gk%less(N,N)=Gk%less(N,N)/(1.d0+0.5d0*xi*dt*Hk(N)+0.25d0*xi*dt**2*K%ret(N,N))
    !
    dGk_new%less(N)=dGk_new%less(N)-xi*Hk(N)*Gk%less(N,N)-0.5d0*xi*dt*K%ret(N,N)*Gk%less(N,N)
    !
    deallocate(KxGk)
    !
  end subroutine dyson_kb_gf_rank0



  subroutine dyson_kb_gf_rank2(Hk,K,Gk,dGk,dGk_new)
    complex(8),dimension(:,:,:)           :: Hk      ![Nso][Nso][Nt]
    type(kb_gf),intent(in)                :: K(:,:)  ![Nso][Nso]
    type(kb_gf),intent(inout)             :: Gk(:,:)     !as K
    type(kb_dgf),intent(inout)            :: dGk(:,:)    !as K
    type(kb_dgf)                          :: dGk_new(:,:)!as K
    integer                               :: N,L,Nso,Niw
    real(8)                               :: dt,dtau
    integer                               :: i,j,itau,jtau,s
    integer                               :: io,jo,ko
    complex(8)                            :: HxGk
    complex(8),dimension(:),allocatable   :: KxGk
    real(8),dimension(:),allocatable      :: ftau
    complex(8),dimension(:,:),allocatable :: Amat,GkxAmat,Gkmat,dGkless
    !
    N   = cc_params%Nt                 !<== work with the ACTUAL size of the contour
    L   = cc_params%Ntau
    Niw = cc_params%Niw
    dt  = cc_params%dt
    dtau= cc_params%dtau
    Nso = size(Hk,1)            !shape was asserted before entering
    !
    if(all(K%is_zero()))then
       if(imsg)write(*,"(A)") " MSG dyson_kb_gf: K=0 using free_kb_gf"
       imsg=.false.
       call free_kb_gf(Hk,Gk,dGk,dGk_new)
       return
    endif
    !
    allocate(KxGk(0:max(N,L)))
    !
    allocate(Amat(Nso,Nso))
    allocate(Gkmat(Nso,Nso))
    allocate(GkxAmat(Nso,Nso))
    allocate(dGkless(Nso,Nso))
    Amat   =zero
    Gkmat  =zero
    GkxAmat=zero
    dGkless=zero
    !
    !Treat here the t=0,0 case:
    !We solve the matsubara part using FFT
    if(N==1)then
       !INITIALIZE THE WEISS FIELD Gk^{x=M,<,R,\lmix} for N=1
       !neq_setup_gf:
       do i=1,Niw
          Amat  =(xi*cc_params%wm(i)+xmu)*zeye(Nso) - Hk(:,:,1)
          forall(io=1:Nso,jo=1:Nso)Gkmat(io,jo) = Amat(io,jo)  - K(io,jo)%iw(i)
          call inv(Gkmat)
          forall(io=1:Nso,jo=1:Nso)Gk(io,jo)%iw(i) = Gkmat(io,jo)
       enddo
       !
       allocate(ftau(0:Niw))
       do io=1,Nso
          do jo=1,Nso
             call fft_iw2tau(Gk(io,jo)%iw,ftau(0:),cc_params%beta)
             call fft_extract_gtau(ftau(0:),Gk(io,jo)%mats(0:))
             Gk(io,jo)%less(1,1) = -xi*Gk(io,jo)%mats(L)
             Gk(io,jo)%lmix(1,0:L)=-xi*Gk(io,jo)%mats(L:0:-1)
          enddo
          Gk(io,io)%ret(1,1)  = -xi
       enddo
       deallocate(ftau)
       !
       !neq_setup_dgf:
       do concurrent (io=1:Nso,jo=1:Nso)
          !get d/dt G_k^R = -i H(k,0)G_k^R
          HxGk=zero
          do ko=1,Nso
             HxGk = HxGk - xi*Hk(io,ko,1)*Gk(ko,jo)%ret(1,1)
          enddo
          dGk_new(io,jo)%ret(1) = HxGk
          !
          !get d/dt G_k^< = -i H(k,0)G_k^< -xi(-xi)int_0^beta S^\lmix*G_k^\rmix
          HxGk=zero
          do ko=1,Nso
             HxGk = HxGk - xi*Hk(io,ko,1)*Gk(ko,jo)%less(1,1)
          enddo
          !
          KxGk(0:)=zero
          do s=0,L
             do ko=1,Nso
                KxGk(s)=KxGk(s)+K(io,ko)%lmix(1,s)*conjg( Gk(ko,jo)%lmix(1,L-s) )
             enddo
          enddo
          dGk_new(io,jo)%less(1) = HxGk - xi*(-xi)*cc_params%dtau*kb_trapz(KxGk(0:),0,L)
          !
          !get d/dt G_k^\lmix = -xi*H(k,0)*G_k^\lmix - xi*int_0^beta G_k^\lmix*G_k^M
          KxGk(0:)=zero
          do ko=1,Nso
             KxGk(0:) = KxGk(0:) - xi*Hk(io,ko,1)*Gk(ko,jo)%lmix(1,0:)
          enddo
          dGk_new(io,jo)%lmix(0:)= KxGk(0:)
          do j=0,L
             KxGk(0:)=zero
             do s=0,j
                do ko=1,Nso
                   KxGk(s)=KxGk(s) + K(io,ko)%lmix(1,s)*Gk(ko,jo)%mats(s+L-j)
                enddo
             end do
             dGk_new(io,jo)%lmix(j)=dGk_new(io,jo)%lmix(j)+xi*cc_params%dtau*kb_trapz(KxGk(0:),0,j)
             KxGk(0:)=zero
             do s=j,L
                do ko=1,Nso
                   KxGk(s)=KxGk(s)+K(io,ko)%lmix(1,s)*Gk(ko,jo)%mats(s-j)
                enddo
             enddo
             dGk_new(io,jo)%lmix(j)=dGk_new(io,jo)%lmix(j)-xi*cc_params%dtau*kb_trapz(KxGk(0:),j,L)
          enddo
          !
       enddo
       !
       return
       !
    end if
    !
    !
    !TYPE: X=RET,LESS,LMIX (R,<,\LMIX)
    !I D/DT Gk^X(T,:)  = Hk(T)Gk^X(T,:) + Q^X(T,:) + INT_{0,T'}^{T}K^R(T,S)Gk^X(S,:)DS
    !
    !Ret component
    ! d/dt Gk^R(t,:) = -i*h(t)*Gk^R(t,:) -i*delta(t,:) - i\int_{:}^t K^R(t,s)*Gk^R(s,:)ds
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !TIP t_{N}, t`_{N}
    do io=1,Nso
       Gk(io,io)%ret(N,N)   = -xi
       do jo=1,Nso
          dGk_new(io,jo)%ret(N)= zero
          do ko=1,Nso
             dGk_new(io,jo)%ret(N)=dGk_new(io,jo)%ret(N)-xi*Hk(io,ko,N)*Gk(ko,jo)%ret(N,N)
          enddo
       enddo
    enddo
    !
    !VERTICAL INTERVAL t_{N}, t`_{j, j=1,...,N-1}
    tp_Ret:do j=1,N-1
       Amat = zeye(Nso)
       do concurrent (io=1:Nso,jo=1:Nso)
          !Add Gk^R(t_{N-1},t_j) + dt/2* d_tGk^ret(t_{N-1},j)
          Gkmat(io,jo) = Gk(io,jo)%ret(N-1,j) + 0.5d0*dt*dGk(io,jo)%ret(j)
          !Add -xi*K^R(t_N,s)*Gk^R(s,t_j)
          KxGk(0:)=zero
          do s=j,N-1
             do ko=1,Nso
                KxGk(s)=KxGk(s)+K(io,ko)%ret(N,s)*Gk(ko,jo)%ret(s,j)
             enddo
          enddo
          dGk_new(io,jo)%ret(j) = -xi*dt*kb_half_trapz(KxGk(0:),j,N-1)
          Gkmat(io,jo)          = Gkmat(io,jo) + 0.5d0*dt*dGk_new(io,jo)%ret(j)
          Amat(io,jo)           = Amat(io,jo) + 0.5d0*xi*dt*Hk(io,jo,N) + 0.25d0*xi*dt**2*K(io,jo)%ret(N,N)
       enddo
       call inv(Amat)
       GkxAmat = matmul(Gkmat,Amat)
       forall(io=1:Nso,jo=1:Nso)Gk(io,jo)%ret(N,j) = GkxAmat(io,jo)
       !
       !
       !Update derivative d_t Gk^R(t,:) as:
       !d/dt Gk^R(t,:) = -i*h(t)*Gk^R(t,:) -i*delta(t,:) - i\int_{:}^t K^R(t,s)*Gk^R(s,:)ds
       do concurrent (io=1:Nso,jo=1:Nso)             !
          do ko=1,Nso
             dGk_new(io,jo)%ret(j) = dGk_new(io,jo)%ret(j) - xi*Hk(io,ko,N)*Gk(ko,jo)%ret(N,j)
             dGk_new(io,jo)%ret(j) = dGk_new(io,jo)%ret(j) - 0.5d0*xi*dt*K(io,ko)%ret(N,N)*Gk(ko,jo)%ret(N,j)
          enddo
       enddo
       !
    enddo tp_Ret
    !
    !
    !Lmix component
    !d/dt Gk^\lmix(t,:) = -i*Hk(t)*Gk^\lmix(t,:) -i\int_0^\beta K^\lmix(t,s)*Gk^M(s,:)ds -i\int_0^t K^R(t,s)Gk^\lmix(s,:)ds
    jtau_lmix: do jtau=0,L
       Amat = zeye(Nso)
       do concurrent (io=1:Nso,jo=1:Nso)
          !Add Gk^lmix(t_{N-1},tau_j) + dt/2* d_tGk^lmix(t_{N-1},tau_j)
          Gkmat(io,jo) = Gk(io,jo)%lmix(N-1,jtau)+0.5d0*dt*dGk(io,jo)%lmix(jtau)
          !Add Q^lmix(t_{N},tau_j) = K^lmix(N,stau)*Gk^mats(stau-jtau) stau<jtau
          KxGk(0:)=zero
          do s=0,jtau
             do ko=1,Nso
                KxGk(s)=KxGk(s)+K(io,ko)%lmix(N,s)*Gk(ko,jo)%mats(s+L-jtau)
             enddo
          enddo
          dGk_new(io,jo)%lmix(jtau) = xi*dtau*kb_trapz(KxGk(0:),0,jtau)                 
          !
          !Add Q^lmix(t_{N},tau_j) = K^lmix(N,stau)*Gk^mats(stau-jtau) stau>jtau
          KxGk(0:)=zero
          do s=jtau,L
             do ko=1,Nso
                KxGk(s)=KxGk(s)+K(io,ko)%lmix(N,s)*Gk(ko,jo)%mats(s-jtau)
             enddo
          enddo
          dGk_new(io,jo)%lmix(jtau)= dGk_new(io,jo)%lmix(jtau)-xi*dtau*kb_trapz(KxGk(0:),jtau,L)
          !
          !Add -xi*K^R(t_N,s)*Gk^lmix(s,tau_j)
          KxGk(0:)=zero
          do s=1,N-1
             do ko=1,Nso
                KxGk(s)=KxGk(s)+K(io,ko)%ret(N,s)*Gk(ko,jo)%lmix(s,jtau)
             enddo
          enddo
          dGk_new(io,jo)%lmix(jtau)=dGk_new(io,jo)%lmix(jtau)-xi*dt*kb_half_trapz(KxGk(0:),1,N-1)
          Gkmat(io,jo) = Gkmat(io,jo) + 0.5d0*dt*dGk_new(io,jo)%lmix(jtau)
          Amat(io,jo)  = Amat(io,jo)  + 0.5d0*xi*dt*Hk(io,jo,N) + 0.25d0*xi*dt**2*K(io,jo)%ret(N,N)
       enddo
       call inv(Amat)
       GkxAmat = matmul(Gkmat,Amat)
       forall(io=1:Nso,jo=1:Nso)Gk(io,jo)%lmix(N,jtau) = GkxAmat(io,jo)
       !
       !
       !Update derivative d_t Gk^\lmix(t,:) as: (dGkmat already holds all the terms not included below)
       !d/dt Gk^\lmix(t,:) = -i*Hk(t)*Gk^\lmix(t,:) -i\int_0^\beta K^\lmix(t,s)*Gk^M(s,:)ds -i\int_0^t K^R(t,s)Gk^\lmix(s,:)ds
       do concurrent (io=1:Nso,jo=1:Nso)
          !
          do ko=1,Nso
             dGk_new(io,jo)%lmix(jtau) = dGk_new(io,jo)%lmix(jtau)- xi*Hk(io,ko,N)*Gk(ko,jo)%lmix(N,jtau)
             dGk_new(io,jo)%lmix(jtau) = dGk_new(io,jo)%lmix(jtau)- 0.5d0*xi*dt*K(io,ko)%ret(N,N)*Gk(ko,jo)%lmix(N,jtau)
          enddo
          !
       enddo
       !
    enddo jtau_lmix
    !
    !
    !
    !
    !Less component
    !d/dt Gk^<(t,:) = -i*Hk(t)*Gk^<(t,:) -i*[ (-i)*\int_0^\beta K^\lmix(t,s)*Gk^\rmix(s,:)ds + \int_0^{:}K^<(t,s)*Gk^A(s,:)ds ]
    !                                 -i*\int_0^t K^R(t,s)*Gk^<(s,:)ds
    !
    ! Gk^<(t_{N},t_{j}), d/dt Gk^<(t_{N},t_{j}) <== lower-right triangle
    tp_less: do j=1,N-1
       Amat = zeye(Nso)
       do concurrent (io=1:Nso,jo=1:Nso)
          Gkmat(io,jo) = Gk(io,jo)%less(N-1,j) + 0.5d0*dt*dGk(io,jo)%less(j)
          KxGk(0:)=zero
          do s=0,L
             do ko=1,Nso
                KxGk(s)=KxGk(s)+K(io,ko)%lmix(N,s)*conjg( Gk(ko,jo)%lmix(j,L-s) ) !rmix <-- lmix
             enddo
          enddo
          dGk_new(io,jo)%less(j) = -xi*(-xi)*dtau*kb_trapz(KxGk(0:),0,L)
          !
          KxGk(0:)=zero
          do s=1,j
             do ko=1,Nso
                KxGk(s)=KxGk(s)+K(io,ko)%less(N,s)*conjg( Gk(ko,jo)%ret(j,s) ) !adv <-- ret
             enddo
          enddo
          dGk_new(io,jo)%less(j)=dGk_new(io,jo)%less(j)-xi*dt*kb_trapz(KxGk(0:),1,j) !<= -iQ(t)
          !
          KxGk(0:)=zero
          do s=1,N-1
             do ko=1,Nso
                KxGk(s)=KxGk(s)+K(io,ko)%ret(N,s)*Gk(ko,jo)%less(s,j)
             enddo
          enddo
          dGk_new(io,jo)%less(j)=dGk_new(io,jo)%less(j)-xi*dt*kb_half_trapz(KxGk(0:),1,N-1)
          Gkmat(io,jo) = Gkmat(io,jo) + 0.5d0*dt*dGk_new(io,jo)%less(j)
          Amat(io,jo)  = Amat(io,jo)  + 0.5d0*xi*dt*Hk(io,jo,N) + 0.25d0*xi*dt**2*K(io,jo)%ret(N,N)
       enddo
       call inv(Amat)
       GkxAmat=matmul(Gkmat,Amat)
       forall(io=1:Nso,jo=1:Nso)Gk(io,jo)%less(N,j)= GkxAmat(io,jo)
       !
       !Update derivative d_t Gk^<(t,:) as: (dGkmat already holds all the terms not included below)
       !d/dt Gk^<(t,:) = -i*Hk(t)*Gk^<(t,:) -i*[ (-i)*\int_0^\beta K^\lmix(t,s)*Gk^\rmix(s,:)ds + \int_0^{:}K^<(t,s)*Gk^A(s,:)ds ]
       !                                 -i*\int_0^t K^R(t,s)*Gk^<(s,:)ds
       do concurrent (io=1:Nso,jo=1:Nso)
          !
          do ko=1,Nso
             dGk_new(io,jo)%less(j) = dGk_new(io,jo)%less(j) - xi*Hk(io,ko,N)*Gk(ko,jo)%less(N,j)
             dGk_new(io,jo)%less(j) = dGk_new(io,jo)%less(j) - 0.5d0*xi*dt*K(io,ko)%ret(N,N)*Gk(ko,jo)%less(N,j)
          enddo
          !
       enddo
       !
    enddo tp_less
    !
    !
    ! Gk^<(t_{i},t_{N}), d/dt Gk^<(t_{i},t_{N}) <== upper left triangle
    ! Hermitian conjugate Gk
    do concurrent (i=1:N-1,io=1:Nso,jo=1:Nso)
       Gk(io,jo)%less(i,N)=-conjg(Gk(jo,io)%less(N,i))
    enddo
    !
    !
    ! d/dt Gk^<(t_{N-1},t_{N})
    dGkless=zero
    do concurrent (io=1:Nso,jo=1:Nso)
       !
       do ko=1,Nso
          dGkless(io,jo) = dGkless(io,jo)-xi*Hk(io,ko,N-1)*Gk(ko,jo)%less(N-1,N)
       enddo
       !
       KxGk(0:)=zero
       do s=0,L
          do ko=1,Nso
             KxGk(s)=KxGk(s)+K(io,ko)%lmix(N-1,s)*conjg( Gk(ko,jo)%lmix(N,L-s) )!rmix <-- lmix
          enddo
       enddo
       dGkless(io,jo) = dGkless(io,jo)-xi*(-xi)*dtau*kb_trapz(KxGk(0:),0,L)
       !
       KxGk(0:)=zero
       do s=1,N
          do ko=1,Nso
             KxGk(s)=KxGk(s)+K(io,ko)%less(N-1,s)*conjg( Gk(ko,jo)%ret(N,s) ) !adv <-- ret
          enddo
       enddo
       dGkless(io,jo) = dGkless(io,jo)-xi*dt*kb_trapz(KxGk(0:),1,N)
       !
       KxGk(0:)=zero
       do s=1,N-1
          do ko=1,Nso
             KxGk(s)=KxGk(s)+K(io,ko)%ret(N-1,s)*Gk(ko,jo)%less(s,N)
          enddo
       enddo
       dGkless(io,jo) = dGkless(io,jo)-xi*dt*kb_trapz(KxGk(0:),1,N-1)
    enddo
    !
    !
    !Gk^<(N,N), d/dt Gk^<(N,N)
    Amat = zeye(Nso)
    do concurrent (io=1:Nso,jo=1:Nso)
       Gkmat(io,jo) = Gk(io,jo)%less(N-1,N)+0.5d0*dt*dGkless(io,jo)
       KxGk(0:)=zero
       do s=0,L
          do ko=1,Nso
             KxGk(s)=KxGk(s)+K(io,ko)%lmix(N,s)*conjg( Gk(ko,jo)%lmix(N,L-s) ) !get_rmix(Gk,s,N,L)
          enddo
       enddo
       dGk_new(io,jo)%less(N)=-xi*(-xi)*dtau*kb_trapz(KxGk(0:),0,L) !this one reset dGkmat
       !
       KxGk(0:)=zero
       do s=1,N
          do ko=1,Nso
             KxGk(s)=KxGk(s)+K(io,ko)%less(N,s)*conjg( Gk(ko,jo)%ret(N,s) ) !get_adv(Gk,s,N)
          enddo
       enddo
       dGk_new(io,jo)%less(N)=dGk_new(io,jo)%less(N) - xi*dt*kb_trapz(KxGk(0:),1,N)
       !
       KxGk(0:)=zero
       do s=1,N-1
          do ko=1,Nso
             KxGk(s)=KxGk(s)+K(io,ko)%ret(N,s)*Gk(ko,jo)%less(s,N)
          enddo
       enddo
       dGk_new(io,jo)%less(N)=dGk_new(io,jo)%less(N) - xi*dt*kb_half_trapz(KxGk(0:),1,N-1)
       Gkmat(io,jo) = Gkmat(io,jo) + 0.5d0*dt*dGk_new(io,jo)%less(N)
       Amat(io,jo)  = Amat(io,jo)  + 0.5d0*xi*dt*Hk(io,jo,N) + 0.25d0*xi*dt**2*K(io,jo)%ret(N,N)
    enddo
    !
    call inv(Amat)
    GkxAmat=matmul(Gkmat,Amat)
    forall(io=1:Nso,jo=1:Nso)Gk(io,jo)%less(N,N) = GkxAmat(io,jo)
    !
    !
    !Update derivative d_t Gk^<(t,:) as: (dGkmat already holds all the terms not included below)
    !d/dt Gk^<(t,:) = -i*Hk(t)*Gk^<(t,:) -i*[ (-i)*\int_0^\beta K^\lmix(t,s)*Gk^\rmix(s,:)ds + \int_0^{:}K^<(t,s)*Gk^A(s,:)ds ]
    !                                 -i*\int_0^t K^R(t,s)*Gk^<(s,:)ds
    do concurrent (io=1:Nso,jo=1:Nso)
       !
       do ko=1,Nso
          dGk_new(io,jo)%less(N) = dGk_new(io,jo)%less(N) - xi*Hk(io,ko,N)*Gk(ko,jo)%less(N,N)
          dGk_new(io,jo)%less(N) = dGk_new(io,jo)%less(N) - 0.5d0*xi*dt*K(io,ko)%ret(N,N)*Gk(ko,jo)%less(N,N)
       enddo
       !
    enddo
    !
    !
    deallocate(KxGk)
    deallocate(Amat,Gkmat,GkxAmat,dGkless)
    !
  end subroutine dyson_kb_gf_rank2



  subroutine dyson_kb_gf_rank4(Hk,K,Gk,dGk,dGk_new)
    complex(8),dimension(:,:,:)           :: Hk      ![Nso][Nso][Nt]
    type(kb_gf),intent(in)                :: K(:,:,:,:)  ![Nspin][Nspin][Norb][Norb]
    type(kb_gf),intent(inout)             :: Gk(:,:,:,:)     !as K
    type(kb_dgf),intent(inout)            :: dGk(:,:,:,:)    !as K
    type(kb_dgf)                          :: dGk_new(:,:,:,:)!as K
    integer                               :: N,L,Nso,Niw
    real(8)                               :: dt,dtau
    integer                               :: i,j,itau,jtau,s
    integer                               :: io,jo,ko
    complex(8)                            :: HxGk
    complex(8),dimension(:),allocatable   :: KxGk
    real(8),dimension(:),allocatable      :: ftau
    complex(8),dimension(:,:),allocatable :: Amat,GkxAmat,Gkmat,dGkless
    !
    N   = cc_params%Nt                 !<== work with the ACTUAL size of the contour
    L   = cc_params%Ntau
    Niw = cc_params%Niw
    dt  = cc_params%dt
    dtau= cc_params%dtau
    Nspin = size(Gk,1) ; Norb = size(Gk,3) ; Nso=Nspin*Norb            !shape was asserted before entering
    ! call assert_shape(Hk,[Nso,Nso,cc_params%Ntime],"dyson_kb_gf_rank0","Hk")
    ! call assert_shape_kb_gf(K,[Nspin,Nspin,Norb,Norb],"dyson_kb_gf_Rank4","K")
    ! call assert_shape_kb_gf(Gk,[Nspin,Nspin,Norb,Norb],"dyson_kb_gf_Rank4","Gk")
    ! call assert_shape_kb_gf(dGk,[Nspin,Nspin,Norb,Norb],"dyson_kb_gf_Rank4","dGk")
    ! call assert_shape_kb_gf(dGk_new,[Nspin,Nspin,Norb,Norb],"dyson_kb_gf_Rank4","dGk_new")
    !
    if(all(K%is_zero()))then
       if(imsg)write(*,"(A)") " MSG dyson_kb_gf: K=0 using free_kb_gf"
       imsg=.false.
       call free_kb_gf(Hk,Gk,dGk,dGk_new)
       return
    endif
    !
    allocate(KxGk(0:max(N,L)))
    !
    allocate(Amat(Nso,Nso))
    allocate(Gkmat(Nso,Nso))
    allocate(GkxAmat(Nso,Nso))
    allocate(dGkless(Nso,Nso))
    Amat   =zero
    Gkmat  =zero
    GkxAmat=zero
    dGkless=zero
    !
    !Treat here the t=0,0 case:
    !We solve the matsubara part using FFT
    if(N==1)then
       !INITIALIZE THE WEISS FIELD Gk^{x=M,<,R,\lmix} for N=1
       !neq_setup_gf:
       do i=1,Niw
          Amat  =(xi*cc_params%wm(i)+xmu)*zeye(Nso) - Hk(:,:,1)
          do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
             io = iorb + (ispin-1)*Norb
             jo = jorb + (jspin-1)*Norb
             Gkmat(io,jo) = Amat(io,jo)  - K(ispin,jspin,iorb,jorb)%iw(i)
          end do
          call inv(Gkmat)
          do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
             io = iorb + (ispin-1)*Norb
             jo = jorb + (jspin-1)*Norb
             Gk(ispin,jspin,iorb,jorb)%iw(i) = Gkmat(io,jo)
          end do
       enddo
       !
       allocate(ftau(0:Niw))
       !FFT are not pure so can not use do concurrent here
       do ispin=1,Nspin
          do iorb=1,Norb
             do jspin=1,Nspin
                do jorb=1,Norb
                   call fft_iw2tau(Gk(ispin,jspin,iorb,jorb)%iw,ftau(0:),cc_params%beta)
                   call fft_extract_gtau(ftau(0:),Gk(ispin,jspin,iorb,jorb)%mats(0:))
                   Gk(ispin,jspin,iorb,jorb)%less(1,1) = -xi*Gk(ispin,jspin,iorb,jorb)%mats(L)
                   Gk(ispin,jspin,iorb,jorb)%lmix(1,0:L)=-xi*Gk(ispin,jspin,iorb,jorb)%mats(L:0:-1)
                enddo
             enddo
             Gk(ispin,ispin,iorb,iorb)%ret(1,1)  = -xi
          enddo
       enddo
       deallocate(ftau)
       !
       !neq_setup_dgf:
       do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          !get d/dt G_k^R = -i H(k,0)G_k^R
          HxGk=zero
          do kspin=1,Nspin
             do korb=1,Norb
                io = iorb + (ispin-1)*Norb
                ko = korb + (kspin-1)*Norb
                HxGk = HxGk - xi*Hk(io,ko,1)*Gk(kspin,jspin,korb,jorb)%ret(1,1)
             enddo
          enddo
          dGk_new(ispin,jspin,iorb,jorb)%ret(1) = HxGk
          !
          !get d/dt G_k^< = -i H(k,0)G_k^< -xi(-xi)int_0^beta S^\lmix*G_k^\rmix
          HxGk=zero
          do kspin=1,Nspin
             do korb=1,Norb
                io = iorb + (ispin-1)*Norb
                ko = korb + (kspin-1)*Norb
                HxGk = HxGk - xi*Hk(io,ko,1)*Gk(kspin,jspin,korb,jorb)%less(1,1)
             enddo
          enddo
          !
          KxGk(0:)=zero
          do s=0,L
             do kspin=1,Nspin
                do korb=1,Norb
                   KxGk(s)=KxGk(s)+K(ispin,kspin,iorb,korb)%lmix(1,s)*conjg( Gk(kspin,jspin,korb,jorb)%lmix(1,L-s) )
                enddo
             enddo
          enddo
          dGk_new(ispin,jspin,iorb,jorb)%less(1) = HxGk - xi*(-xi)*cc_params%dtau*kb_trapz(KxGk(0:),0,L)
          !
          !get d/dt G_k^\lmix = -xi*H(k,0)*G_k^\lmix - xi*int_0^beta G_k^\lmix*G_k^M
          KxGk(0:)=zero
          do kspin=1,Nspin
             do korb=1,Norb
                io = iorb + (ispin-1)*Norb
                ko = korb + (kspin-1)*Norb
                KxGk(0:) = KxGk(0:) - xi*Hk(io,ko,1)*Gk(kspin,jspin,korb,jorb)%lmix(1,0:)
             enddo
          enddo
          dGk_new(ispin,jspin,iorb,jorb)%lmix(0:)= KxGk(0:)
          do j=0,L
             KxGk(0:)=zero
             do s=0,j
                do kspin=1,Nspin
                   do korb=1,Norb
                      KxGk(s)=KxGk(s) + K(ispin,kspin,iorb,korb)%lmix(1,s)*Gk(kspin,jspin,korb,jorb)%mats(s+L-j)
                   enddo
                enddo
             end do
             dGk_new(ispin,jspin,iorb,jorb)%lmix(j)=dGk_new(ispin,jspin,iorb,jorb)%lmix(j)+xi*cc_params%dtau*kb_trapz(KxGk(0:),0,j)
             KxGk(0:)=zero
             do s=j,L
                do kspin=1,Nspin
                   do korb=1,Norb
                      KxGk(s)=KxGk(s)+K(ispin,kspin,iorb,korb)%lmix(1,s)*Gk(kspin,jspin,korb,jorb)%mats(s-j)
                   enddo
                enddo
             enddo
             dGk_new(ispin,jspin,iorb,jorb)%lmix(j)=dGk_new(ispin,jspin,iorb,jorb)%lmix(j)-xi*cc_params%dtau*kb_trapz(KxGk(0:),j,L)
          enddo
          !
       enddo
       !
       return
       !
    end if
    !
    !
    !TYPE: X=RET,LESS,LMIX (R,<,\LMIX)
    !I D/DT Gk^X(T,:)  = Hk(T)Gk^X(T,:) + Q^X(T,:) + INT_{0,T'}^{T}K^R(T,S)Gk^X(S,:)DS
    !
    !Ret component
    ! d/dt Gk^R(t,:) = -i*h(t)*Gk^R(t,:) -i*delta(t,:) - i\int_{:}^t K^R(t,s)*Gk^R(s,:)ds
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !TIP t_{N}, t`_{N}
    do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
       Gk(ispin,ispin,iorb,iorb)%ret(N,N)   = -xi
       dGk_new(ispin,jspin,iorb,jorb)%ret(N)= zero
       do kspin=1,Nspin
          do korb=1,Norb
             io = iorb + (ispin-1)*Norb
             ko = korb + (kspin-1)*Norb
             dGk_new(ispin,jspin,iorb,jorb)%ret(N)=dGk_new(ispin,jspin,iorb,jorb)%ret(N)-xi*Hk(io,ko,N)*Gk(kspin,jspin,korb,jorb)%ret(N,N)
          enddo
       enddo
    enddo
    !
    !VERTICAL INTERVAL t_{N}, t`_{j, j=1,...,N-1}
    tp_Ret:do j=1,N-1
       Amat = zeye(Nso)
       do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb
          jo = jorb + (jspin-1)*Norb
          !Add Gk^R(t_{N-1},t_j) + dt/2* d_tGk^ret(t_{N-1},j)
          Gkmat(io,jo) = Gk(ispin,jspin,iorb,jorb)%ret(N-1,j) + 0.5d0*dt*dGk(ispin,jspin,iorb,jorb)%ret(j)
          !Add -xi*K^R(t_N,s)*Gk^R(s,t_j)
          KxGk(0:)=zero
          do s=j,N-1
             do kspin=1,Nspin
                do korb=1,Norb
                   KxGk(s)=KxGk(s)+K(ispin,kspin,iorb,korb)%ret(N,s)*Gk(kspin,jspin,korb,jorb)%ret(s,j)
                enddo
             enddo
          enddo
          dGk_new(ispin,jspin,iorb,jorb)%ret(j) = -xi*dt*kb_half_trapz(KxGk(0:),j,N-1)
          Gkmat(io,jo) = Gkmat(io,jo) + 0.5d0*dt*dGk_new(ispin,jspin,iorb,jorb)%ret(j)
          Amat(io,jo)  = Amat(io,jo) + 0.5d0*xi*dt*Hk(io,jo,N) + 0.25d0*xi*dt**2*K(ispin,jspin,iorb,jorb)%ret(N,N)
       enddo
       call inv(Amat)
       GkxAmat = matmul(Gkmat,Amat)
       do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb
          jo = jorb + (jspin-1)*Norb
          Gk(ispin,jspin,iorb,jorb)%ret(N,j) = GkxAmat(io,jo)
       enddo
       !
       !
       !Update derivative d_t Gk^R(t,:) as:
       !d/dt Gk^R(t,:) = -i*h(t)*Gk^R(t,:) -i*delta(t,:) - i\int_{:}^t K^R(t,s)*Gk^R(s,:)ds
       do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          !
          do kspin=1,Nspin
             do korb=1,Norb
                io = iorb + (ispin-1)*Norb
                ko = korb + (kspin-1)*Norb
                dGk_new(ispin,jspin,iorb,jorb)%ret(j) = dGk_new(ispin,jspin,iorb,jorb)%ret(j) - xi*Hk(io,ko,N)*Gk(kspin,jspin,korb,jorb)%ret(N,j)
                dGk_new(ispin,jspin,iorb,jorb)%ret(j) = dGk_new(ispin,jspin,iorb,jorb)%ret(j) - 0.5d0*xi*dt*K(ispin,kspin,iorb,korb)%ret(N,N)*Gk(kspin,jspin,korb,jorb)%ret(N,j)
             enddo
          enddo
          !
       enddo
       !
    enddo tp_Ret
    !
    !
    !Lmix component
    !d/dt Gk^\lmix(t,:) = -i*Hk(t)*Gk^\lmix(t,:) -i\int_0^\beta K^\lmix(t,s)*Gk^M(s,:)ds -i\int_0^t K^R(t,s)Gk^\lmix(s,:)ds
    jtau_lmix: do jtau=0,L
       Amat = zeye(Nso)
       do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb
          jo = jorb + (jspin-1)*Norb
          !Add Gk^lmix(t_{N-1},tau_j) + dt/2* d_tGk^lmix(t_{N-1},tau_j)
          Gkmat(io,jo) = Gk(ispin,jspin,iorb,jorb)%lmix(N-1,jtau)+0.5d0*dt*dGk(ispin,jspin,iorb,jorb)%lmix(jtau)
          !Add Q^lmix(t_{N},tau_j) = K^lmix(N,stau)*Gk^mats(stau-jtau) stau<jtau
          KxGk(0:)=zero
          do s=0,jtau
             do kspin=1,Nspin
                do korb=1,Norb
                   KxGk(s)=KxGk(s)+K(ispin,kspin,iorb,korb)%lmix(N,s)*Gk(kspin,jspin,korb,jorb)%mats(s+L-jtau)
                enddo
             enddo
          enddo
          dGk_new(ispin,jspin,iorb,jorb)%lmix(jtau) = xi*dtau*kb_trapz(KxGk(0:),0,jtau)                 
          !
          !Add Q^lmix(t_{N},tau_j) = K^lmix(N,stau)*Gk^mats(stau-jtau) stau>jtau
          KxGk(0:)=zero
          do s=jtau,L
             do kspin=1,Nspin
                do korb=1,Norb
                   KxGk(s)=KxGk(s)+K(ispin,kspin,iorb,korb)%lmix(N,s)*Gk(kspin,jspin,korb,jorb)%mats(s-jtau)
                enddo
             enddo
          enddo
          dGk_new(ispin,jspin,iorb,jorb)%lmix(jtau)= dGk_new(ispin,jspin,iorb,jorb)%lmix(jtau)-xi*dtau*kb_trapz(KxGk(0:),jtau,L)
          !
          !Add -xi*K^R(t_N,s)*Gk^lmix(s,tau_j)
          KxGk(0:)=zero
          do s=1,N-1
             do kspin=1,Nspin
                do korb=1,Norb
                   KxGk(s)=KxGk(s)+K(ispin,kspin,iorb,korb)%ret(N,s)*Gk(kspin,jspin,korb,jorb)%lmix(s,jtau)
                enddo
             enddo
          enddo
          dGk_new(ispin,jspin,iorb,jorb)%lmix(jtau)=dGk_new(ispin,jspin,iorb,jorb)%lmix(jtau)-xi*dt*kb_half_trapz(KxGk(0:),1,N-1)
          Gkmat(io,jo) = Gkmat(io,jo) + 0.5d0*dt*dGk_new(ispin,jspin,iorb,jorb)%lmix(jtau)
          Amat(io,jo)  = Amat(io,jo)  + 0.5d0*xi*dt*Hk(io,jo,N) + 0.25d0*xi*dt**2*K(ispin,jspin,iorb,jorb)%ret(N,N)
       enddo
       call inv(Amat)
       GkxAmat = matmul(Gkmat,Amat)
       do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb
          jo = jorb + (jspin-1)*Norb
          Gk(ispin,jspin,iorb,jorb)%lmix(N,jtau) = GkxAmat(io,jo)
       enddo
       !
       !
       !Update derivative d_t Gk^\lmix(t,:) as: (dGkmat already holds all the terms not included below)
       !d/dt Gk^\lmix(t,:) = -i*Hk(t)*Gk^\lmix(t,:) -i\int_0^\beta K^\lmix(t,s)*Gk^M(s,:)ds -i\int_0^t K^R(t,s)Gk^\lmix(s,:)ds
       do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          !
          do kspin=1,Nspin
             do korb=1,Norb
                io = iorb + (ispin-1)*Norb
                ko = korb + (kspin-1)*Norb
                dGk_new(ispin,jspin,iorb,jorb)%lmix(jtau) = dGk_new(ispin,jspin,iorb,jorb)%lmix(jtau)- xi*Hk(io,ko,N)*Gk(kspin,jspin,korb,jorb)%lmix(N,jtau)
                dGk_new(ispin,jspin,iorb,jorb)%lmix(jtau) = dGk_new(ispin,jspin,iorb,jorb)%lmix(jtau)- 0.5d0*xi*dt*K(ispin,kspin,iorb,korb)%ret(N,N)*Gk(kspin,jspin,korb,jorb)%lmix(N,jtau)
             enddo
          enddo
          !
       enddo
       !
    enddo jtau_lmix
    !
    !
    !
    !
    !Less component
    !d/dt Gk^<(t,:) = -i*Hk(t)*Gk^<(t,:) -i*[ (-i)*\int_0^\beta K^\lmix(t,s)*Gk^\rmix(s,:)ds + \int_0^{:}K^<(t,s)*Gk^A(s,:)ds ]
    !                                 -i*\int_0^t K^R(t,s)*Gk^<(s,:)ds
    !
    ! Gk^<(t_{N},t_{j}), d/dt Gk^<(t_{N},t_{j}) <== lower-right triangle
    tp_less: do j=1,N-1
       Amat = zeye(Nso)
       do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb
          jo = jorb + (jspin-1)*Norb
          Gkmat(io,jo) = Gk(ispin,jspin,iorb,jorb)%less(N-1,j) + 0.5d0*dt*dGk(ispin,jspin,iorb,jorb)%less(j)
          KxGk(0:)=zero
          do s=0,L
             do kspin=1,Nspin
                do korb=1,Norb
                   KxGk(s)=KxGk(s)+K(ispin,kspin,iorb,korb)%lmix(N,s)*conjg( Gk(kspin,jspin,korb,jorb)%lmix(j,L-s) ) !rmix <-- lmix
                enddo
             enddo
          enddo
          dGk_new(ispin,jspin,iorb,jorb)%less(j) = -xi*(-xi)*dtau*kb_trapz(KxGk(0:),0,L)
          !
          KxGk(0:)=zero
          do s=1,j
             do kspin=1,Nspin
                do korb=1,Norb
                   KxGk(s)=KxGk(s)+K(ispin,kspin,iorb,korb)%less(N,s)*conjg( Gk(kspin,jspin,korb,jorb)%ret(j,s) ) !adv <-- ret
                enddo
             enddo
          enddo
          dGk_new(ispin,jspin,iorb,jorb)%less(j)=dGk_new(ispin,jspin,iorb,jorb)%less(j)-xi*dt*kb_trapz(KxGk(0:),1,j) !<= -iQ(t)
          !
          KxGk(0:)=zero
          do s=1,N-1
             do kspin=1,Nspin
                do korb=1,Norb
                   KxGk(s)=KxGk(s)+K(ispin,kspin,iorb,korb)%ret(N,s)*Gk(kspin,jspin,korb,jorb)%less(s,j)
                enddo
             enddo
          enddo
          dGk_new(ispin,jspin,iorb,jorb)%less(j)=dGk_new(ispin,jspin,iorb,jorb)%less(j)-xi*dt*kb_half_trapz(KxGk(0:),1,N-1)
          Gkmat(io,jo) = Gkmat(io,jo) + 0.5d0*dt*dGk_new(ispin,jspin,iorb,jorb)%less(j)
          Amat(io,jo)  = Amat(io,jo)  + 0.5d0*xi*dt*Hk(io,jo,N) + 0.25d0*xi*dt**2*K(ispin,jspin,iorb,jorb)%ret(N,N)
       enddo
       call inv(Amat)
       GkxAmat=matmul(Gkmat,Amat)
       do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb
          jo = jorb + (jspin-1)*Norb
          Gk(ispin,jspin,iorb,jorb)%less(N,j)= GkxAmat(io,jo)
       enddo
       !
       !Update derivative d_t Gk^<(t,:) as: (dGkmat already holds all the terms not included below)
       !d/dt Gk^<(t,:) = -i*Hk(t)*Gk^<(t,:) -i*[ (-i)*\int_0^\beta K^\lmix(t,s)*Gk^\rmix(s,:)ds + \int_0^{:}K^<(t,s)*Gk^A(s,:)ds ]
       !                                 -i*\int_0^t K^R(t,s)*Gk^<(s,:)ds
       do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          !
          do kspin=1,Nspin
             do korb=1,Norb
                io = iorb + (ispin-1)*Norb
                ko = korb + (kspin-1)*Norb
                dGk_new(ispin,jspin,iorb,jorb)%less(j) = dGk_new(ispin,jspin,iorb,jorb)%less(j) - xi*Hk(io,ko,N)*Gk(kspin,jspin,korb,jorb)%less(N,j)
                dGk_new(ispin,jspin,iorb,jorb)%less(j) = dGk_new(ispin,jspin,iorb,jorb)%less(j) - 0.5d0*xi*dt*K(ispin,kspin,iorb,korb)%ret(N,N)*Gk(kspin,jspin,korb,jorb)%less(N,j)
             enddo
          enddo
          !
       enddo
       !
    enddo tp_less
    !
    !
    ! Gk^<(t_{i},t_{N}), d/dt Gk^<(t_{i},t_{N}) <== upper left triangle
    ! Hermitian conjugate Gk
    do i=1,N-1
       do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          Gk(ispin,jspin,iorb,jorb)%less(i,N)=-conjg(Gk(jspin,ispin,jorb,iorb)%less(N,i))
       enddo
    enddo
    !
    !
    ! d/dt Gk^<(t_{N-1},t_{N})
    dGkless=zero
    do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
       !
       do kspin=1,Nspin
          do korb=1,Norb
             io = iorb + (ispin-1)*Norb
             ko = korb + (kspin-1)*Norb
             dGkless(io,jo) = dGkless(io,jo)-xi*Hk(io,ko,N-1)*Gk(kspin,jspin,korb,jorb)%less(N-1,N)
          enddo
       enddo
       !
       KxGk(0:)=zero
       do s=0,L
          do kspin=1,Nspin
             do korb=1,Norb
                KxGk(s)=KxGk(s)+K(ispin,kspin,iorb,korb)%lmix(N-1,s)*conjg( Gk(kspin,jspin,korb,jorb)%lmix(N,L-s) )!rmix <-- lmix
             enddo
          enddo
       enddo
       dGkless(io,jo) = dGkless(io,jo)-xi*(-xi)*dtau*kb_trapz(KxGk(0:),0,L)
       !
       KxGk(0:)=zero
       do s=1,N
          do kspin=1,Nspin
             do korb=1,Norb
                KxGk(s)=KxGk(s)+K(ispin,kspin,iorb,korb)%less(N-1,s)*conjg( Gk(kspin,jspin,korb,jorb)%ret(N,s) ) !adv <-- ret
             enddo
          enddo
       enddo
       dGkless(io,jo) = dGkless(io,jo)-xi*dt*kb_trapz(KxGk(0:),1,N)
       !
       KxGk(0:)=zero
       do s=1,N-1
          do kspin=1,Nspin
             do korb=1,Norb
                KxGk(s)=KxGk(s)+K(ispin,kspin,iorb,korb)%ret(N-1,s)*Gk(kspin,jspin,korb,jorb)%less(s,N)
             enddo
          enddo
       enddo
       dGkless(io,jo) = dGkless(io,jo)-xi*dt*kb_trapz(KxGk(0:),1,N-1)
    enddo
    !
    !
    !Gk^<(N,N), d/dt Gk^<(N,N)
    Amat = zeye(Nso)
    do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
       io = iorb + (ispin-1)*Norb
       jo = jorb + (jspin-1)*Norb
       Gkmat(io,jo) = Gk(ispin,jspin,iorb,jorb)%less(N-1,N)+0.5d0*dt*dGkless(io,jo)
       KxGk(0:)=zero
       do s=0,L
          do kspin=1,Nspin
             do korb=1,Norb
                KxGk(s)=KxGk(s)+K(ispin,kspin,iorb,korb)%lmix(N,s)*conjg( Gk(kspin,jspin,korb,jorb)%lmix(N,L-s) ) !get_rmix(Gk,s,N,L)
             enddo
          enddo
       enddo
       dGk_new(ispin,jspin,iorb,jorb)%less(N)=-xi*(-xi)*dtau*kb_trapz(KxGk(0:),0,L) !this one reset dGkmat
       !
       KxGk(0:)=zero
       do s=1,N
          do kspin=1,Nspin
             do korb=1,Norb
                KxGk(s)=KxGk(s)+K(ispin,kspin,iorb,korb)%less(N,s)*conjg( Gk(kspin,jspin,korb,jorb)%ret(N,s) ) !get_adv(Gk,s,N)
             enddo
          enddo
       enddo
       dGk_new(ispin,jspin,iorb,jorb)%less(N)=dGk_new(ispin,jspin,iorb,jorb)%less(N) - xi*dt*kb_trapz(KxGk(0:),1,N)
       !
       KxGk(0:)=zero
       do s=1,N-1
          do kspin=1,Nspin
             do korb=1,Norb
                KxGk(s)=KxGk(s)+K(ispin,kspin,iorb,korb)%ret(N,s)*Gk(kspin,jspin,korb,jorb)%less(s,N)
             enddo
          enddo
       enddo
       dGk_new(ispin,jspin,iorb,jorb)%less(N)=dGk_new(ispin,jspin,iorb,jorb)%less(N) - xi*dt*kb_half_trapz(KxGk(0:),1,N-1)
       Gkmat(io,jo) = Gkmat(io,jo) + 0.5d0*dt*dGk_new(ispin,jspin,iorb,jorb)%less(N)
       Amat(io,jo)  = Amat(io,jo)  + 0.5d0*xi*dt*Hk(io,jo,N) + 0.25d0*xi*dt**2*K(ispin,jspin,iorb,jorb)%ret(N,N)
    enddo
    !
    call inv(Amat)
    GkxAmat=matmul(Gkmat,Amat)
    do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
       io = iorb + (ispin-1)*Norb
       jo = jorb + (jspin-1)*Norb
       Gk(ispin,jspin,iorb,jorb)%less(N,N) = GkxAmat(io,jo)
    enddo
    !
    !
    !Update derivative d_t Gk^<(t,:) as: (dGkmat already holds all the terms not included below)
    !d/dt Gk^<(t,:) = -i*Hk(t)*Gk^<(t,:) -i*[ (-i)*\int_0^\beta K^\lmix(t,s)*Gk^\rmix(s,:)ds + \int_0^{:}K^<(t,s)*Gk^A(s,:)ds ]
    !                                 -i*\int_0^t K^R(t,s)*Gk^<(s,:)ds
    do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
       !
       do kspin=1,Nspin
          do korb=1,Norb
             io = iorb + (ispin-1)*Norb
             ko = korb + (kspin-1)*Norb
             dGk_new(ispin,jspin,iorb,jorb)%less(N) = dGk_new(ispin,jspin,iorb,jorb)%less(N) - xi*Hk(io,ko,N)*Gk(kspin,jspin,korb,jorb)%less(N,N)
             dGk_new(ispin,jspin,iorb,jorb)%less(N) = dGk_new(ispin,jspin,iorb,jorb)%less(N) - 0.5d0*xi*dt*K(ispin,kspin,iorb,korb)%ret(N,N)*Gk(kspin,jspin,korb,jorb)%less(N,N)
          enddo
       enddo
       !
    enddo
    !
    !
    deallocate(KxGk)
    deallocate(Amat,Gkmat,GkxAmat,dGkless)
    !
  end subroutine dyson_kb_gf_rank4



  subroutine dyson_kb_gf_rank6(Hk,K,Gk,dGk,dGk_new)
    complex(8),dimension(:,:,:)           :: Hk              ![Nlso][Nlso][Nt]
    type(kb_gf),intent(in)                :: K(:,:,:,:,:,:)  ![Nlat][Nlat][Nspin][Nspin][Norb][Norb]
    type(kb_gf),intent(inout)             :: Gk(:,:,:,:,:,:)     !as K
    type(kb_dgf),intent(inout)            :: dGk(:,:,:,:,:,:)    !as K
    type(kb_dgf)                          :: dGk_new(:,:,:,:,:,:)!as K
    integer                               :: N,L,Nlso,Niw
    real(8)                               :: dt,dtau
    integer                               :: i,j,itau,jtau,s
    integer                               :: io,jo,ko
    complex(8)                            :: HxGk
    complex(8),dimension(:),allocatable   :: KxGk
    real(8),dimension(:),allocatable      :: ftau
    complex(8),dimension(:,:),allocatable :: Amat,GkxAmat,Gkmat,dGkless
    !
    N   = cc_params%Nt                 !<== work with the ACTUAL size of the contour
    L   = cc_params%Ntau
    Niw = cc_params%Niw
    dt  = cc_params%dt
    dtau= cc_params%dtau
    Nlat = size(Gk,1) ; Nspin = size(Gk,3) ; Norb = size(Gk,5) ; Nso=Nlat*Nspin*Norb
    ! call assert_shape(Hk,[Nlso,Nlso,cc_params%Ntime],"dyson_kb_gf_rank0","Hk")
    ! call assert_shape_kb_gf(K,[Nlat,Nlat,Nspin,Nspin,Norb,Norb],"dyson_kb_gf_Rank4","K")
    ! call assert_shape_kb_gf(Gk,[Nlat,Nlat,Nspin,Nspin,Norb,Norb],"dyson_kb_gf_Rank4","Gk")
    ! call assert_shape_kb_gf(dGk,[Nlat,Nlat,Nspin,Nspin,Norb,Norb],"dyson_kb_gf_Rank4","dGk")
    ! call assert_shape_kb_gf(dGk_new,[Nlat,Nlat,Nspin,Nspin,Norb,Norb],"dyson_kb_gf_Rank4","dGk_new")
    !
    if(all(K%is_zero()))then
       if(imsg)write(*,"(A)") " MSG dyson_kb_gf: K=0 using free_kb_gf"
       imsg=.false.
       call free_kb_gf(Hk,Gk,dGk,dGk_new)
       return
    endif
    !
    allocate(KxGk(0:max(N,L)))
    !
    allocate(Amat(Nlso,Nlso))
    allocate(Gkmat(Nlso,Nlso))
    allocate(GkxAmat(Nlso,Nlso))
    allocate(dGkless(Nlso,Nlso))
    Amat   =zero
    Gkmat  =zero
    GkxAmat=zero
    dGkless=zero
    !
    !Treat here the t=0,0 case:
    !We solve the matsubara part using FFT
    if(N==1)then
       !INITIALIZE THE WEISS FIELD Gk^{x=M,<,R,\lmix} for N=1
       !neq_setup_gf:
       do i=1,Niw
          Amat  =(xi*cc_params%wm(i)+xmu)*zeye(Nlso) - Hk(:,:,1)
          do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
             io = iorb + (ispin-1)*Norb + (ilat-1)*Nspin*Norb
             jo = jorb + (jspin-1)*Norb + (jlat-1)*Nspin*Norb
             Gkmat(io,jo) = Amat(io,jo)  - K(ilat,jlat,ispin,jspin,iorb,jorb)%iw(i)
          end do
          call inv(Gkmat)
          do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
             io = iorb + (ispin-1)*Norb + (ilat-1)*Nspin*Norb
             jo = jorb + (jspin-1)*Norb + (jlat-1)*Nspin*Norb
             Gk(ilat,jlat,ispin,jspin,iorb,jorb)%iw(i) = Gkmat(io,jo)
          end do
       enddo
       !
       allocate(ftau(0:Niw))
       !FFT are not pure so can not use do concurrent here
       do ilat=1,Nlat
          do ispin=1,Nspin
             do iorb=1,Norb
                do jlat=1,Nlat
                   do jspin=1,Nspin
                      do jorb=1,Norb
                         call fft_iw2tau(Gk(ilat,jlat,ispin,jspin,iorb,jorb)%iw,ftau(0:),cc_params%beta)
                         call fft_extract_gtau(ftau(0:),Gk(ilat,jlat,ispin,jspin,iorb,jorb)%mats(0:))
                         Gk(ilat,jlat,ispin,jspin,iorb,jorb)%less(1,1) = -xi*Gk(ilat,jlat,ispin,jspin,iorb,jorb)%mats(L)
                         Gk(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(1,0:L)=-xi*Gk(ilat,jlat,ispin,jspin,iorb,jorb)%mats(L:0:-1)
                      enddo
                   enddo
                enddo
                Gk(ilat,ilat,ispin,ispin,iorb,iorb)%ret(1,1)  = -xi
             enddo
          enddo
       enddo
       deallocate(ftau)
       !
       !neq_setup_dgf:
       do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          !get d/dt G_k^R = -i H(k,0)G_k^R
          HxGk=zero
          do klat=1,Nlat
             do kspin=1,Nspin
                do korb=1,Norb
                   io = iorb + (ispin-1)*Norb
                   ko = korb + (kspin-1)*Norb
                   HxGk = HxGk - xi*Hk(io,ko,1)*Gk(klat,jlat,kspin,jspin,korb,jorb)%ret(1,1)
                enddo
             enddo
          enddo
          dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%ret(1) = HxGk
          !
          !get d/dt G_k^< = -i H(k,0)G_k^< -xi(-xi)int_0^beta S^\lmix*G_k^\rmix
          HxGk=zero
          do klat=1,Nlat
             do kspin=1,Nspin
                do korb=1,Norb
                   io = iorb + (ispin-1)*Norb
                   ko = korb + (kspin-1)*Norb
                   HxGk = HxGk - xi*Hk(io,ko,1)*Gk(klat,jlat,kspin,jspin,korb,jorb)%less(1,1)
                enddo
             enddo
          enddo
          !
          KxGk(0:)=zero
          do s=0,L
             do klat=1,Nlat
                do kspin=1,Nspin
                   do korb=1,Norb
                      KxGk(s)=KxGk(s)+K(ilat,klat,ispin,kspin,iorb,korb)%lmix(1,s)*conjg( Gk(klat,jlat,kspin,jspin,korb,jorb)%lmix(1,L-s) )
                   enddo
                enddo
             enddo
          enddo
          dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%less(1) = HxGk - xi*(-xi)*cc_params%dtau*kb_trapz(KxGk(0:),0,L)
          !
          !get d/dt G_k^\lmix = -xi*H(k,0)*G_k^\lmix - xi*int_0^beta G_k^\lmix*G_k^M
          KxGk(0:)=zero
          do klat=1,Nlat
             do kspin=1,Nspin
                do korb=1,Norb
                   io = iorb + (ispin-1)*Norb
                   ko = korb + (kspin-1)*Norb
                   KxGk(0:) = KxGk(0:) - xi*Hk(io,ko,1)*Gk(klat,jlat,kspin,jspin,korb,jorb)%lmix(1,0:)
                enddo
             enddo
          enddo
          dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(0:)= KxGk(0:)
          do j=0,L
             KxGk(0:)=zero
             do s=0,j
                do klat=1,Nlat
                   do kspin=1,Nspin
                      do korb=1,Norb
                         KxGk(s)=KxGk(s) + K(ilat,klat,ispin,kspin,iorb,korb)%lmix(1,s)*Gk(klat,jlat,kspin,jspin,korb,jorb)%mats(s+L-j)
                      enddo
                   enddo
                enddo
             end do
             dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(j)=dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(j)+xi*cc_params%dtau*kb_trapz(KxGk(0:),0,j)
             KxGk(0:)=zero
             do s=j,L
                do klat=1,Nlat
                   do kspin=1,Nspin
                      do korb=1,Norb
                         KxGk(s)=KxGk(s)+K(ilat,klat,ispin,kspin,iorb,korb)%lmix(1,s)*Gk(klat,jlat,kspin,jspin,korb,jorb)%mats(s-j)
                      enddo
                   enddo
                enddo
             enddo
             dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(j)=dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(j)-xi*cc_params%dtau*kb_trapz(KxGk(0:),j,L)
          enddo
          !
       enddo
       !
       return
       !
    end if
    !
    !
    !TYPE: X=RET,LESS,LMIX (R,<,\LMIX)
    !I D/DT Gk^X(T,:)  = Hk(T)Gk^X(T,:) + Q^X(T,:) + INT_{0,T'}^{T}K^R(T,S)Gk^X(S,:)DS
    !
    !Ret component
    ! d/dt Gk^R(t,:) = -i*h(t)*Gk^R(t,:) -i*delta(t,:) - i\int_{:}^t K^R(t,s)*Gk^R(s,:)ds
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !TIP t_{N}, t`_{N}
    do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
       Gk(ilat,ilat,ispin,ispin,iorb,iorb)%ret(N,N)   = -xi
       dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%ret(N)= zero
       do klat=1,Nlat
          do kspin=1,Nspin
             do korb=1,Norb
                io = iorb + (ispin-1)*Norb + (ilat-1)*Nspin*Norb
                ko = korb + (kspin-1)*Norb + (klat-1)*Nspin*Norb
                dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%ret(N)=dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%ret(N)-xi*Hk(io,ko,N)*Gk(klat,jlat,kspin,jspin,korb,jorb)%ret(N,N)
             enddo
          enddo
       enddo
    enddo
    !
    !VERTICAL INTERVAL t_{N}, t`_{j, j=1,...,N-1}
    tp_Ret:do j=1,N-1
       Amat = zeye(Nlso)
       do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb
          jo = jorb + (jspin-1)*Norb
          !Add Gk^R(t_{N-1},t_j) + dt/2* d_tGk^ret(t_{N-1},j)
          Gkmat(io,jo) = Gk(ilat,jlat,ispin,jspin,iorb,jorb)%ret(N-1,j) + 0.5d0*dt*dGk(ilat,jlat,ispin,jspin,iorb,jorb)%ret(j)
          !Add -xi*K^R(t_N,s)*Gk^R(s,t_j)
          KxGk(0:)=zero
          do s=j,N-1
             do klat=1,Nlat
                do kspin=1,Nspin
                   do korb=1,Norb
                      KxGk(s)=KxGk(s)+K(ilat,klat,ispin,kspin,iorb,korb)%ret(N,s)*Gk(klat,jlat,kspin,jspin,korb,jorb)%ret(s,j)
                   enddo
                enddo
             enddo
          enddo
          dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%ret(j) = -xi*dt*kb_half_trapz(KxGk(0:),j,N-1)
          Gkmat(io,jo) = Gkmat(io,jo) + 0.5d0*dt*dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%ret(j)
          Amat(io,jo)  = Amat(io,jo) + 0.5d0*xi*dt*Hk(io,jo,N) + 0.25d0*xi*dt**2*K(ilat,jlat,ispin,jspin,iorb,jorb)%ret(N,N)
       enddo
       call inv(Amat)
       GkxAmat = matmul(Gkmat,Amat)
       do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb
          jo = jorb + (jspin-1)*Norb
          Gk(ilat,jlat,ispin,jspin,iorb,jorb)%ret(N,j) = GkxAmat(io,jo)
       enddo
       !
       !
       !Update derivative d_t Gk^R(t,:) as:
       !d/dt Gk^R(t,:) = -i*h(t)*Gk^R(t,:) -i*delta(t,:) - i\int_{:}^t K^R(t,s)*Gk^R(s,:)ds
       do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          !
          do klat=1,Nlat
             do kspin=1,Nspin
                do korb=1,Norb
                   io = iorb + (ispin-1)*Norb
                   ko = korb + (kspin-1)*Norb
                   dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%ret(j) = dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%ret(j) - xi*Hk(io,ko,N)*Gk(klat,jlat,kspin,jspin,korb,jorb)%ret(N,j)
                   dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%ret(j) = dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%ret(j) - 0.5d0*xi*dt*K(ilat,klat,ispin,kspin,iorb,korb)%ret(N,N)*Gk(klat,jlat,kspin,jspin,korb,jorb)%ret(N,j)
                enddo
             enddo
          enddo
          !
       enddo
       !
    enddo tp_Ret
    !
    !
    !Lmix component
    !d/dt Gk^\lmix(t,:) = -i*Hk(t)*Gk^\lmix(t,:) -i\int_0^\beta K^\lmix(t,s)*Gk^M(s,:)ds -i\int_0^t K^R(t,s)Gk^\lmix(s,:)ds
    jtau_lmix: do jtau=0,L
       Amat = zeye(Nlso)
       do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb
          jo = jorb + (jspin-1)*Norb
          !Add Gk^lmix(t_{N-1},tau_j) + dt/2* d_tGk^lmix(t_{N-1},tau_j)
          Gkmat(io,jo) = Gk(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(N-1,jtau)+0.5d0*dt*dGk(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(jtau)
          !Add Q^lmix(t_{N},tau_j) = K^lmix(N,stau)*Gk^mats(stau-jtau) stau<jtau
          KxGk(0:)=zero
          do s=0,jtau
             do klat=1,Nlat
                do kspin=1,Nspin
                   do korb=1,Norb
                      KxGk(s)=KxGk(s)+K(ilat,klat,ispin,kspin,iorb,korb)%lmix(N,s)*Gk(klat,jlat,kspin,jspin,korb,jorb)%mats(s+L-jtau)
                   enddo
                enddo
             enddo
          enddo
          dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(jtau) = xi*dtau*kb_trapz(KxGk(0:),0,jtau)                 
          !
          !Add Q^lmix(t_{N},tau_j) = K^lmix(N,stau)*Gk^mats(stau-jtau) stau>jtau
          KxGk(0:)=zero
          do s=jtau,L
             do klat=1,Nlat
                do kspin=1,Nspin
                   do korb=1,Norb
                      KxGk(s)=KxGk(s)+K(ilat,klat,ispin,kspin,iorb,korb)%lmix(N,s)*Gk(klat,jlat,kspin,jspin,korb,jorb)%mats(s-jtau)
                   enddo
                enddo
             enddo
          enddo
          dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(jtau)= dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(jtau)-xi*dtau*kb_trapz(KxGk(0:),jtau,L)
          !
          !Add -xi*K^R(t_N,s)*Gk^lmix(s,tau_j)
          KxGk(0:)=zero
          do s=1,N-1
             do klat=1,Nlat
                do kspin=1,Nspin
                   do korb=1,Norb
                      KxGk(s)=KxGk(s)+K(ilat,klat,ispin,kspin,iorb,korb)%ret(N,s)*Gk(klat,jlat,kspin,jspin,korb,jorb)%lmix(s,jtau)
                   enddo
                enddo
             enddo
          enddo
          dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(jtau)=dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(jtau)-xi*dt*kb_half_trapz(KxGk(0:),1,N-1)
          Gkmat(io,jo) = Gkmat(io,jo) + 0.5d0*dt*dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(jtau)
          Amat(io,jo)  = Amat(io,jo)  + 0.5d0*xi*dt*Hk(io,jo,N) + 0.25d0*xi*dt**2*K(ilat,jlat,ispin,jspin,iorb,jorb)%ret(N,N)
       enddo
       call inv(Amat)
       GkxAmat = matmul(Gkmat,Amat)
       do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb
          jo = jorb + (jspin-1)*Norb
          Gk(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(N,jtau) = GkxAmat(io,jo)
       enddo
       !
       !
       !Update derivative d_t Gk^\lmix(t,:) as: (dGkmat already holds all the terms not included below)
       !d/dt Gk^\lmix(t,:) = -i*Hk(t)*Gk^\lmix(t,:) -i\int_0^\beta K^\lmix(t,s)*Gk^M(s,:)ds -i\int_0^t K^R(t,s)Gk^\lmix(s,:)ds
       do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          !
          do klat=1,Nlat
             do kspin=1,Nspin
                do korb=1,Norb
                   io = iorb + (ispin-1)*Norb
                   ko = korb + (kspin-1)*Norb
                   dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(jtau) = dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(jtau)- xi*Hk(io,ko,N)*Gk(klat,jlat,kspin,jspin,korb,jorb)%lmix(N,jtau)
                   dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(jtau) = dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(jtau)- 0.5d0*xi*dt*K(ilat,klat,ispin,kspin,iorb,korb)%ret(N,N)*Gk(klat,jlat,kspin,jspin,korb,jorb)%lmix(N,jtau)
                enddo
             enddo
          enddo
          !
       enddo
       !
    enddo jtau_lmix
    !
    !
    !
    !
    !Less component
    !d/dt Gk^<(t,:) = -i*Hk(t)*Gk^<(t,:) -i*[ (-i)*\int_0^\beta K^\lmix(t,s)*Gk^\rmix(s,:)ds + \int_0^{:}K^<(t,s)*Gk^A(s,:)ds ]
    !                                 -i*\int_0^t K^R(t,s)*Gk^<(s,:)ds
    !
    ! Gk^<(t_{N},t_{j}), d/dt Gk^<(t_{N},t_{j}) <== lower-right triangle
    tp_less: do j=1,N-1
       Amat = zeye(Nlso)
       do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb
          jo = jorb + (jspin-1)*Norb
          Gkmat(io,jo) = Gk(ilat,jlat,ispin,jspin,iorb,jorb)%less(N-1,j) + 0.5d0*dt*dGk(ilat,jlat,ispin,jspin,iorb,jorb)%less(j)
          KxGk(0:)=zero
          do s=0,L
             do klat=1,Nlat
                do kspin=1,Nspin
                   do korb=1,Norb
                      KxGk(s)=KxGk(s)+K(ilat,klat,ispin,kspin,iorb,korb)%lmix(N,s)*conjg( Gk(klat,jlat,kspin,jspin,korb,jorb)%lmix(j,L-s) ) !rmix <-- lmix
                   enddo
                enddo
             enddo
          enddo
          dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%less(j) = -xi*(-xi)*dtau*kb_trapz(KxGk(0:),0,L)
          !
          KxGk(0:)=zero
          do s=1,j
             do klat=1,Nlat
                do kspin=1,Nspin
                   do korb=1,Norb
                      KxGk(s)=KxGk(s)+K(ilat,klat,ispin,kspin,iorb,korb)%less(N,s)*conjg( Gk(klat,jlat,kspin,jspin,korb,jorb)%ret(j,s) ) !adv <-- ret
                   enddo
                enddo
             enddo
          enddo
          dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%less(j)=dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%less(j)-xi*dt*kb_trapz(KxGk(0:),1,j) !<= -iQ(t)
          !
          KxGk(0:)=zero
          do s=1,N-1
             do klat=1,Nlat
                do kspin=1,Nspin
                   do korb=1,Norb
                      KxGk(s)=KxGk(s)+K(ilat,klat,ispin,kspin,iorb,korb)%ret(N,s)*Gk(klat,jlat,kspin,jspin,korb,jorb)%less(s,j)
                   enddo
                enddo
             enddo
          enddo
          dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%less(j)=dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%less(j)-xi*dt*kb_half_trapz(KxGk(0:),1,N-1)
          Gkmat(io,jo) = Gkmat(io,jo) + 0.5d0*dt*dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%less(j)
          Amat(io,jo)  = Amat(io,jo)  + 0.5d0*xi*dt*Hk(io,jo,N) + 0.25d0*xi*dt**2*K(ilat,jlat,ispin,jspin,iorb,jorb)%ret(N,N)
       enddo
       call inv(Amat)
       GkxAmat=matmul(Gkmat,Amat)
       do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb
          jo = jorb + (jspin-1)*Norb
          Gk(ilat,jlat,ispin,jspin,iorb,jorb)%less(N,j)= GkxAmat(io,jo)
       enddo
       !
       !Update derivative d_t Gk^<(t,:) as: (dGkmat already holds all the terms not included below)
       !d/dt Gk^<(t,:) = -i*Hk(t)*Gk^<(t,:) -i*[ (-i)*\int_0^\beta K^\lmix(t,s)*Gk^\rmix(s,:)ds + \int_0^{:}K^<(t,s)*Gk^A(s,:)ds ]
       !                                 -i*\int_0^t K^R(t,s)*Gk^<(s,:)ds
       do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          !
          do klat=1,Nlat
             do kspin=1,Nspin
                do korb=1,Norb
                   io = iorb + (ispin-1)*Norb
                   ko = korb + (kspin-1)*Norb
                   dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%less(j) = dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%less(j) - xi*Hk(io,ko,N)*Gk(klat,jlat,kspin,jspin,korb,jorb)%less(N,j)
                   dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%less(j) = dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%less(j) - 0.5d0*xi*dt*K(ilat,klat,ispin,kspin,iorb,korb)%ret(N,N)*Gk(klat,jlat,kspin,jspin,korb,jorb)%less(N,j)
                enddo
             enddo
          enddo
          !
       enddo
       !
    enddo tp_less
    !
    !
    ! Gk^<(t_{i},t_{N}), d/dt Gk^<(t_{i},t_{N}) <== upper left triangle
    ! Hermitian conjugate Gk
    do i=1,N-1
       do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          Gk(ilat,jlat,ispin,jspin,iorb,jorb)%less(i,N)=-conjg(Gk(jlat,ilat,jspin,ispin,jorb,iorb)%less(N,i))
       enddo
    enddo
    !
    !
    ! d/dt Gk^<(t_{N-1},t_{N})
    dGkless=zero
    do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
       !
       do klat=1,Nlat
          do kspin=1,Nspin
             do korb=1,Norb
                io = iorb + (ispin-1)*Norb + (ilat-1)*Nspin*Norb
                ko = korb + (kspin-1)*Norb + (klat-1)*Nspin*Norb
                dGkless(io,jo) = dGkless(io,jo)-xi*Hk(io,ko,N-1)*Gk(klat,jlat,kspin,jspin,korb,jorb)%less(N-1,N)
             enddo
          enddo
       enddo
       !
       KxGk(0:)=zero
       do s=0,L
          do klat=1,Nlat
             do kspin=1,Nspin
                do korb=1,Norb
                   KxGk(s)=KxGk(s)+K(ilat,klat,ispin,kspin,iorb,korb)%lmix(N-1,s)*conjg( Gk(klat,jlat,kspin,jspin,korb,jorb)%lmix(N,L-s) )!rmix <-- lmix
                enddo
             enddo
          enddo
       enddo
       dGkless(io,jo) = dGkless(io,jo)-xi*(-xi)*dtau*kb_trapz(KxGk(0:),0,L)
       !
       KxGk(0:)=zero
       do s=1,N
          do klat=1,Nlat
             do kspin=1,Nspin
                do korb=1,Norb
                   KxGk(s)=KxGk(s)+K(ilat,klat,ispin,kspin,iorb,korb)%less(N-1,s)*conjg( Gk(klat,jlat,kspin,jspin,korb,jorb)%ret(N,s) ) !adv <-- ret
                enddo
             enddo
          enddo
       enddo
       dGkless(io,jo) = dGkless(io,jo)-xi*dt*kb_trapz(KxGk(0:),1,N)
       !
       KxGk(0:)=zero
       do s=1,N-1
          do klat=1,Nlat
             do kspin=1,Nspin
                do korb=1,Norb
                   KxGk(s)=KxGk(s)+K(ilat,klat,ispin,kspin,iorb,korb)%ret(N-1,s)*Gk(klat,jlat,kspin,jspin,korb,jorb)%less(s,N)
                enddo
             enddo
          enddo
       enddo
       dGkless(io,jo) = dGkless(io,jo)-xi*dt*kb_trapz(KxGk(0:),1,N-1)
    enddo
    !
    !
    !Gk^<(N,N), d/dt Gk^<(N,N)
    Amat = zeye(Nlso)
    do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
       io = iorb + (ispin-1)*Norb
       jo = jorb + (jspin-1)*Norb
       Gkmat(io,jo) = Gk(ilat,jlat,ispin,jspin,iorb,jorb)%less(N-1,N)+0.5d0*dt*dGkless(io,jo)
       KxGk(0:)=zero
       do s=0,L
          do klat=1,Nlat
             do kspin=1,Nspin
                do korb=1,Norb
                   KxGk(s)=KxGk(s)+K(ilat,klat,ispin,kspin,iorb,korb)%lmix(N,s)*conjg( Gk(klat,jlat,kspin,jspin,korb,jorb)%lmix(N,L-s) ) !get_rmix(Gk,s,N,L)
                enddo
             enddo
          enddo
       enddo
       dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%less(N)=-xi*(-xi)*dtau*kb_trapz(KxGk(0:),0,L) !this one reset dGkmat
       !
       KxGk(0:)=zero
       do s=1,N
          do klat=1,Nlat
             do kspin=1,Nspin
                do korb=1,Norb
                   KxGk(s)=KxGk(s)+K(ilat,klat,ispin,kspin,iorb,korb)%less(N,s)*conjg( Gk(klat,jlat,kspin,jspin,korb,jorb)%ret(N,s) ) !get_adv(Gk,s,N)
                enddo
             enddo
          enddo
       enddo
       dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%less(N)=dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%less(N) - xi*dt*kb_trapz(KxGk(0:),1,N)
       !
       KxGk(0:)=zero
       do s=1,N-1
          do klat=1,Nlat
             do kspin=1,Nspin
                do korb=1,Norb
                   KxGk(s)=KxGk(s)+K(ilat,klat,ispin,kspin,iorb,korb)%ret(N,s)*Gk(klat,jlat,kspin,jspin,korb,jorb)%less(s,N)
                enddo
             enddo
          enddo
       enddo
       dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%less(N)=dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%less(N) - xi*dt*kb_half_trapz(KxGk(0:),1,N-1)
       Gkmat(io,jo) = Gkmat(io,jo) + 0.5d0*dt*dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%less(N)
       Amat(io,jo)  = Amat(io,jo)  + 0.5d0*xi*dt*Hk(io,jo,N) + 0.25d0*xi*dt**2*K(ilat,jlat,ispin,jspin,iorb,jorb)%ret(N,N)
    enddo
    !
    call inv(Amat)
    GkxAmat=matmul(Gkmat,Amat)
    do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
       io = iorb + (ispin-1)*Norb
       jo = jorb + (jspin-1)*Norb
       Gk(ilat,jlat,ispin,jspin,iorb,jorb)%less(N,N) = GkxAmat(io,jo)
    enddo
    !
    !
    !Update derivative d_t Gk^<(t,:) as: (dGkmat already holds all the terms not included below)
    !d/dt Gk^<(t,:) = -i*Hk(t)*Gk^<(t,:) -i*[ (-i)*\int_0^\beta K^\lmix(t,s)*Gk^\rmix(s,:)ds + \int_0^{:}K^<(t,s)*Gk^A(s,:)ds ]
    !                                 -i*\int_0^t K^R(t,s)*Gk^<(s,:)ds
    do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
       !
       do klat=1,Nlat
          do kspin=1,Nspin
             do korb=1,Norb
                io = iorb + (ispin-1)*Norb + (ilat-1)*Nspin*Norb
                ko = korb + (kspin-1)*Norb + (klat-1)*Nspin*Norb
                dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%less(N) = dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%less(N) - xi*Hk(io,ko,N)*Gk(klat,jlat,kspin,jspin,korb,jorb)%less(N,N)
                dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%less(N) = dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%less(N) - 0.5d0*xi*dt*K(ilat,klat,ispin,kspin,iorb,korb)%ret(N,N)*Gk(klat,jlat,kspin,jspin,korb,jorb)%less(N,N)
             enddo
          enddo
       enddo
       !
    enddo
    !
    !
    deallocate(KxGk)
    deallocate(Amat,Gkmat,GkxAmat,dGkless)
    !
  end subroutine dyson_kb_gf_rank6




  !##################################################################
  ! The procedures are divided in two sets:
  ! Even Ranks: 0,2,4,6
  ! and 
  ! Odd Rank: 1,3,5,7
  ! in this case the last dimension is used as a diagonal index
  ! for the remaining D-1 components following the Even Ranks.
  ! e.g. G(:,:,L) --> L times Rank2 with G(:,:,l)
  ! NOTE THAT THIS SCHEME IS ARBITRARY AND USER SHOULD BE AWARE OF THAT.
  ! THE REASON FOR THIS CHOICE IS THAT WE EMPLOY MATRICES OF THE TYPE
  !  G(NLAT,NLAT,NSPIN,NSPIN,NORB,NORB) ---> GP(NLAT*NSPIN*NORB,NLAT*NSPIN*NORB)
  ! [THE OTHER ORDERING (i1,i2,i3,j1,j2,j3) --> (i,j) WOULD REQUIRE TO HAVE
  !  FUNCTIONS OF THE TYPE F(NLAT,NSPIN,NORB,NLAT,NSPIN,NORB)]
  !##################################################################

  subroutine dyson_kb_gf_d1(H,K,G,dG,dG_new)
    complex(8),dimension(:,:)  :: H       ![Nk][Nt]
    type(kb_gf),intent(in)     :: K(:)     ![Nk]
    type(kb_gf),intent(inout)  :: G(:)     !as K
    type(kb_dgf),intent(inout) :: dG(:)    !as K
    type(kb_dgf)               :: dG_new(:)!as K
    Nk = size(H,1)
    call assert_shape(H,[Nk,cc_params%Ntime],"dyson_kb_gf_d1","H")
    call assert_shape_kb_gf(K,[Nk],"dyson_kb_gf_d1","K")
    call assert_shape_kb_gf(G,[Nk],"dyson_kb_gf_d1","G")
    call assert_shape_kb_gf(dG,[Nk],"dyson_kb_gf_d1","dG")
    call assert_shape_kb_gf(dG_new,[Nk],"dyson_kb_gf_d1","dG_new")
    !
    do ik=1,Nk
       call dyson_kb_gf_rank0(H(ik,:),K(ik),G(ik),dG(ik),dG_new(ik))
    enddo
    !
  end subroutine dyson_kb_gf_d1


  subroutine dyson_kb_gf_d1_(H,K,G,dG,dG_new)
    complex(8),dimension(:,:)  :: H        ![Nk][Nt]
    type(kb_gf),intent(in)     :: K        !
    type(kb_gf),intent(inout)  :: G(:)     !
    type(kb_dgf),intent(inout) :: dG(:)    !as G
    type(kb_dgf)               :: dG_new(:)!as G
    integer                    :: ik,Nk  
    Nk = size(H,1)
    !
    call assert_shape(H,[Nk,cc_params%Ntime],"dyson_kb_gf_d1","H")
    call assert_shape_kb_gf(G,[Nk],"dyson_kb_gf_d1","G")
    call assert_shape_kb_gf(dG,[Nk],"dyson_kb_gf_d1","dG")
    call assert_shape_kb_gf(dG_new,[Nk],"dyson_kb_gf_d1","dG_new")
    !
    do ik=1,Nk
       call dyson_kb_gf_rank0(H(ik,:),K,G(ik),dG(ik),dG_new(ik))
    enddo
    !
  end subroutine dyson_kb_gf_d1_


  subroutine dyson_kb_gf_d2(H,K,G,dG,dG_new)
    complex(8),dimension(:,:,:) :: H       ![Nso][Nso][Nt]
    type(kb_gf),intent(in)      :: K(:,:)  ![Nso][Nso]
    type(kb_gf),intent(inout)   :: G(:,:)     !as K
    type(kb_dgf),intent(inout)  :: dG(:,:)    !as K
    type(kb_dgf)                :: dG_new(:,:)!as K
    integer                     :: Nso
    Nso = size(H,1)
    !
    call assert_shape(H,[Nso,Nso,cc_params%Ntime],"dyson_kb_gf_d2","H")
    call assert_shape_kb_gf(K,[Nso,Nso],"dyson_kb_gf_d2","K")
    call assert_shape_kb_gf(G,[Nso,Nso],"dyson_kb_gf_d2","G")
    call assert_shape_kb_gf(dG,[Nso,Nso],"dyson_kb_gf_d2","dG")
    call assert_shape_kb_gf(dG_new,[Nso,Nso],"dyson_kb_gf_d2","dG_new")
    !
    select case(Nso)
    case(1)
       call dyson_kb_gf_rank0(H(1,1,:),K(1,1),G(1,1),dG(1,1),dG_new(1,1))
    case default
       call dyson_kb_gf_rank2(H,K,G,dG,dG_new)
    end select
    !
  end subroutine dyson_kb_gf_d2


  !K  [Nlat][Nso][Nso]
  !Gk [Nlso][Nlso][Nk]
  subroutine dyson_kb_gf_d3(H,K,G,dG,dG_new)
    complex(8),dimension(:,:,:,:) :: H             ![Nso][Nso][Nk][Nt]
    type(kb_gf),intent(in)        :: K(:,:,:)      ![Nso][Nso][Nk]
    type(kb_gf),intent(inout)     :: G(:,:,:)      !as K
    type(kb_dgf),intent(inout)    :: dG(:,:,:)     !as K
    type(kb_dgf)                  :: dG_new(:,:,:) !as K
    integer                       :: Nk,Nso,ik
    Nso = size(H,1)
    Nk  = size(H,3)
    !
    call assert_shape(H,[Nso,Nso,Nk,cc_params%Ntime],"dyson_kb_gf_d3","H")
    call assert_shape_kb_gf(K,[Nso,Nso,Nk],"dyson_kb_gf_d3","K")
    call assert_shape_kb_gf(G,[Nso,Nso,Nk],"dyson_kb_gf_d3","G")
    call assert_shape_kb_gf(dG,[Nso,Nso,Nk],"dyson_kb_gf_d3","dG")
    call assert_shape_kb_gf(dG_new,[Nso,Nso,Nk],"dyson_kb_gf_d3","dG_new")
    !
    select case(Nso)
    case(1)
       do ik=1,Nk
          call dyson_kb_gf_rank0(H(1,1,ik,:),K(1,1,ik),G(1,1,ik),dG(1,1,ik),dG_new(1,1,ik))
       enddo
    case default
       do ik=1,Nk
          call dyson_kb_gf_rank2(H(:,:,ik,:),K(:,:,ik),G(:,:,ik),dG(:,:,ik),dG_new(:,:,ik))
       enddo
    end select
    !
  end subroutine dyson_kb_gf_d3

  subroutine dyson_kb_gf_d3_(H,K,G,dG,dG_new)
    complex(8),dimension(:,:,:,:) :: H            ![Nso][Nso][Nk][Nt]
    type(kb_gf),intent(in)        :: K(:,:)       ![Nso][Nso]
    type(kb_gf),intent(inout)     :: G(:,:,:)     ![Nso][Nso][Nk]
    type(kb_dgf),intent(inout)    :: dG(:,:,:)    !as G
    type(kb_dgf)                  :: dG_new(:,:,:)!as G
    integer                       :: Nk,Nso,ik
    Nso = size(H,1)
    Nk  = size(H,3)
    !
    call assert_shape(H,[Nso,Nso,Nk,cc_params%Ntime],"dyson_kb_gf_d3","H")
    call assert_shape_kb_gf(K,[Nso,Nso],"dyson_kb_gf_d3","K")
    call assert_shape_kb_gf(G,[Nso,Nso,Nk],"dyson_kb_gf_d3","G")
    call assert_shape_kb_gf(dG,[Nso,Nso,Nk],"dyson_kb_gf_d3","dG")
    call assert_shape_kb_gf(dG_new,[Nso,Nso,Nk],"dyson_kb_gf_d3","dG_new")
    !
    select case(Nso)
    case(1)
       do ik=1,Nk
          call dyson_kb_gf_rank0(H(1,1,ik,:),K(1,1),G(1,1,ik),dG(1,1,ik),dG_new(1,1,ik))
       enddo
    case default
       do ik=1,Nk
          call dyson_kb_gf_rank2(H(:,:,ik,:),K(:,:),G(:,:,ik),dG(:,:,ik),dG_new(:,:,ik))
       enddo
    end select
    !
  end subroutine dyson_kb_gf_d3_



  subroutine dyson_kb_gf_d4(H,K,G,dG,dG_new)
    complex(8),dimension(:,:,:)             :: H      ![Nso][Nso][Nt]
    type(kb_gf),intent(in)                  :: K(:,:,:,:)  ![Nspin][Nspin][Norb][Norb]
    type(kb_gf),intent(inout)               :: G(:,:,:,:)     !as K
    type(kb_dgf),intent(inout)              :: dG(:,:,:,:)    !as K
    type(kb_dgf)                            :: dG_new(:,:,:,:)!as K
    Nspin = size(K,1)
    Norb  = size(K,3)
    Nso   = Nspin*Norb
    !
    call assert_shape(H,[Nso,Nso,cc_params%Ntime],"dyson_kb_gf_d4","H")
    call assert_shape_kb_gf(K,[Nspin,Nspin,Norb,Norb],"dyson_kb_gf_d4","K")
    call assert_shape_kb_gf(G,[Nspin,Nspin,Norb,Norb],"dyson_kb_gf_d4","G")
    call assert_shape_kb_gf(dG,[Nspin,Nspin,Norb,Norb],"dyson_kb_gf_d4","dG")
    call assert_shape_kb_gf(dG_new,[Nspin,Nspin,Norb,Norb],"dyson_kb_gf_d4","dG_new")
    !
    select case(Nso)
    case(1)
       call dyson_kb_gf_rank0(H(1,1,:),K(1,1,1,1),G(1,1,1,1),dG(1,1,1,1),dG_new(1,1,1,1))
    case default
       call dyson_kb_gf_rank4(H,K,G,dG,dG_new)
    end select
  end subroutine dyson_kb_gf_d4



  subroutine dyson_kb_gf_d5(H,K,G,dG,dG_new)
    complex(8),dimension(:,:,:,:)           :: H      ![Nso][Nso][Nk][Nt]
    type(kb_gf),intent(in)                  :: K(:,:,:,:,:)     ![Nspin][Nspin][Norb][Norb][Nk]
    type(kb_gf),intent(inout)               :: G(:,:,:,:,:)     !as K
    type(kb_dgf),intent(inout)              :: dG(:,:,:,:,:)    !as K
    type(kb_dgf)                            :: dG_new(:,:,:,:,:)!as K
    type(kb_gf),dimension(:,:),allocatable  :: K_,G_
    type(kb_dgf),dimension(:,:),allocatable :: dG_,dG_new_
    integer                                 :: Nk
    Nspin = size(G,1)
    Norb  = size(G,3)
    Nk    = size(H,3)
    Nso   = Nspin*Norb
    !
    call assert_shape(H,[Nso,Nso,Nk,cc_params%Ntime],"dyson_kb_gf_d5","H")
    call assert_shape_kb_gf(K,[Nspin,Nspin,Norb,Norb,Nk],"dyson_kb_gf_d5","K")
    call assert_shape_kb_gf(G,[Nspin,Nspin,Norb,Norb,Nk],"dyson_kb_gf_d5","G")
    call assert_shape_kb_gf(dG,[Nspin,Nspin,Norb,Norb,Nk],"dyson_kb_gf_d5","dG")
    call assert_shape_kb_gf(dG_new,[Nspin,Nspin,Norb,Norb,Nk],"dyson_kb_gf_d5","dG_new")
    !
    select case(Nso)
    case(1)
       do ik=1,Nk
          call dyson_kb_gf_rank0(&
               H(1,1,ik,:),&
               K(1,1,1,1,ik),&
               G(1,1,1,1,ik),&
               dG(1,1,1,1,ik),&
               dG_new(1,1,1,1,ik))
       enddo
    case default
       do ik=1,Nk
          call dyson_kb_gf_rank4(&
               H(:,:,ik,:),&
               K(:,:,:,:,ik),&
               G(:,:,:,:,ik),&
               dG(:,:,:,:,ik),&
               dG_new(:,:,:,:,ik))
       enddo
    end select
  end subroutine dyson_kb_gf_d5

  subroutine dyson_kb_gf_d5_(H,K,G,dG,dG_new)
    complex(8),dimension(:,:,:,:)           :: H      ![Nso][Nso][Nk][Nt]
    type(kb_gf),intent(in)                  :: K(:,:,:,:)  ![Nspin][Nspin][Norb][Norb]
    type(kb_gf),intent(inout)               :: G(:,:,:,:,:)     !as K
    type(kb_dgf),intent(inout)              :: dG(:,:,:,:,:)    !as K
    type(kb_dgf)                            :: dG_new(:,:,:,:,:)!as K
    type(kb_gf),dimension(:,:),allocatable  :: K_,G_
    type(kb_dgf),dimension(:,:),allocatable :: dG_,dG_new_
    integer                                 :: Nk
    !
    Nspin = size(K,1)
    Norb  = size(K,3)
    Nk    = size(H,3)
    Nso   = Nspin*Norb
    !
    call assert_shape(H,[Nso,Nso,Nk,cc_params%Ntime],"dyson_kb_gf_d5","H")
    call assert_shape_kb_gf(K,[Nspin,Nspin,Norb,Norb],"dyson_kb_gf_d5","K")
    call assert_shape_kb_gf(G,[Nspin,Nspin,Norb,Norb,Nk],"dyson_kb_gf_d5","G")
    call assert_shape_kb_gf(dG,[Nspin,Nspin,Norb,Norb,Nk],"dyson_kb_gf_d5","dG")
    call assert_shape_kb_gf(dG_new,[Nspin,Nspin,Norb,Norb,Nk],"dyson_kb_gf_d5","dG_new")
    !
    select case(Nso)
    case(1)
       do ik=1,Nk
          call dyson_kb_gf_rank0(&
               H(1,1,ik,:),&
               K(1,1,1,1),&
               G(1,1,1,1,ik),&
               dG(1,1,1,1,ik),&
               dG_new(1,1,1,1,ik))
       enddo
    case default
       do ik=1,Nk
          call dyson_kb_gf_rank4(&
               H(:,:,ik,:),&
               K(:,:,:,:),&
               G(:,:,:,:,ik),&
               dG(:,:,:,:,ik),&
               dG_new(:,:,:,:,ik))
       enddo
    end select
  end subroutine dyson_kb_gf_d5_



  subroutine dyson_kb_gf_d6(H,K,G,dG,dG_new)
    complex(8),dimension(:,:,:)             :: H      ![Nso][Nso][Nt]
    type(kb_gf),intent(in)                  :: K(:,:,:,:,:,:)  ![Nlat][Nlat][Nspin][Nspin][Norb][Norb]
    type(kb_gf),intent(inout)               :: G(:,:,:,:,:,:)     !as K
    type(kb_dgf),intent(inout)              :: dG(:,:,:,:,:,:)    !as K
    type(kb_dgf)                            :: dG_new(:,:,:,:,:,:)!as K
    !
    !
    Nlat  = size(K,1)
    Nspin = size(K,3)
    Norb  = size(K,5)
    Nlso  = Nlat*Nspin*Norb
    !
    call assert_shape(H,[Nlso,Nlso,cc_params%Ntime],"dyson_kb_gf_d6","H")
    call assert_shape_kb_gf(K,[Nlat,Nlat,Nspin,Nspin,Norb,Norb],"dyson_kb_gf_d6","K")
    call assert_shape_kb_gf(G,[Nlat,Nlat,Nspin,Nspin,Norb,Norb],"dyson_kb_gf_d6","G")
    call assert_shape_kb_gf(dG,[Nlat,Nlat,Nspin,Nspin,Norb,Norb],"dyson_kb_gf_d6","dG")
    call assert_shape_kb_gf(dG_new,[Nlat,Nlat,Nspin,Nspin,Norb,Norb],"dyson_kb_gf_d6","dG_new")
    !
    select case(Nlso)
    case (1)
       call dyson_kb_gf_rank0(H(1,1,:),K(1,1,1,1,1,1),G(1,1,1,1,1,1),dG(1,1,1,1,1,1),dG_new(1,1,1,1,1,1))
    case default
       call dyson_kb_gf_rank6(H,K,G,dG,dG_new)
    end select
  end subroutine dyson_kb_gf_d6



  subroutine dyson_kb_gf_d6_(H,K,G,dG,dG_new) !local kernel
    complex(8),dimension(:,:,:)                    :: H      ![Nlso][Nlso][Nt]
    type(kb_gf),intent(in)                         :: K(:,:,:,:,:)  ![Nlat][Nspin][Nspin][Norb][Norb]
    type(kb_gf),intent(inout)                      :: G(:,:,:,:,:,:)     !
    type(kb_dgf),intent(inout)                     :: dG(:,:,:,:,:,:)    !as K
    type(kb_dgf)                                   :: dG_new(:,:,:,:,:,:)!as K
    type(kb_gf),dimension(:,:,:,:,:,:),allocatable :: K_
    !
    Nlat  = size(K,1)
    Nspin = size(K,2)
    Norb  = size(K,4)
    Nlso  = Nlat*Nspin*Norb
    !
    call assert_shape(H,[Nlso,Nlso,cc_params%Ntime],"dyson_kb_gf_d6","H")
    call assert_shape_kb_gf(K,[Nlat,Nspin,Nspin,Norb,Norb],"dyson_kb_gf_d6","K")
    call assert_shape_kb_gf(G,[Nlat,Nlat,Nspin,Nspin,Norb,Norb],"dyson_kb_gf_d6","G")
    call assert_shape_kb_gf(dG,[Nlat,Nlat,Nspin,Nspin,Norb,Norb],"dyson_kb_gf_d6","dG")
    call assert_shape_kb_gf(dG_new,[Nlat,Nlat,Nspin,Nspin,Norb,Norb],"dyson_kb_gf_d6","dG_new")
    !    
    select case(Nlso)
    case (1)
       call dyson_kb_gf_rank0(H(1,1,:),K(1,1,1,1,1),G(1,1,1,1,1,1),dG(1,1,1,1,1,1),dG_new(1,1,1,1,1,1))
    case default
       allocate(K_(Nlat,Nlat,Nspin,Nspin,Norb,Norb))
       call K_%init()
       do concurrent(ilat=1:Nlat,ispin=1:Nspin,iorb=1:Norb,jspin=1:Nspin,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb + (ilat-1)*Norb*Nspin
          jo = jorb + (jspin-1)*Norb + (ilat-1)*Norb*Nspin
          K_(ilat,ilat,ispin,jspin,iorb,jorb) = K(ilat,ispin,jspin,iorb,jorb)
       enddo
       call dyson_kb_gf_rank6(H,K_,G,dG,dG_new)
       call K_%free()
    end select
  end subroutine dyson_kb_gf_d6_


  subroutine dyson_kb_gf_d7(H,K,G,dG,dG_new)
    complex(8),dimension(:,:,:,:)           :: H      ![Nlso][Nlso][Nk][Nt]
    type(kb_gf),intent(in)                  :: K(:,:,:,:,:,:,:)  ![Nlat][Nlat][Nspin][Nspin][Norb][Norb][Nk]
    type(kb_gf),intent(inout)               :: G(:,:,:,:,:,:,:)     !as K
    type(kb_dgf),intent(inout)              :: dG(:,:,:,:,:,:,:)    !as K
    type(kb_dgf)                            :: dG_new(:,:,:,:,:,:,:)!as K
    integer                                 :: Nk
    Nlat  = size(K,1)
    Nspin = size(K,3)
    Norb  = size(K,5)
    Nk    = size(H,3)
    Nlso  = Nlat*Nspin*Norb
    !
    call assert_shape(H,[Nlso,Nlso,Nk,cc_params%Ntime],"dyson_kb_gf_d7","H")
    call assert_shape_kb_gf(K,[Nlat,Nlat,Nspin,Nspin,Norb,Norb,Nk],"dyson_kb_gf_d7","K")
    call assert_shape_kb_gf(G,[Nlat,Nlat,Nspin,Nspin,Norb,Norb,Nk],"dyson_kb_gf_d7","G")
    call assert_shape_kb_gf(dG,[Nlat,Nlat,Nspin,Nspin,Norb,Norb,Nk],"dyson_kb_gf_d7","dG")
    call assert_shape_kb_gf(dG_new,[Nlat,Nlat,Nspin,Nspin,Norb,Norb,Nk],"dyson_kb_gf_d7","dG_new")
    !
    select case(Nlso)
    case(1)
       do ik=1,Nk
          call dyson_kb_gf_rank0(&
               H(1,1,ik,:),&
               K(1,1,1,1,1,1,ik),&
               G(1,1,1,1,1,1,ik),&
               dG(1,1,1,1,1,1,ik),&
               dG_new(1,1,1,1,1,1,ik))
       enddo
    case default
       do ik=1,Nk
          call dyson_kb_gf_rank6(&
               H(:,:,ik,:),&
               K(:,:,:,:,:,:,ik),&
               G(:,:,:,:,:,:,ik),&
               dG(:,:,:,:,:,:,ik),&
               dG_new(:,:,:,:,:,:,ik))
       enddo
    end select
  end subroutine dyson_kb_gf_d7

  subroutine dyson_kb_gf_d7_(H,K,G,dG,dG_new)
    complex(8),dimension(:,:,:,:) :: H      ![Nlso][Nlso][Nk][Nt]
    type(kb_gf),intent(in)        :: K(:,:,:,:,:,:)  ![Nlat][Nlat][Nspin][Nspin][Norb][Norb]
    type(kb_gf),intent(inout)     :: G(:,:,:,:,:,:,:)     !as K.[Nk]
    type(kb_dgf),intent(inout)    :: dG(:,:,:,:,:,:,:)    !as K.[Nk]
    type(kb_dgf)                  :: dG_new(:,:,:,:,:,:,:)!as K.[Nk]
    integer                       :: Nk
    Nlat  = size(K,1)
    Nspin = size(K,3)
    Norb  = size(K,5)
    Nk    = size(H,3)
    Nlso  = Nlat*Nspin*Norb
    !
    call assert_shape(H,[Nlso,Nlso,Nk,cc_params%Ntime],"dyson_kb_gf_d7","H")
    call assert_shape_kb_gf(K,[Nlat,Nlat,Nspin,Nspin,Norb,Norb],"dyson_kb_gf_d7","K")
    call assert_shape_kb_gf(G,[Nlat,Nlat,Nspin,Nspin,Norb,Norb,Nk],"dyson_kb_gf_d7","G")
    call assert_shape_kb_gf(dG,[Nlat,Nlat,Nspin,Nspin,Norb,Norb,Nk],"dyson_kb_gf_d7","dG")
    call assert_shape_kb_gf(dG_new,[Nlat,Nlat,Nspin,Nspin,Norb,Norb,Nk],"dyson_kb_gf_d7","dG_new")
    !
    select case(Nlso)
    case(1)
       do ik=1,Nk
          call dyson_kb_gf_rank0(&
               H(1,1,ik,:),&
               K(1,1,1,1,1,1),&
               G(1,1,1,1,1,1,ik),&
               dG(1,1,1,1,1,1,ik),&
               dG_new(1,1,1,1,1,1,ik))
       enddo
    case default
       do ik=1,Nk
          call dyson_kb_gf_rank6(&
               H(:,:,ik,:),&
               K(:,:,:,:,:,:),&
               G(:,:,:,:,:,:,ik),&
               dG(:,:,:,:,:,:,ik),&
               dG_new(:,:,:,:,:,:,ik))
       enddo
    end select
  end subroutine dyson_kb_gf_d7_



  subroutine dyson_kb_gf_d7__(H,K,G,dG,dG_new)
    complex(8),dimension(:,:,:,:)                  :: H      ![Nlso][Nlso][Nk][Nt]
    type(kb_gf),intent(in)                         :: K(:,:,:,:,:)         ![Nlat][Nspin][Nspin][Norb][Norb]
    type(kb_gf),intent(inout)                      :: G(:,:,:,:,:,:,:)     ![Nlat][Nlat][Nspin][Nspin][Norb][Norb][Nk]
    type(kb_dgf),intent(inout)                     :: dG(:,:,:,:,:,:,:)    !as G
    type(kb_dgf)                                   :: dG_new(:,:,:,:,:,:,:)!as G
    type(kb_gf),dimension(:,:,:,:,:,:),allocatable :: K_
    integer                                        :: Nk
    Nlat  = size(K,1)
    Nspin = size(K,3)
    Norb  = size(K,5)
    Nk    = size(H,3)
    Nlso  = Nlat*Nspin*Norb
    !
    call assert_shape(H,[Nlso,Nlso,Nk,cc_params%Ntime],"dyson_kb_gf_d7","H")
    call assert_shape_kb_gf(K,[Nlat,Nlat,Nspin,Nspin,Norb,Norb],"dyson_kb_gf_d7","K")
    call assert_shape_kb_gf(G,[Nlat,Nlat,Nspin,Nspin,Norb,Norb,Nk],"dyson_kb_gf_d7","G")
    call assert_shape_kb_gf(dG,[Nlat,Nlat,Nspin,Nspin,Norb,Norb,Nk],"dyson_kb_gf_d7","dG")
    call assert_shape_kb_gf(dG_new,[Nlat,Nlat,Nspin,Nspin,Norb,Norb,Nk],"dyson_kb_gf_d7","dG_new")
    !
    select case(Nlso)
    case(1)
       do ik=1,Nk
          call dyson_kb_gf_rank0(&
               H(1,1,ik,:),&
               K(1,1,1,1,1),&
               G(1,1,1,1,1,1,ik),&
               dG(1,1,1,1,1,1,ik),&
               dG_new(1,1,1,1,1,1,ik))
       enddo
    case default
       allocate(K_(Nlat,Nlat,Nspin,Nspin,Norb,Norb))
       call K_%init()
       do concurrent(ilat=1:Nlat,ispin=1:Nspin,iorb=1:Norb,jspin=1:Nspin,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb + (ilat-1)*Norb*Nspin
          jo = jorb + (jspin-1)*Norb + (ilat-1)*Norb*Nspin
          K_(ilat,ilat,ispin,jspin,iorb,jorb) = K(ilat,ispin,jspin,iorb,jorb)
       enddo
       do ik=1,Nk
          call dyson_kb_gf_rank6(&
               H(:,:,ik,:),&
               K_(:,:,:,:,:,:),&
               G(:,:,:,:,:,:,ik),&
               dG(:,:,:,:,:,:,ik),&
               dG_new(:,:,:,:,:,:,ik))
       enddo
       call K_%free()
    end select
  end subroutine dyson_kb_gf_d7__


END MODULE KB_GF_DYSON











