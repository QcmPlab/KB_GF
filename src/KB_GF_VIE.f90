MODULE KB_GF_VIE
  USE KB_VARS_GLOBAL
  USE KB_CONTOUR
  USE KB_AUX
  USE KB_GF_COMMON
  USE SCIFOR, only: one,xi,zero,pi,zeye,inv
  implicit none
  private


  ! CONVOLUTION:
  interface vie_kb_gf
     module procedure :: vie_kb_gf_rank0
     module procedure :: vie_kb_gf_d1
     module procedure :: vie_kb_gf_d2
     module procedure :: vie_kb_gf_d3
     module procedure :: vie_kb_gf_d4
     module procedure :: vie_kb_gf_d5
     module procedure :: vie_kb_gf_d6
     module procedure :: vie_kb_gf_d7
  end interface vie_kb_gf


  public :: vie_kb_gf

contains




  !----------------------------------------------------------------------------
  !  This subroutine solves a Volterra integral equation of the second kind,
  !              G(t,t')+(K*G)(t,t')=Q(t,t')
  !  for t=n*dt or t'=n*dt, using 2^nd *implicit* Runge-Kutta method.
  !----------------------------------------------------------------------------
  subroutine vie_kb_gf_Rank0(G,K,Q,notail)
    type(kb_gf), intent(inout)          :: G
    type(kb_gf), intent(in)             :: K
    type(kb_gf), intent(in)             :: Q
    logical,optional                    :: notail
    logical                             :: notail_
    integer                             :: N,L,Niw
    real(8)                             :: dt,dtau
    integer                             :: i,j,s,itau,jtau
    real(8),dimension(:),allocatable    :: ftau
    complex(8),dimension(:),allocatable :: KxG
    !
    notail_=.true.;if(present(notail))notail_=notail
    !
    N   = cc_params%Nt                 !<== work with the ACTUAL size of the contour
    L   = cc_params%Ntau
    Niw = cc_params%Niw
    dt  = cc_params%dt
    dtau= cc_params%dtau
    !
    allocate(KxG(0:max(N,L)))
    !
    if(N==1)then
       allocate(ftau(0:Niw))
       !Mats component:
       ![1d0 + K(iw)].G(iw) = Q(iw)
       G%iw = Q%iw/(one + K%iw)       
       call fft_iw2tau(G%iw,ftau(0:),cc_params%beta,notail=notail_)
       call fft_extract_gtau(ftau(0:),G%mats(0:))
       ! Ret component
       ! G^R(t,t') = Q^R(t,t')
       ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       !TIP   t_{N}, t`_{N}
       G%ret(1,1)=Q%ret(1,1)
       !
       ! Lmix component
       ! G^\lmix(t,tau') = Q^\lmix(t,tau') + \int_0^\beta K^\lmix(t,s)*G^M(s,tau')ds
       ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       do jtau=0,L
          G%lmix(1,jtau)=Q%lmix(1,jtau)
          KxG(0:)=zero
          do s=0,jtau
             KxG(s)=K%lmix(1,s)*G%mats(s+L-jtau)
          end do
          G%lmix(1,jtau)=G%lmix(1,jtau)-dtau*kb_trapz(KxG(0:),0,jtau)
          KxG(0:)=zero
          do s=jtau,L
             KxG(s)=K%lmix(1,s)*G%mats(s-jtau)
          end do
          G%lmix(1,jtau)=G%lmix(1,jtau)+dtau*kb_trapz(KxG(0:),jtau,L)
       end do
       !
       ! Less component
       ! G^<(t,t')  = Q^<(t,t') - i\int_0^\beta K^\lmix(t,s)*G^\rmix(s,t')ds
       ! G^{<}(t_{n},t_{n}) <= Diagonal
       ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
       G%less(1,1)=Q%less(1,1)
       KxG(0:)=zero
       do s=0,L
          KxG(s)=K%lmix(1,s)*conjg(G%lmix(1,L-s))
       enddo
       G%less(1,1)=G%less(1,1)-xi*dtau*kb_trapz(KxG(0:),0,L)
       !
       deallocate(ftau)
       return
       !
    end if
    !
    !
    ! Ret component
    ! G^R(t,t') - \int_{t'}^t K^R(t,s)*G^R(s,t')ds = Q^R(t,t')
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !TIP   t_{N}, t`_{N}
    G%ret(N,N)=Q%ret(N,N)
    !VERTICAL INTERVAL  t_{N}, t`_{j, j=1,...,N-1}
    do j=1,N-1
       G%ret(N,j)=Q%ret(N,j)
       !
       KxG(0:)=zero
       do s=j,N-1
          KxG(s)=K%ret(N,s)*G%ret(s,j)
       end do
       G%ret(N,j)=G%ret(N,j) + dt*kb_half_trapz(KxG(0:),j,N-1)
       !
       G%ret(N,j)=G%ret(N,j)/(1.d0-0.5d0*dt*K%ret(N,N))
    end do
    !
    ! Lmix component
    ! G^\lmix(t,tau') - \int_0^t K^R(t,s)*G^\lmix(s,tau')ds
    !    = Q^\lmix(t,tau') + \int_0^\beta K^\lmix(t,s)*G^M(s,tau')ds
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    do jtau=0,L
       G%lmix(N,jtau)=Q%lmix(N,jtau)
       !
       KxG(0:)=zero
       do s=0,jtau
          KxG(s)=K%lmix(N,s)*G%mats(s+L-jtau)
       end do
       G%lmix(N,jtau)=G%lmix(N,jtau)-dtau*kb_trapz(KxG(0:),0,jtau)
       !
       KxG(0:)=zero
       do s=jtau,L
          KxG(s)=K%lmix(N,s)*G%mats(s-jtau)
       end do
       G%lmix(N,jtau)=G%lmix(N,jtau)+dtau*kb_trapz(KxG(0:),jtau,L)
       !
       KxG(0:)=zero
       do s=1,N-1
          KxG(s)=K%ret(N,s)*G%lmix(s,jtau)
       end do
       G%lmix(N,jtau)=G%lmix(N,jtau) + dt*kb_half_trapz(KxG(0:),1,N-1)
       !
       G%lmix(N,jtau)=G%lmix(N,jtau)/(1.d0-0.5d0*dt*K%ret(N,N))
    end do
    !
    ! Less component
    ! G^<(t,t') - \int_0^t K^{R}(t,s)*G^{<}(s,t')ds
    !    = Q^<(t,t') - i\int_0^\beta K^\lmix(t,s)*G^\rmix(s,t')ds
    !      + \int_0^{t'} K^<(t,s)*G^A(s,t')ds
    ! G^<(t_{N},t_{j}), j=1,...,N-1
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    do j=1, N-1
       G%less(N,j)=Q%less(N,j)
       !
       KxG(0:)=zero
       do s=0,L
          KxG(s)=K%lmix(N,s)*conjg(G%lmix(j,L-s))
       enddo
       G%less(N,j)=G%less(N,j)-xi*dtau*kb_trapz(KxG(0:),0,L)
       !
       KxG(0:)=zero
       do s=1,j
          KxG(s)=K%less(N,s)*conjg(G%ret(j,s))
       enddo
       G%less(N,j)=G%less(N,j)+dt*kb_trapz(KxG(0:),1,j)
       !
       KxG(0:)=zero
       do s=1,N-1
          KxG(s)=K%ret(N,s)*G%less(s,j)
       enddo
       G%less(N,j)=G%less(N,j) + dt*kb_half_trapz(KxG(0:),1,N-1)
       !
       G%less(N,j)=G%less(N,j)/(1.d0-0.5d0*dt*K%ret(N,N))
    end do
    !
    ! G^<(t_{i},t_{N}) <= Hermite conjugate
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    do i=1,N-1
       G%less(i,N) = -conjg(G%less(N,i))
    end do
    !
    ! G^{<}(t_{n},t_{n}) <= Diagonal
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
    G%less(N,N)=Q%less(N,N)
    !
    KxG(0:)=zero
    do s=0,L
       KxG(s)=K%lmix(N,s)*conjg(G%lmix(N,L-s))
    end do
    G%less(N,N)=G%less(N,N)-xi*dtau*kb_trapz(KxG(0:),0,L)
    !
    KxG(0:)=zero
    do s=1,N
       KxG(s)=K%less(N,s)*conjg(G%ret(N,s))
    end do
    G%less(N,N)=G%less(N,N)+dt*kb_trapz(KxG(0:),1,N)
    !
    KxG(0:)=zero
    do s=1,N-1
       KxG(s)=K%ret(N,s)*G%less(s,N)
    end do
    G%less(N,N)=G%less(N,N) + dt*kb_half_trapz(KxG(0:),1,N-1)
    !
    G%less(N,N)=G%less(N,N)/(1.d0-0.5d0*dt*K%ret(N,N))
    !
    deallocate(KxG)
    !
  end subroutine vie_kb_gf_Rank0





  subroutine vie_kb_gf_Rank2(G,K,Q,notail)
    type(kb_gf), intent(inout)            :: G(:,:)
    type(kb_gf), intent(in)               :: K(:,:)
    type(kb_gf), intent(in)               :: Q(:,:)
    logical,optional                      :: notail
    logical                               :: notail_
    integer                               :: N,L,Niw
    real(8)                               :: dt,dtau
    integer                               :: Nso,i,j,s,itau,jtau,io,jo,ko
    real(8),dimension(:),allocatable      :: ftau
    complex(8),dimension(:),allocatable   :: KxG
    complex(8),dimension(:,:),allocatable :: Amat,Gmat
    !
    notail_=.true.;if(present(notail))notail_=notail
    !
    N   = cc_params%Nt                 !<== work with the ACTUAL size of the contour
    L   = cc_params%Ntau
    Niw = cc_params%Niw
    dt  = cc_params%dt
    dtau= cc_params%dtau
    !
    Nso = size(G,1)
    call assert_shape_kb_gf(G,[Nso,Nso],"vie_kb_contour_gf_Rank2","G")
    call assert_shape_kb_gf(K,[Nso,Nso],"vie_kb_contour_gf_Rank2","K")
    call assert_shape_kb_gf(Q,[Nso,Nso],"vie_kb_contour_gf_Rank2","Q")
    !
    allocate(KxG(0:max(N,L)),Amat(Nso,Nso),Gmat(Nso,Nso))
    !
    if(N==1)then
       allocate(ftau(0:Niw))
       !Mats component:
       ![1d0 + K(iw)].G(iw) = Q(iw)
       do i=1,Niw
          do concurrent(io=1:Nso,jo=1:Nso)
             Amat(io,jo) = Q(io,jo)%iw(i)
             Gmat(io,jo) = zeye(io,jo) + K(io,jo)%iw(i)
          enddo
          call inv(Gmat)
          Gmat = matmul(Gmat,Amat)
          do concurrent(io=1:Nso,jo=1:Nso)
             G(io,jo)%iw(i) = Gmat(io,jo)
          enddo
       enddo
       !
       do io=1,Nso
          do jo=1,Nso
             call fft_iw2tau(G(io,jo)%iw,ftau(0:),cc_params%beta,notail=notail_)
             call fft_extract_gtau(ftau(0:),G(io,jo)%mats(0:))
          enddo
       enddo
       !
       ! Ret component
       ! G^R(t,t') = Q^R(t,t')
       ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       !TIP   t_{N}, t`_{N}
       forall(io=1:Nso,jo=1:Nso)G(io,jo)%ret(1,1)=Q(io,jo)%ret(1,1)
       !
       ! Lmix component
       ! G_ab^\lmix(t,tau') = Q_ab^\lmix(t,tau') + \int_0^\beta K_ak^\lmix(t,s)*G_kb^M(s,tau')ds
       ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       do jtau=0,L
          do concurrent(io=1:Nso,jo=1:Nso)
             G(io,jo)%lmix(1,jtau) = Q(io,jo)%lmix(1,jtau)
             KxG(0:)=zero           
             do s=0,jtau
                do ko=1,Nso
                   KxG(s)=KxG(s)+K(io,ko)%lmix(1,s)*G(ko,jo)%mats(s+L-jtau)
                enddo
             enddo
             G(io,jo)%lmix(1,jtau)=G(io,jo)%lmix(1,jtau) - dtau*kb_trapz(KxG(0:),0,jtau)
             KxG(0:)=zero
             do s=jtau,L
                do ko=1,Nso
                   KxG(s)=KxG(s)+K(io,ko)%lmix(1,s)*G(ko,jo)%mats(s-jtau)
                enddo
             enddo
             G(io,jo)%lmix(1,jtau)=G(io,jo)%lmix(1,jtau) + dtau*kb_trapz(KxG(0:),jtau,L)
          enddo
       end do
       !
       ! Less component
       ! G_ab^<(t,t')  = Q_ab^<(t,t') - i\int_0^\beta K_ak^\lmix(t,s)*G_kb^\rmix(s,t')ds
       ! G^{<}(t_{n},t_{n}) <= Diagonal
       ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
       do concurrent(io=1:Nso,jo=1:Nso)
          G(io,jo)%less(1,1) = Q(io,jo)%less(1,1)
          KxG(0:)=zero
          do s=0,L
             do ko=1,Nso
                KxG(s)=KxG(s)+K(io,ko)%lmix(1,s)*conjg( G(ko,jo)%lmix(1,L-s) ) !rmix <-- lmix
             enddo
          enddo
          G(io,jo)%less(1,1) = G(io,jo)%less(1,1) - xi*dtau*kb_trapz(KxG(0:),0,L)
       enddo
       !
       deallocate(ftau)
       !
       return
       !
    endif
    !
    !
    !
    ! Ret component
    ! G^R(t,t') - \int_{t'}^t K^R(t,s)*G^R(s,t')ds = Q^R(t,t')
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !TIP   t_{N}, t`_{N}
    forall(io=1:Nso,jo=1:Nso)G(io,jo)%ret(N,N)=Q(io,jo)%ret(N,N)
    !
    !VERTICAL INTERVAL  t_{N}, t`_{j, j=1,...,N-1}
    do j=1,N-1
       Amat=zeye(Nso)
       do concurrent(io=1:Nso,jo=1:Nso)
          Gmat(io,jo) = Q(io,jo)%ret(N,j)
          KxG(0:)=zero
          do s=j,N-1
             do ko=1,Nso
                KxG(s)=KxG(s) + K(io,ko)%ret(N,s)*G(ko,jo)%ret(s,j)
             enddo
          enddo
          Gmat(io,jo) = Gmat(io,jo) + dt*kb_half_trapz(KxG(0:),j,N-1)
          Amat(io,jo) = Amat(io,jo) - 0.5d0*dt*K(io,jo)%ret(N,N) !Check this line
       enddo
       call inv(Amat)
       Gmat = matmul(Amat,Gmat) !check this line
       forall(io=1:Nso,jo=1:Nso)G(io,jo)%ret(N,j) = Gmat(io,jo)
    end do
    !
    !
    ! Lmix component
    ! G^\lmix(t,tau') - \int_0^t K^R(t,s)*G^\lmix(s,tau')ds
    !    = Q^\lmix(t,tau') + \int_0^\beta K^\lmix(t,s)*G^M(s,tau')ds
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    do jtau=0,L
       Amat=zeye(Nso)
       do concurrent(io=1:Nso,jo=1:Nso)
          Gmat(io,jo) = Q(io,jo)%lmix(N,jtau)
          KxG(0:)=zero           
          do s=0,jtau
             do ko=1,Nso
                KxG(s)=KxG(s)+K(io,ko)%lmix(N,s)*G(ko,jo)%mats(s+L-jtau)
             enddo
          enddo
          Gmat(io,jo)=Gmat(io,jo) - dtau*kb_trapz(KxG(0:),0,jtau)
          !
          KxG(0:)=zero
          do s=jtau,L
             do ko=1,Nso
                KxG(s)=KxG(s)+K(io,ko)%lmix(N,s)*G(ko,jo)%mats(s-jtau)
             enddo
          enddo
          Gmat(io,jo)=Gmat(io,jo) + dtau*kb_trapz(KxG(0:),jtau,L)
          !
          KxG(0:)=zero
          do s=1,N-1
             do ko=1,Nso
                KxG(s)=KxG(s)+K(io,ko)%ret(N,s)*G(ko,jo)%lmix(s,jtau)
             enddo
          enddo
          Gmat(io,jo) = Gmat(io,jo) + dt*kb_half_trapz(KxG(0:),1,N-1)
          Amat(io,jo) = Amat(io,jo) - 0.5d0*dt*K(io,jo)%ret(N,N)
       enddo
       call inv(Amat)
       Gmat = matmul(Amat,Gmat) !check this line
       forall(io=1:Nso,jo=1:Nso)G(io,jo)%lmix(N,jtau) = Gmat(io,jo)
    end do
    !
    !
    ! Less component
    ! G^<(t,t') - \int_0^t K^{R}(t,s)*G^{<}(s,t')ds
    !    = Q^<(t,t') - i\int_0^\beta K^\lmix(t,s)*G^\rmix(s,t')ds
    !      + \int_0^{t'} K^<(t,s)*G^A(s,t')ds
    ! G^<(t_{N},t_{j}), j=1,...,N-1
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    do j=1,N-1
       Amat=zeye(Nso)
       do concurrent(io=1:Nso,jo=1:Nso)
          Gmat(io,jo) = Q(io,jo)%less(N,j)
          KxG(0:)=zero
          do s=0,L
             do ko=1,Nso
                KxG(s)=KxG(s)+K(io,ko)%lmix(N,s)*conjg( G(ko,jo)%lmix(j,L-s) ) !rmix <-- lmix
             enddo
          enddo
          Gmat(io,jo) = Gmat(io,jo) - xi*dtau*kb_trapz(KxG(0:),0,L)
          !
          KxG(0:)=zero
          do s=1,j
             do ko=1,Nso
                KxG(s)=KxG(s)+K(io,ko)%less(N,s)*conjg( G(ko,jo)%ret(j,s) ) !adv <-- ret
             enddo
          enddo
          Gmat(io,jo) = Gmat(io,jo) + dt*kb_trapz(KxG(0:),1,j)
          !
          KxG(0:)=zero
          do s=1,N-1
             do ko=1,Nso
                KxG(s)=KxG(s)+K(io,ko)%ret(N,s)*G(ko,jo)%less(s,j)
             enddo
          enddo
          Gmat(io,jo) = Gmat(io,jo) + dt*kb_half_trapz(KxG(0:),1,N-1)
          Amat(io,jo) = Amat(io,jo) - 0.5d0*dt*K(io,jo)%ret(N,N)           
       enddo
       call inv(Amat)
       Gmat = matmul(Amat,Gmat)
       forall(io=1:Nso,jo=1:Nso)G(io,jo)%less(N,j) = Gmat(io,jo)
    enddo
    !
    ! G^<(t_{i},t_{N}) <= Hermite conjugate
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    do i=1,N-1     
       forall(io=1:Nso,jo=1:Nso)G(io,jo)%less(i,N) = -conjg(G(jo,io)%less(N,i))
    end do
    !
    ! G^{<}(t_{n},t_{n}) <= Diagonal
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    Amat=zeye(Nso)
    do concurrent(io=1:Nso,jo=1:Nso)
       Gmat(io,jo) = Q(io,jo)%less(N,N)
       KxG(0:)=zero
       do s=0,L
          do ko=1,Nso
             KxG(s)=KxG(s)+K(io,ko)%lmix(N,s)*conjg( G(ko,jo)%lmix(N,L-s) ) !rmix <-- lmix
          enddo
       enddo
       Gmat(io,jo) = Gmat(io,jo) - xi*dtau*kb_trapz(KxG(0:),0,L)
       !
       KxG(0:)=zero
       do s=1,N
          do ko=1,Nso
             KxG(s)=KxG(s)+K(io,ko)%less(N,s)*conjg( G(ko,jo)%ret(N,s) ) !adv <-- ret
          enddo
       enddo
       Gmat(io,jo) = Gmat(io,jo) + dt*kb_trapz(KxG(0:),1,N)
       !
       KxG(0:)=zero
       do s=1,N-1
          do ko=1,Nso
             KxG(s)=KxG(s)+K(io,ko)%ret(N,s)*G(ko,jo)%less(s,N)
          enddo
       enddo
       Gmat(io,jo) = Gmat(io,jo) + dt*kb_half_trapz(KxG(0:),1,N-1)
       Amat(io,jo) = Amat(io,jo) - 0.5d0*dt*K(io,jo)%ret(N,N)
    enddo
    call inv(Amat)
    Gmat = matmul(Amat,Gmat)
    forall(io=1:Nso,jo=1:Nso)G(io,jo)%less(N,N) = Gmat(io,jo)
    !
    !
    deallocate(KxG,Amat,Gmat)
  end subroutine vie_kb_gf_Rank2





  subroutine vie_kb_gf_rank4(G,K,Q,notail)
    type(kb_gf), intent(inout)            :: G(:,:,:,:)
    type(kb_gf), intent(in)               :: K(:,:,:,:)
    type(kb_gf), intent(in)               :: Q(:,:,:,:)
    logical,optional                      :: notail
    logical                               :: notail_
    integer                               :: N,L,Niw
    real(8)                               :: dt,dtau
    integer                               :: Nspin,Norb,Nso,i,j,s,itau,jtau,io,jo
    real(8),dimension(:),allocatable      :: ftau
    complex(8),dimension(:),allocatable   :: KxG
    complex(8),dimension(:,:),allocatable :: Amat,Gmat
    !
    notail_=.true.;if(present(notail))notail_=notail
    !
    !
    N   = cc_params%Nt                 !<== work with the ACTUAL size of the contour
    L   = cc_params%Ntau
    Niw = cc_params%Niw
    dt  = cc_params%dt
    dtau= cc_params%dtau
    !
    Nspin = size(G,1) ; Norb = size(G,3) ; Nso=Nspin*Norb
    call assert_shape_kb_gf(G,[Nspin,Nspin,Norb,Norb],"vie_kb_gf_Rank4","G")
    call assert_shape_kb_gf(K,[Nspin,Nspin,Norb,Norb],"vie_kb_gf_Rank4","K")
    call assert_shape_kb_gf(Q,[Nspin,Nspin,Norb,Norb],"vie_kb_gf_Rank4","Q")
    !
    allocate(KxG(0:max(N,L)),Amat(Nso,Nso),Gmat(Nso,Nso))
    !
    if(N==1)then
       allocate(ftau(0:Niw))
       !Mats component: [1d0 + K(iw)].G(iw) = Q(iw)
       do i=1,Niw
          do concurrent(ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
             io = iorb + (ispin-1)*Norb
             jo = jorb + (jspin-1)*Norb
             Amat(io,jo) = Q(ispin,jspin,iorb,jorb)%iw(i)
             Gmat(io,jo) = zeye(io,jo) + K(ispin,jspin,iorb,jorb)%iw(i)
          enddo
          call inv(Gmat)
          Gmat = matmul(Gmat,Amat)
          do concurrent(ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
             io = iorb + (ispin-1)*Norb
             jo = jorb + (jspin-1)*Norb
             G(ispin,jspin,iorb,jorb)%iw(i) = Gmat(io,jo)
          enddo
       enddo
       !
       do ispin=1,Nspin
          do jspin=1,Nspin
             do iorb=1,Norb
                do jorb=1,Norb
                   call fft_iw2tau(G(ispin,jspin,iorb,jorb)%iw,ftau(0:),cc_params%beta,notail=notail_)
                   call fft_extract_gtau(ftau(0:),G(ispin,jspin,iorb,jorb)%mats(0:))
                enddo
             enddo
          enddo
       enddo
       !
       ! Ret component
       ! G^R(t,t') = Q^R(t,t')
       ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       !TIP   t_{N}, t`_{N}
       do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          G(ispin,jspin,iorb,jorb)%ret(1,1)=Q(ispin,jspin,iorb,jorb)%ret(1,1)
       enddo
       !
       ! Lmix component
       ! G_ab^\lmix(t,tau') = Q_ab^\lmix(t,tau') + \int_0^\beta K_ak^\lmix(t,s)*G_kb^M(s,tau')ds
       ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       do jtau=0,L
          do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
             G(ispin,jspin,iorb,jorb)%lmix(1,jtau) = Q(ispin,jspin,iorb,jorb)%lmix(1,jtau)
             KxG(0:)=zero           
             do s=0,jtau
                do kspin=1,Nspin
                   do korb=1,Norb
                      KxG(s)=KxG(s)+K(ispin,kspin,iorb,korb)%lmix(1,s)*G(kspin,jspin,korb,jorb)%mats(s+L-jtau)
                   enddo
                enddo
             enddo
             G(ispin,jspin,iorb,jorb)%lmix(1,jtau)=G(ispin,jspin,iorb,jorb)%lmix(1,jtau) - dtau*kb_trapz(KxG(0:),0,jtau)
             KxG(0:)=zero
             do s=jtau,L
                do kspin=1,Nspin
                   do korb=1,Norb
                      KxG(s)=KxG(s)+K(ispin,kspin,iorb,korb)%lmix(1,s)*G(kspin,jspin,korb,jorb)%mats(s-jtau)
                   enddo
                enddo
             enddo
             G(ispin,jspin,iorb,jorb)%lmix(1,jtau)=G(ispin,jspin,iorb,jorb)%lmix(1,jtau) + dtau*kb_trapz(KxG(0:),jtau,L)
          enddo
       enddo
       !
       ! Less component
       ! G_ab^<(t,t')  = Q_ab^<(t,t') - i\int_0^\beta K_ak^\lmix(t,s)*G_kb^\rmix(s,t')ds
       ! G^{<}(t_{n},t_{n}) <= Diagonal
       ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
       do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          G(ispin,jspin,iorb,jorb)%less(1,1) = Q(ispin,jspin,iorb,jorb)%less(1,1)
          KxG(0:)=zero
          do s=0,L
             do kspin=1,Nspin
                do korb=1,Norb
                   KxG(s)=KxG(s)+K(ispin,kspin,iorb,korb)%lmix(1,s)*conjg( G(kspin,jspin,korb,jorb)%lmix(1,L-s) ) !rmix <-- lmix
                enddo
             enddo
          enddo
          G(ispin,jspin,iorb,jorb)%less(1,1) = G(ispin,jspin,iorb,jorb)%less(1,1) - xi*dtau*kb_trapz(KxG(0:),0,L)
       enddo
       !
       deallocate(ftau)
       !
       return
       !
    endif
    !
    !
    !
    ! Ret component
    ! G^R(t,t') - \int_{t'}^t K^R(t,s)*G^R(s,t')ds = Q^R(t,t')
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !TIP   t_{N}, t`_{N}
    do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
       G(ispin,jspin,iorb,jorb)%ret(N,N)=Q(ispin,jspin,iorb,jorb)%ret(N,N)
    enddo
    !
    !VERTICAL INTERVAL  t_{N}, t`_{j, j=1,...,N-1}
    do j=1,N-1
       Amat=zeye(Nso)
       do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb
          jo = jorb + (jspin-1)*Norb
          Gmat(io,jo) = Q(ispin,jspin,iorb,jorb)%ret(N,j)
          KxG(0:)=zero
          do s=j,N-1
             do kspin=1,Nspin
                do korb=1,Norb
                   KxG(s)=KxG(s) + K(ispin,kspin,iorb,korb)%ret(N,s)*G(kspin,jspin,korb,jorb)%ret(s,j)
                enddo
             enddo
          enddo
          Gmat(io,jo) = Gmat(io,jo) + dt*kb_half_trapz(KxG(0:),j,N-1)
          Amat(io,jo) = Amat(io,jo) - 0.5d0*dt*K(ispin,jspin,iorb,jorb)%ret(N,N) !Check this line
       enddo
       call inv(Amat)
       Gmat = matmul(Amat,Gmat) !check this line
       do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          G(ispin,jspin,iorb,jorb)%ret(N,j) = Gmat(io,jo)
       enddo
    end do
    !
    !
    ! Lmix component
    ! G^\lmix(t,tau') - \int_0^t K^R(t,s)*G^\lmix(s,tau')ds
    !    = Q^\lmix(t,tau') + \int_0^\beta K^\lmix(t,s)*G^M(s,tau')ds
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    do jtau=0,L
       Amat=zeye(Nso)
       do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb
          jo = jorb + (jspin-1)*Norb
          Gmat(io,jo) = Q(ispin,jspin,iorb,jorb)%lmix(N,jtau)
          KxG(0:)=zero           
          do s=0,jtau
             do kspin=1,Nspin
                do korb=1,Norb
                   KxG(s)=KxG(s)+K(ispin,kspin,iorb,korb)%lmix(N,s)*G(kspin,jspin,korb,jorb)%mats(s+L-jtau)
                enddo
             enddo
          enddo
          Gmat(io,jo)=Gmat(io,jo) - dtau*kb_trapz(KxG(0:),0,jtau)
          !
          KxG(0:)=zero
          do s=jtau,L
             do kspin=1,Nspin
                do korb=1,Norb
                   KxG(s)=KxG(s)+K(ispin,kspin,iorb,korb)%lmix(N,s)*G(kspin,jspin,korb,jorb)%mats(s-jtau)
                enddo
             enddo
          enddo
          Gmat(io,jo)=Gmat(io,jo) + dtau*kb_trapz(KxG(0:),jtau,L)
          !
          KxG(0:)=zero
          do s=1,N-1
             do kspin=1,Nspin
                do korb=1,Norb
                   KxG(s)=KxG(s)+K(ispin,kspin,iorb,korb)%ret(N,s)*G(kspin,jspin,korb,jorb)%lmix(s,jtau)
                enddo
             enddo
          enddo
          Gmat(io,jo) = Gmat(io,jo) + dt*kb_half_trapz(KxG(0:),1,N-1)
          Amat(io,jo) = Amat(io,jo) - 0.5d0*dt*K(ispin,jspin,iorb,jorb)%ret(N,N)
       enddo
       call inv(Amat)
       Gmat = matmul(Amat,Gmat) !check this line
       do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb
          jo = jorb + (jspin-1)*Norb
          G(ispin,jspin,iorb,jorb)%lmix(N,jtau) = Gmat(io,jo)
       end do
    end do
    !
    !
    ! Less component
    ! G^<(t,t') - \int_0^t K^{R}(t,s)*G^{<}(s,t')ds
    !    = Q^<(t,t') - i\int_0^\beta K^\lmix(t,s)*G^\rmix(s,t')ds
    !      + \int_0^{t'} K^<(t,s)*G^A(s,t')ds
    ! G^<(t_{N},t_{j}), j=1,...,N-1
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    do j=1,N-1
       Amat=zeye(Nso)
       do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          Gmat(io,jo) = Q(ispin,jspin,iorb,jorb)%less(N,j)
          KxG(0:)=zero
          do s=0,L
             do kspin=1,Nspin
                do korb=1,Norb
                   KxG(s)=KxG(s)+K(ispin,kspin,iorb,korb)%lmix(N,s)*conjg( G(kspin,jspin,korb,jorb)%lmix(j,L-s) ) !rmix <-- lmix
                enddo
             enddo
          enddo
          Gmat(io,jo) = Gmat(io,jo) - xi*dtau*kb_trapz(KxG(0:),0,L)
          !
          KxG(0:)=zero
          do s=1,j
             do kspin=1,Nspin
                do korb=1,Norb
                   KxG(s)=KxG(s)+K(ispin,kspin,iorb,korb)%less(N,s)*conjg( G(kspin,jspin,korb,jorb)%ret(j,s) ) !adv <-- ret
                enddo
             enddo
          enddo
          Gmat(io,jo) = Gmat(io,jo) + dt*kb_trapz(KxG(0:),1,j)
          !
          KxG(0:)=zero
          do s=1,N-1
             do kspin=1,Nspin
                do korb=1,Norb
                   KxG(s)=KxG(s)+K(ispin,kspin,iorb,korb)%ret(N,s)*G(kspin,jspin,korb,jorb)%less(s,j)
                enddo
             enddo
          enddo
          Gmat(io,jo) = Gmat(io,jo) + dt*kb_half_trapz(KxG(0:),1,N-1)
          Amat(io,jo) = Amat(io,jo) - 0.5d0*dt*K(ispin,jspin,iorb,jorb)%ret(N,N)           
       enddo
       call inv(Amat)
       Gmat = matmul(Amat,Gmat)
       do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb
          jo = jorb + (jspin-1)*Norb
          G(ispin,jspin,iorb,jorb)%less(N,j) = Gmat(io,jo)
       enddo
    enddo
    !
    ! G^<(t_{i},t_{N}) <= Hermite conjugate
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    do i=1,N-1     
       forall(ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)&
            G(ispin,jspin,iorb,jorb)%less(i,N) = -conjg(G(jspin,ispin,jorb,iorb)%less(N,i))
    end do
    !
    ! G^{<}(t_{n},t_{n}) <= Diagonal
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    Amat=zeye(Nso)
    do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
       io = iorb + (ispin-1)*Norb
       jo = jorb + (jspin-1)*Norb
       Gmat(io,jo) = Q(ispin,jspin,iorb,jorb)%less(N,N)
       KxG(0:)=zero
       do s=0,L
          do kspin=1,Nspin
             do korb=1,Norb
                KxG(s)=KxG(s)+K(ispin,kspin,iorb,korb)%lmix(N,s)*conjg( G(kspin,jspin,korb,jorb)%lmix(N,L-s) ) !rmix <-- lmix
             enddo
          enddo
       enddo
       Gmat(io,jo) = Gmat(io,jo) - xi*dtau*kb_trapz(KxG(0:),0,L)
       !
       KxG(0:)=zero
       do s=1,N
          do kspin=1,Nspin
             do korb=1,Norb
                KxG(s)=KxG(s)+K(ispin,kspin,iorb,korb)%less(N,s)*conjg( G(kspin,jspin,korb,jorb)%ret(N,s) ) !adv <-- ret
             enddo
          enddo
       enddo
       Gmat(io,jo) = Gmat(io,jo) + dt*kb_trapz(KxG(0:),1,N)
       !
       KxG(0:)=zero
       do s=1,N-1
          do kspin=1,Nspin
             do korb=1,Norb
                KxG(s)=KxG(s)+K(ispin,kspin,iorb,korb)%ret(N,s)*G(kspin,jspin,korb,jorb)%less(s,N)
             enddo
          enddo
       enddo
       Gmat(io,jo) = Gmat(io,jo) + dt*kb_half_trapz(KxG(0:),1,N-1)
       Amat(io,jo) = Amat(io,jo) - 0.5d0*dt*K(ispin,jspin,iorb,jorb)%ret(N,N)
    enddo
    call inv(Amat)
    Gmat = matmul(Amat,Gmat)
    do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
       io = iorb + (ispin-1)*Norb
       jo = jorb + (jspin-1)*Norb
       G(ispin,jspin,iorb,jorb)%less(N,N) = Gmat(io,jo)
    enddo
    !
    !
    deallocate(KxG,Amat,Gmat)
  end subroutine vie_kb_gf_Rank4





  subroutine vie_kb_gf_rank6(G,K,Q,notail)
    type(kb_gf), intent(inout)            :: G(:,:,:,:,:,:)
    type(kb_gf), intent(in)               :: K(:,:,:,:,:,:)
    type(kb_gf), intent(in)               :: Q(:,:,:,:,:,:)
    logical,optional                      :: notail
    logical                               :: notail_
    integer                               :: N,L,Niw
    real(8)                               :: dt,dtau
    integer                               :: Nlat,Nspin,Norb,Nlso,i,j,s,itau,jtau,io,jo,ko
    real(8),dimension(:),allocatable      :: ftau
    complex(8),dimension(:),allocatable   :: KxG
    complex(8),dimension(:,:),allocatable :: Amat,Gmat
    !
    notail_=.true.;if(present(notail))notail_=notail
    !
    !
    N   = cc_params%Nt                 !<== work with the ACTUAL size of the contour
    L   = cc_params%Ntau
    Niw = cc_params%Niw
    dt  = cc_params%dt
    dtau= cc_params%dtau
    !
    Nlat = size(G,1) ; Nspin = size(G,3) ; Norb = size(G,5) ; Nlso=Nlat*Nspin*Norb
    call assert_shape_kb_gf(G,[Nlat,Nlat,Nspin,Nspin,Norb,Norb],"vie_kb_gf_Rank6","G")
    call assert_shape_kb_gf(K,[Nlat,Nlat,Nspin,Nspin,Norb,Norb],"vie_kb_gf_Rank6","K")
    call assert_shape_kb_gf(Q,[Nlat,Nlat,Nspin,Nspin,Norb,Norb],"vie_kb_gf_Rank6","Q")
    !
    allocate(KxG(0:max(N,L)),Amat(Nlso,Nlso),Gmat(Nlso,Nlso))
    !
    if(N==1)then
       allocate(ftau(0:Niw))
       !Mats component: [1d0 + K(iw)].G(iw) = Q(iw)
       do i=1,Niw
          do concurrent(ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
             io = iorb + (ispin-1)*Norb + (ilat-1)*Nspin*Norb
             jo = jorb + (jspin-1)*Norb + (ilat-1)*Nspin*Norb
             Amat(io,jo) = Q(ilat,jlat,ispin,jspin,iorb,jorb)%iw(i)
             Gmat(io,jo) = zeye(io,jo) + K(ilat,jlat,ispin,jspin,iorb,jorb)%iw(i)
          enddo
          call inv(Gmat)
          Gmat = matmul(Gmat,Amat)
          do concurrent(ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
             io = iorb + (ispin-1)*Norb + (ilat-1)*Nspin*Norb
             jo = jorb + (jspin-1)*Norb + (ilat-1)*Nspin*Norb
             G(ilat,jlat,ispin,jspin,iorb,jorb)%iw(i) = Gmat(io,jo)
          enddo
       enddo
       !
       do ispin=1,Nspin
          do jspin=1,Nspin
             do iorb=1,Norb
                do jorb=1,Norb
                   call fft_iw2tau(G(ilat,jlat,ispin,jspin,iorb,jorb)%iw,ftau(0:),cc_params%beta,notail=notail_)
                   call fft_extract_gtau(ftau(0:),G(ilat,jlat,ispin,jspin,iorb,jorb)%mats(0:))
                enddo
             enddo
          enddo
       enddo
       !
       ! Ret component
       ! G^R(t,t') = Q^R(t,t')
       ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       !TIP   t_{N}, t`_{N}
       do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          G(ilat,jlat,ispin,jspin,iorb,jorb)%ret(1,1)=Q(ilat,jlat,ispin,jspin,iorb,jorb)%ret(1,1)
       enddo
       !
       ! Lmix component
       ! G_ab^\lmix(t,tau') = Q_ab^\lmix(t,tau') + \int_0^\beta K_ak^\lmix(t,s)*G_kb^M(s,tau')ds
       ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       do jtau=0,L
          do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
             G(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(1,jtau) = Q(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(1,jtau)
             KxG(0:)=zero           
             do s=0,jtau
                do kspin=1,Nspin
                   do korb=1,Norb
                      KxG(s)=KxG(s)+K(ilat,klat,ispin,kspin,iorb,korb)%lmix(1,s)*G(klat,jlat,kspin,jspin,korb,jorb)%mats(s+L-jtau)
                   enddo
                enddo
             enddo
             G(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(1,jtau)=G(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(1,jtau) - dtau*kb_trapz(KxG(0:),0,jtau)
             KxG(0:)=zero
             do s=jtau,L
                do kspin=1,Nspin
                   do korb=1,Norb
                      KxG(s)=KxG(s)+K(ilat,klat,ispin,kspin,iorb,korb)%lmix(1,s)*G(klat,jlat,kspin,jspin,korb,jorb)%mats(s-jtau)
                   enddo
                enddo
             enddo
             G(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(1,jtau)=G(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(1,jtau) + dtau*kb_trapz(KxG(0:),jtau,L)
          enddo
       enddo
       !
       ! Less component
       ! G_ab^<(t,t')  = Q_ab^<(t,t') - i\int_0^\beta K_ak^\lmix(t,s)*G_kb^\rmix(s,t')ds
       ! G^{<}(t_{n},t_{n}) <= Diagonal
       ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
       do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          G(ilat,jlat,ispin,jspin,iorb,jorb)%less(1,1) = Q(ilat,jlat,ispin,jspin,iorb,jorb)%less(1,1)
          KxG(0:)=zero
          do s=0,L
             do kspin=1,Nspin
                do korb=1,Norb
                   KxG(s)=KxG(s)+K(ilat,klat,ispin,kspin,iorb,korb)%lmix(1,s)*conjg( G(klat,jlat,kspin,jspin,korb,jorb)%lmix(1,L-s) ) !rmix <-- lmix
                enddo
             enddo
          enddo
          G(ilat,jlat,ispin,jspin,iorb,jorb)%less(1,1) = G(ilat,jlat,ispin,jspin,iorb,jorb)%less(1,1) - xi*dtau*kb_trapz(KxG(0:),0,L)
       enddo
       !
       deallocate(ftau)
       !
       return
       !
    endif
    !
    !
    !
    ! Ret component
    ! G^R(t,t') - \int_{t'}^t K^R(t,s)*G^R(s,t')ds = Q^R(t,t')
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !TIP   t_{N}, t`_{N}
    do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
       G(ilat,jlat,ispin,jspin,iorb,jorb)%ret(N,N)=Q(ilat,jlat,ispin,jspin,iorb,jorb)%ret(N,N)
    enddo
    !
    !VERTICAL INTERVAL  t_{N}, t`_{j, j=1,...,N-1}
    do j=1,N-1
       Amat=zeye(Nso)
       do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb + (ilat-1)*Nspin*Norb
          jo = jorb + (jspin-1)*Norb + (ilat-1)*Nspin*Norb
          Gmat(io,jo) = Q(ilat,jlat,ispin,jspin,iorb,jorb)%ret(N,j)
          KxG(0:)=zero
          do s=j,N-1
             do kspin=1,Nspin
                do korb=1,Norb
                   KxG(s)=KxG(s) + K(ilat,klat,ispin,kspin,iorb,korb)%ret(N,s)*G(klat,jlat,kspin,jspin,korb,jorb)%ret(s,j)
                enddo
             enddo
          enddo
          Gmat(io,jo) = Gmat(io,jo) + dt*kb_half_trapz(KxG(0:),j,N-1)
          Amat(io,jo) = Amat(io,jo) - 0.5d0*dt*K(ilat,jlat,ispin,jspin,iorb,jorb)%ret(N,N) !Check this line
       enddo
       call inv(Amat)
       Gmat = matmul(Amat,Gmat) !check this line
       do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          G(ilat,jlat,ispin,jspin,iorb,jorb)%ret(N,j) = Gmat(io,jo)
       enddo
    end do
    !
    !
    ! Lmix component
    ! G^\lmix(t,tau') - \int_0^t K^R(t,s)*G^\lmix(s,tau')ds
    !    = Q^\lmix(t,tau') + \int_0^\beta K^\lmix(t,s)*G^M(s,tau')ds
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    do jtau=0,L
       Amat=zeye(Nso)
       do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb + (ilat-1)*Nspin*Norb
          jo = jorb + (jspin-1)*Norb + (ilat-1)*Nspin*Norb
          Gmat(io,jo) = Q(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(N,jtau)
          KxG(0:)=zero           
          do s=0,jtau
             do kspin=1,Nspin
                do korb=1,Norb
                   KxG(s)=KxG(s)+K(ilat,klat,ispin,kspin,iorb,korb)%lmix(N,s)*G(klat,jlat,kspin,jspin,korb,jorb)%mats(s+L-jtau)
                enddo
             enddo
          enddo
          Gmat(io,jo)=Gmat(io,jo) - dtau*kb_trapz(KxG(0:),0,jtau)
          !
          KxG(0:)=zero
          do s=jtau,L
             do kspin=1,Nspin
                do korb=1,Norb
                   KxG(s)=KxG(s)+K(ilat,klat,ispin,kspin,iorb,korb)%lmix(N,s)*G(klat,jlat,kspin,jspin,korb,jorb)%mats(s-jtau)
                enddo
             enddo
          enddo
          Gmat(io,jo)=Gmat(io,jo) + dtau*kb_trapz(KxG(0:),jtau,L)
          !
          KxG(0:)=zero
          do s=1,N-1
             do kspin=1,Nspin
                do korb=1,Norb
                   KxG(s)=KxG(s)+K(ilat,klat,ispin,kspin,iorb,korb)%ret(N,s)*G(klat,jlat,kspin,jspin,korb,jorb)%lmix(s,jtau)
                enddo
             enddo
          enddo
          Gmat(io,jo) = Gmat(io,jo) + dt*kb_half_trapz(KxG(0:),1,N-1)
          Amat(io,jo) = Amat(io,jo) - 0.5d0*dt*K(ilat,jlat,ispin,jspin,iorb,jorb)%ret(N,N)
       enddo
       call inv(Amat)
       Gmat = matmul(Amat,Gmat) !check this line
       do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb + (ilat-1)*Nspin*Norb
          jo = jorb + (jspin-1)*Norb + (ilat-1)*Nspin*Norb
          G(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(N,jtau) = Gmat(io,jo)
       end do
    end do
    !
    !
    ! Less component
    ! G^<(t,t') - \int_0^t K^{R}(t,s)*G^{<}(s,t')ds
    !    = Q^<(t,t') - i\int_0^\beta K^\lmix(t,s)*G^\rmix(s,t')ds
    !      + \int_0^{t'} K^<(t,s)*G^A(s,t')ds
    ! G^<(t_{N},t_{j}), j=1,...,N-1
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    do j=1,N-1
       Amat=zeye(Nso)
       do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          Gmat(io,jo) = Q(ilat,jlat,ispin,jspin,iorb,jorb)%less(N,j)
          KxG(0:)=zero
          do s=0,L
             do kspin=1,Nspin
                do korb=1,Norb
                   KxG(s)=KxG(s)+K(ilat,klat,ispin,kspin,iorb,korb)%lmix(N,s)*conjg( G(klat,jlat,kspin,jspin,korb,jorb)%lmix(j,L-s) ) !rmix <-- lmix
                enddo
             enddo
          enddo
          Gmat(io,jo) = Gmat(io,jo) - xi*dtau*kb_trapz(KxG(0:),0,L)
          !
          KxG(0:)=zero
          do s=1,j
             do kspin=1,Nspin
                do korb=1,Norb
                   KxG(s)=KxG(s)+K(ilat,klat,ispin,kspin,iorb,korb)%less(N,s)*conjg( G(klat,jlat,kspin,jspin,korb,jorb)%ret(j,s) ) !adv <-- ret
                enddo
             enddo
          enddo
          Gmat(io,jo) = Gmat(io,jo) + dt*kb_trapz(KxG(0:),1,j)
          !
          KxG(0:)=zero
          do s=1,N-1
             do kspin=1,Nspin
                do korb=1,Norb
                   KxG(s)=KxG(s)+K(ilat,klat,ispin,kspin,iorb,korb)%ret(N,s)*G(klat,jlat,kspin,jspin,korb,jorb)%less(s,j)
                enddo
             enddo
          enddo
          Gmat(io,jo) = Gmat(io,jo) + dt*kb_half_trapz(KxG(0:),1,N-1)
          Amat(io,jo) = Amat(io,jo) - 0.5d0*dt*K(ilat,jlat,ispin,jspin,iorb,jorb)%ret(N,N)           
       enddo
       call inv(Amat)
       Gmat = matmul(Amat,Gmat)
       do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb + (ilat-1)*Nspin*Norb
          jo = jorb + (jspin-1)*Norb + (ilat-1)*Nspin*Norb
          G(ilat,jlat,ispin,jspin,iorb,jorb)%less(N,j) = Gmat(io,jo)
       enddo
    enddo
    !
    ! G^<(t_{i},t_{N}) <= Hermite conjugate
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    do i=1,N-1     
       forall(ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)&
            G(ilat,jlat,ispin,jspin,iorb,jorb)%less(i,N) = -conjg(G(jlat,ilat,jspin,ispin,jorb,iorb)%less(N,i))
    end do
    !
    ! G^{<}(t_{n},t_{n}) <= Diagonal
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    Amat=zeye(Nso)
    do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
       io = iorb + (ispin-1)*Norb + (ilat-1)*Nspin*Norb
       jo = jorb + (jspin-1)*Norb + (ilat-1)*Nspin*Norb
       Gmat(io,jo) = Q(ilat,jlat,ispin,jspin,iorb,jorb)%less(N,N)
       KxG(0:)=zero
       do s=0,L
          do kspin=1,Nspin
             do korb=1,Norb
                KxG(s)=KxG(s)+K(ilat,klat,ispin,kspin,iorb,korb)%lmix(N,s)*conjg( G(klat,jlat,kspin,jspin,korb,jorb)%lmix(N,L-s) ) !rmix <-- lmix
             enddo
          enddo
       enddo
       Gmat(io,jo) = Gmat(io,jo) - xi*dtau*kb_trapz(KxG(0:),0,L)
       !
       KxG(0:)=zero
       do s=1,N
          do kspin=1,Nspin
             do korb=1,Norb
                KxG(s)=KxG(s)+K(ilat,klat,ispin,kspin,iorb,korb)%less(N,s)*conjg( G(klat,jlat,kspin,jspin,korb,jorb)%ret(N,s) ) !adv <-- ret
             enddo
          enddo
       enddo
       Gmat(io,jo) = Gmat(io,jo) + dt*kb_trapz(KxG(0:),1,N)
       !
       KxG(0:)=zero
       do s=1,N-1
          do kspin=1,Nspin
             do korb=1,Norb
                KxG(s)=KxG(s)+K(ilat,klat,ispin,kspin,iorb,korb)%ret(N,s)*G(klat,jlat,kspin,jspin,korb,jorb)%less(s,N)
             enddo
          enddo
       enddo
       Gmat(io,jo) = Gmat(io,jo) + dt*kb_half_trapz(KxG(0:),1,N-1)
       Amat(io,jo) = Amat(io,jo) - 0.5d0*dt*K(ilat,jlat,ispin,jspin,iorb,jorb)%ret(N,N)
    enddo
    call inv(Amat)
    Gmat = matmul(Amat,Gmat)
    do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
       io = iorb + (ispin-1)*Norb + (ilat-1)*Nspin*Norb
       jo = jorb + (jspin-1)*Norb + (ilat-1)*Nspin*Norb
       G(ilat,jlat,ispin,jspin,iorb,jorb)%less(N,N) = Gmat(io,jo)
    enddo
    !
    !
    deallocate(KxG,Amat,Gmat)
  end subroutine vie_kb_gf_rank6





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
  subroutine vie_kb_gf_d1(G,K,Q,notail)
    type(kb_gf),intent(inout) :: G(:)
    type(kb_gf),intent(in)    :: K(size(G))
    type(kb_gf),intent(in)    :: Q(size(G))
    logical,optional          :: notail
    logical                   :: notail_
    notail_=.true.;if(present(notail))notail_=notail
    do i1=1,size(G)
       call vie_kb_gf_Rank0(G(i1),K(i1),Q(i1),notail_)
    enddo
  end subroutine vie_kb_gf_d1


  subroutine vie_kb_gf_d2(G,K,Q,notail)
    type(kb_gf),intent(inout) :: G(:,:)
    type(kb_gf),intent(in)    :: K(:,:)
    type(kb_gf),intent(in)    :: Q(:,:)
    logical,optional          :: notail
    logical                   :: notail_
    integer                   :: Nso        
    notail_=.true.;if(present(notail))notail_=notail
    Nso = size(G,1)
    select case(Nso)
    case (1)
       call vie_kb_gf_rank0(G(1,1),K(1,1),Q(1,1),notail_)
    case default
       call vie_kb_gf_rank2(G,K,Q,notail_)
    end select
  end subroutine vie_kb_gf_d2

  subroutine vie_kb_gf_d3(G,K,Q,notail)
    type(kb_gf),intent(inout) :: G(:,:,:)
    type(kb_gf),intent(in)    :: K(:,:,:)
    type(kb_gf),intent(in)    :: Q(:,:,:)
    logical,optional          :: notail
    logical                   :: notail_
    integer                   :: Nk
    notail_=.true.;if(present(notail))notail_=notail
    Nk = size(G,3)
    if(Nk /= size(K,3)) stop "ERROR vie_kb_gf_d3: size(K,3) /= size(G,3)"
    if(Nk /= size(Q,3)) stop "ERROR vie_kb_gf_d3: size(Q,3) /= size(G,3)"
    do ik=1,Nk
       call vie_kb_gf_rank2(G(:,:,ik),K(:,:,ik),Q(:,:,ik),notail_)
    enddo
  end subroutine vie_kb_gf_d3


  subroutine vie_kb_gf_d4(G,K,Q,notail)
    type(kb_gf),intent(inout) :: G(:,:,:,:)
    type(kb_gf),intent(in)    :: K(:,:,:,:)
    type(kb_gf),intent(in)    :: Q(:,:,:,:)
    logical,optional          :: notail
    logical                   :: notail_
    integer                   :: Nspin,Norb,Nso
    integer                   :: ispin,jspin,iorb,jorb,io,jo
    notail_=.true.;if(present(notail))notail_=notail
    Nspin = size(G,1) ; Norb = size(G,3) ; Nso = Nspin*Norb
    call assert_shape_kb_gf(G,[Nspin,Nspin,Norb,Norb],"vie_kb_gf_d4","G")
    call assert_shape_kb_gf(K,[Nspin,Nspin,Norb,Norb],"vie_kb_gf_d4","K")
    call assert_shape_kb_gf(Q,[Nspin,Nspin,Norb,Norb],"vie_kb_gf_d4","Q")
    !
    select case(Nso)
    case (1)
       call vie_kb_gf_rank0(G(1,1,1,1),K(1,1,1,1),Q(1,1,1,1),notail_)
    case default
       call vie_kb_gf_rank4(G,K,Q,notail_)
    end select
    !
  end subroutine vie_kb_gf_d4


  subroutine vie_kb_gf_d5(G,K,Q,notail)
    type(kb_gf),intent(inout) :: G(:,:,:,:,:)
    type(kb_gf),intent(in)    :: K(:,:,:,:,:)
    type(kb_gf),intent(in)    :: Q(:,:,:,:,:)
    logical,optional          :: notail
    logical                   :: notail_
    notail_=.true.;if(present(notail))notail_=notail
    N1 = size(G,1)
    N2 = size(G,3)
    Nk = size(G,5)
    call assert_shape_kb_gf(G,[N1,N1,N2,N2,Nk],"vie_kb_gf_d5","G")
    call assert_shape_kb_gf(K,[N1,N1,N2,N2,Nk],"vie_kb_gf_d5","K")
    call assert_shape_kb_gf(Q,[N1,N1,N2,N2,Nk],"vie_kb_gf_d5","Q")
    do ik=1,Nk
       call vie_kb_gf_rank4(G(:,:,:,:,ik),K(:,:,:,:,ik),Q(:,:,:,:,ik),notail_)
    enddo
  end subroutine vie_kb_gf_d5


  subroutine vie_kb_gf_d6(G,K,Q,notail)
    type(kb_gf),intent(inout) :: G(:,:,:,:,:,:)
    type(kb_gf),intent(in)    :: K(:,:,:,:,:,:)
    type(kb_gf),intent(in)    :: Q(:,:,:,:,:,:)
    logical,optional          :: notail
    logical                   :: notail_
    integer                   :: Nlat,Nspin,Norb,Nlso
    integer                   :: ilat,jlat
    integer                   :: ispin,jspin
    integer                   :: iorb,jorb
    integer                   :: io,jo
    notail_=.true.;if(present(notail))notail_=notail
    Nlat = size(G,1) ; Nspin = size(G,3) ;  Norb = size(G,5) ; Nlso = Nlat*Nspin*Norb
    call assert_shape_kb_gf(G,[Nlat,Nlat,Nspin,Nspin,Norb,Norb],"vie_kb_gf_d6","G")
    call assert_shape_kb_gf(K,[Nlat,Nlat,Nspin,Nspin,Norb,Norb],"vie_kb_gf_d6","K")
    call assert_shape_kb_gf(Q,[Nlat,Nlat,Nspin,Nspin,Norb,Norb],"vie_kb_gf_d6","Q")
    !
    select case(Nlso)
    case (1)
       call vie_kb_gf_rank0(G(1,1,1,1,1,1),K(1,1,1,1,1,1),Q(1,1,1,1,1,1),notail_)
    case default
       call vie_kb_gf_rank6(G,K,Q,notail_)
    end select
    !
  end subroutine vie_kb_gf_d6


  subroutine vie_kb_gf_d7(G,K,Q,notail)
    type(kb_gf),intent(inout) :: G(:,:,:,:,:,:,:)
    type(kb_gf),intent(in)    :: K(:,:,:,:,:,:,:)
    type(kb_gf),intent(in)    :: Q(:,:,:,:,:,:,:)
    integer                   :: Ni,N1,N2,N3
    logical,optional         :: notail
    logical                   :: notail_
    notail_=.true.;if(present(notail))notail_=notail
    N1 = size(G,1)
    N2 = size(G,3)
    N3 = size(G,5)
    Nk = size(G,7)
    call assert_shape_kb_gf(G,[N1,N1,N2,N2,N3,N3,Nk],"vie_kb_gf_d7","G")
    call assert_shape_kb_gf(K,[N1,N1,N2,N2,N3,N3,Nk],"vie_kb_gf_d7","K")
    call assert_shape_kb_gf(Q,[N1,N1,N2,N2,N3,N3,Nk],"vie_kb_gf_d7","Q")
    do ik=1,Nk
       call vie_kb_gf_rank6(G(:,:,:,:,:,:,ik),K(:,:,:,:,:,:,ik),Q(:,:,:,:,:,:,ik),notail_)
    enddo
  end subroutine vie_kb_gf_d7




END MODULE KB_GF_VIE











