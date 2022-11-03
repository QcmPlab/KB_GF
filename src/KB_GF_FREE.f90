MODULE KB_GF_FREE
  USE KB_VARS_GLOBAL
  USE KB_CONTOUR
  USE KB_AUX
  USE KB_GF_COMMON
  USE SCIFOR, only: one,xi,zero,pi,zeye,inv,assert_shape
  implicit none
  private


  !FREE SOLVER:
  interface free_kb_gf
     module procedure :: free_kb_gf_rank0
     module procedure :: free_kb_gf_d1
     module procedure :: free_kb_gf_d2     
     module procedure :: free_kb_gf_d3
     module procedure :: free_kb_gf_d4
     module procedure :: free_kb_gf_d5
     module procedure :: free_kb_gf_d6
     module procedure :: free_kb_gf_d7
  end interface free_kb_gf

  public :: free_kb_gf



contains



  !----------------------------------------------------------------------------
  !  This subroutine solves the differential equation for Free systems:
  !              [i*d/dt-h(t)]G(t,t') = delta(t,t')
  !  for t=n*dt or t'=n*dt, using 2^nd implicit Runge-Kutta method.
  !
  ! TYPE: X=RET,LESS,LMIX (R,<,\LMIX)
  !i d/dt G^x(t,:)  = h(t)G^x(t,:) + Q^x(t,:)
  !----------------------------------------------------------------------------
  subroutine free_kb_gf_rank0(Hk,Gk,dGk,dGk_new)
    complex(8),dimension(:)          :: Hk
    type(kb_gf),intent(inout)        :: Gk
    type(kb_dgf),intent(inout)       :: dGk
    type(kb_dgf)                     :: dGk_new
    integer                          :: N,L,Niw
    real(8)                          :: dt,dtau
    integer                          :: i,j,itau,jtau,s
    real(8),dimension(:),allocatable :: ftau
    complex(8)                       :: dGk_less,A
    !
    N   = cc_params%Nt                 !<== work with the ACTUAL size of the contour
    L   = cc_params%Ntau
    Niw = cc_params%Niw
    dt  = cc_params%dt
    dtau= cc_params%dtau
    !
    call assert_shape(Hk,[cc_params%Ntime],"vide_kb_gf_rank0","Hk")
    !
    !Treat here the t=0,0 case: solve the matsubara part using FFT
    !INITIALIZE THE WEISS FIELD Gk^{x=M,<,R,\lmix} for N=1
    if(N==1)then
       Gk%iw = one/(xi*cc_params%wm(:) + xmu - Hk(1))
       allocate(ftau(0:Niw))
       call fft_iw2tau(Gk%iw,ftau(0:),cc_params%beta)
       call fft_extract_gtau(ftau(0:),Gk%mats(0:))
       Gk%less(1,1) = -xi*Gk%mats(L)
       Gk%ret(1,1)  = -xi
       Gk%lmix(1,0:L)=-xi*Gk%mats(L:0:-1)
       deallocate(ftau)
       !
       !neq_setup_dgf:
       !get d/dt G_k^R = -i H(k,0)G_k^R
       dGk_new%ret(1)  = -xi*Hk(1)*Gk%ret(1,1)
       !
       !get d/dt G_k^< = -i H(k,0)G_k^< 
       dGk_new%less(1) = -xi*Hk(1)*Gk%less(1,1)
       !
       !get d/dt G_k^\lmix = -xi*H(k,0)*G_k^\lmix
       dGk_new%lmix(0:)= -xi*Hk(1)*Gk%lmix(1,0:)
       !
       return
       !
    end if
    !
    !TYPE: X=RET,LESS,LMIX (R,<,\LMIX)
    !I D/DT Gk^X(T,:)  = Hk(T)Gk^X(T,:) + Q^X(T,:)
    !
    !Ret component
    ! d/dt Gk^R(t,:) = -i*h(t)*Gk^R(t,:) -i*delta(t,:)
    Gk%ret(N,N)=-xi
    dGk_new%ret(N)=-xi*Hk(N)*Gk%ret(N,N)
    do j=1,N-1
       Gk%ret(N,j)=Gk%ret(N-1,j) + 0.5d0*dt*dGk%ret(j)
       Gk%ret(N,j)=Gk%ret(N,j)/(1.d0 + 0.5d0*xi*dt*Hk(N))
       dGk_new%ret(j)= -xi*Hk(N)*Gk%ret(N,j)
    enddo
    !
    !Lmix component
    !d/dt Gk^\lmix(t,:) = -i*Hk(t)*Gk^\lmix(t,:)
    do jtau=0,L
       Gk%lmix(N,jtau)=Gk%lmix(N-1,jtau)+0.5d0*dt*dGk%lmix(jtau)
       Gk%lmix(N,jtau)=Gk%lmix(N,jtau)/(1.d0 + 0.5d0*xi*dt*Hk(N))
       dGk_new%lmix(jtau)=-xi*Hk(N)*Gk%lmix(N,jtau)
    end do
    !
    !Less component
    !d/dt Gk^<(t,:) = -i*Hk(t)*Gk^<(t,:) 
    !
    ! Gk^<(t_{N},t_{j}), d/dt Gk^<(t_{N},t_{j}) <== lower-right triangle
    do j=1,N-1
       Gk%less(N,j)=Gk%less(N-1,j) + 0.5d0*dt*dGk%less(j)
       Gk%less(N,j)=Gk%less(N,j)/(1.d0 + 0.5d0*xi*dt*Hk(N))
       dGk_new%less(j)=-xi*Hk(N)*Gk%less(N,j)
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
    !
    !Gk^<(N,N), d/dt Gk^<(N,N)
    !d/dt Gk <== d/dt Gk_new
    Gk%less(N,N)=Gk%less(N-1,N)+0.5d0*dt*dGk_less
    Gk%less(N,N)=Gk%less(N,N)/(1.d0+0.5d0*xi*dt*Hk(N))
    !
    dGk_new%less(N)=-xi*Hk(N)*Gk%less(N,N)
    !
  end subroutine free_kb_gf_rank0




  subroutine free_kb_gf_rank2(Hk,Gk,dGk,dGk_new)
    complex(8),dimension(:,:,:)           :: Hk          ![Nso][Nso][Nt]
    type(kb_gf),intent(inout)             :: Gk(:,:)     !
    type(kb_dgf),intent(inout)            :: dGk(:,:)    !
    type(kb_dgf)                          :: dGk_new(:,:)!
    !
    integer                               :: N,L,Nso,Niw
    real(8)                               :: dt,dtau
    integer                               :: i,j,itau,jtau,s
    integer                               :: io,jo,ko
    complex(8)                            :: HxGk
    complex(8),dimension(:),allocatable   :: KxGk
    real(8),dimension(:),allocatable      :: ftau
    complex(8),dimension(:,:),allocatable :: Amat,AxGkmat,Gkmat,dGkless
    !
    N   = cc_params%Nt                 !<== work with the ACTUAL size of the contour
    L   = cc_params%Ntau
    Niw = cc_params%Niw
    dt  = cc_params%dt
    dtau= cc_params%dtau
    Nso = size(Hk,1)            !shape was asserted before entering
    !
    !
    allocate(KxGk(0:max(N,L)))
    allocate(Amat(Nso,Nso))
    allocate(Gkmat(Nso,Nso))
    allocate(AxGkmat(Nso,Nso))
    allocate(dGkless(Nso,Nso))
    Amat   =zero
    Gkmat  =zero
    AxGkmat=zero
    dGkless=zero
    !
    !Treat here the t=0,0 case:
    !We solve the matsubara part using FFT
    if(N==1)then
       !INITIALIZE THE WEISS FIELD Gk^{x=M,<,R,\lmix} for N=1
       !neq_setup_contour_gf:
       do i=1,Niw
          Gkmat = (xi*cc_params%wm(i)+xmu)*zeye(Nso) - Hk(:,:,1)
          call inv(Gkmat)
          forall(io=1:Nso,jo=1:Nso)Gk(io,jo)%iw(i) = Gkmat(io,jo)
       enddo
       !
       allocate(ftau(0:Niw))
       do io=1,Nso
          do jo=1,Nso
             call fft_iw2tau(Gk(io,jo)%iw,ftau(0:),cc_params%beta,notail=(io/=jo))
             call fft_extract_gtau(ftau(0:),Gk(io,jo)%mats(0:))
             Gk(io,jo)%less(1,1)  = -xi*Gk(io,jo)%mats(L)
             Gk(io,jo)%ret(1,1)   = zero
             Gk(io,jo)%lmix(1,0:L)= -xi*Gk(io,jo)%mats(L:0:-1)
          enddo
          Gk(io,io)%ret(1,1)  = -xi
       enddo
       deallocate(ftau)
       !
       !neq_setup_contour_dgf:
       do concurrent (io=1:Nso,jo=1:Nso)
          !get d/dt G_k^R = -i H(k,0)G_k^R
          HxGk=zero
          do ko=1,Nso
             HxGk = HxGk + Hk(io,ko,1)*Gk(ko,jo)%ret(1,1)
          enddo
          dGk_new(io,jo)%ret(1) = - xi*HxGk
          !
          !get d/dt G_k^< = -i H(k,0)G_k^<
          HxGk=zero
          do ko=1,Nso
             HxGk = HxGk + Hk(io,ko,1)*Gk(ko,jo)%less(1,1)
          enddo
          dGk_new(io,jo)%less(1) = - xi*HxGk
          !
          !get d/dt G_k^\lmix = -xi*H(k,0)*G_k^\lmix
          KxGk(0:)=zero
          do ko=1,Nso
             KxGk(0:) = KxGk(0:) + Hk(io,ko,1)*Gk(ko,jo)%lmix(1,0:)
          enddo
          dGk_new(io,jo)%lmix(0:)= -xi*KxGk(0:)
       enddo
       !
       return
       !
    end if
    !
    !
    Amat  = zeye(Nso) + 0.5d0*xi*dt*Hk(:,:,N)
    call inv(Amat)
    !
    !TYPE: X=RET,LESS,LMIX (R,<,\LMIX)
    !I D/DT Gk^X(T,:)  = Hk(T)Gk^X(T,:) + Q^X(T,:)
    !
    !Ret component
    !d/dt Gk^R(t,:) = -i*h(t)*Gk^R(t,:) -i*delta(t,:) 
    !TIP t_{N}, t`_{N}
    AxGkmat = -xi*zeye(Nso)
    do concurrent (io=1:Nso,jo=1:Nso)
       Gk(io,jo)%ret(N,N)    = AxGkmat(io,jo)
       dGk_new(io,jo)%ret(N) = -xi*sum(Hk(io,:,N)*AxGkmat(:,jo))
    end do
    !
    !VERTICAL INTERVAL t_{N}, t'_{j=1,...,N-1}
    do j=1,N-1
       do concurrent(io=1:Nso,jo=1:Nso)
          Gkmat(io,jo) = Gk(io,jo)%ret(N-1,j) + 0.5d0*dt*dGk(io,jo)%ret(j)
       enddo
       AxGkmat = matmul(Amat,Gkmat)
       do concurrent(io=1:Nso,jo=1:Nso)
          Gk(io,jo)%ret(N,j)    = AxGkmat(io,jo)
          dGk_new(io,jo)%ret(j) = -xi*sum(Hk(io,:,N)*AxGkmat(:,jo))
       end do
    enddo
    !
    !
    !Lmix component
    !d/dt Gk^\lmix(t,:) = -i*Hk(t)*Gk^\lmix(t,:)
    do jtau=0,L
       do concurrent(io=1:Nso,jo=1:Nso)
          Gkmat(io,jo) = Gk(io,jo)%lmix(N-1,jtau) + 0.5d0*dt*dGk(io,jo)%lmix(jtau)
       end do
       AxGkmat = matmul(Amat,Gkmat)
       do concurrent(io=1:Nso,jo=1:Nso)
          Gk(io,jo)%lmix(N,jtau)    = AxGkmat(io,jo)
          dGk_new(io,jo)%lmix(jtau) = -xi*sum(Hk(io,:,N)*AxGkmat(:,jo))
       end do
    enddo
    !
    !Less component
    !d/dt Gk^<(t,:) = -i*Hk(t)*Gk^<(t,:)
    ! Gk^<(t_{N},t_{j}), d/dt Gk^<(t_{N},t_{j}) <== lower-right triangle
    tp_less: do j=1,N-1
       do concurrent(io=1:Nso,jo=1:Nso)
          Gkmat(io,jo) = Gk(io,jo)%less(N-1,j) + 0.5d0*dt*dGk(io,jo)%less(j)
       end do
       AxGkmat = matmul(Amat,Gkmat)
       do concurrent(io=1:Nso,jo=1:Nso)
          Gk(io,jo)%less(N,j)    = AxGkmat(io,jo)
          dGk_new(io,jo)%less(j) = -xi*sum(Hk(io,:,N)*AxGkmat(:,jo))
       end do
    enddo tp_less
    !
    ! Gk^<(t_{i},t_{N}), d/dt Gk^<(t_{i},t_{N}) <== upper left triangle
    do concurrent (i=1:N-1,io=1:Nso,jo=1:Nso)
       Gk(io,jo)%less(i,N)=-conjg(Gk(jo,io)%less(N,i))
    end do
    !
    ! d/dt Gk^<(t_{N-1},t_{N})
    do io=1,Nso
       do jo=1,Nso
          dGkless(io,jo)=zero
          do ko=1,Nso
             dGkless(io,jo) = dGkless(io,jo)-xi*Hk(io,ko,N-1)*Gk(ko,jo)%less(N-1,N)
          enddo
       enddo
    enddo
    !
    !Gk^<(N,N), d/dt Gk^<(N,N)
    do concurrent(io=1:Nso,jo=1:Nso)
       Gkmat(io,jo) = Gk(io,jo)%less(N-1,N)+0.5d0*dt*dGkless(io,jo)
    end do
    AxGkmat=matmul(Amat,Gkmat)
    do concurrent(io=1:Nso,jo=1:Nso)
       Gk(io,jo)%less(N,N)    = AxGkmat(io,jo)
       dGk_new(io,jo)%less(N) = -xi*sum(Hk(io,:,N)*AxGkmat(:,jo))
    end do
    !
    deallocate(KxGk,Amat,Gkmat,AxGkmat,dGkless)
    !
  end subroutine free_kb_gf_rank2



  subroutine free_kb_gf_rank4(Hk,Gk,dGk,dGk_new)
    complex(8),dimension(:,:,:)           :: Hk          ![Nso][Nso][Nt]
    type(kb_gf),intent(inout)             :: Gk(:,:,:,:)     ![Nspin][Nspin][Norb][Norb]
    type(kb_dgf),intent(inout)            :: dGk(:,:,:,:)    !as Gk
    type(kb_dgf)                          :: dGk_new(:,:,:,:)!as Gk
    !
    integer                               :: N,L,Nso,Niw
    real(8)                               :: dt,dtau
    integer                               :: i,j,itau,jtau,s
    integer                               :: io,jo,ko
    complex(8)                            :: HxGk
    complex(8),dimension(:),allocatable   :: KxGk
    real(8),dimension(:),allocatable      :: ftau
    complex(8),dimension(:,:),allocatable :: Amat,AxGkmat,Gkmat,dGkless
    !
    N   = cc_params%Nt                 !<== work with the ACTUAL size of the contour
    L   = cc_params%Ntau
    Niw = cc_params%Niw
    dt  = cc_params%dt
    dtau= cc_params%dtau
    Nspin = size(Gk,1) ; Norb = size(Gk,3) ; Nso=Nspin*Norb
    call assert_shape(Hk,[Nso,Nso,cc_params%Ntime],"dyson_kb_gf_rank0","Hk")
    call assert_shape_kb_gf(Gk,[Nspin,Nspin,Norb,Norb],"dyson_kb_gf_Rank4","Gk")
    call assert_shape_kb_gf(dGk,[Nspin,Nspin,Norb,Norb],"dyson_kb_gf_Rank4","dGk")
    call assert_shape_kb_gf(dGk_new,[Nspin,Nspin,Norb,Norb],"dyson_kb_gf_Rank4","dGk_new")
    !
    !
    allocate(KxGk(0:max(N,L)))
    allocate(Amat(Nso,Nso))
    allocate(Gkmat(Nso,Nso))
    allocate(AxGkmat(Nso,Nso))
    allocate(dGkless(Nso,Nso))
    Amat   =zero
    Gkmat  =zero
    AxGkmat=zero
    dGkless=zero
    !
    !Treat here the t=0,0 case:
    !We solve the matsubara part using FFT
    if(N==1)then
       !INITIALIZE THE WEISS FIELD Gk^{x=M,<,R,\lmix} for N=1
       !neq_setup_contour_gf:
       do i=1,Niw
          Gkmat = (xi*cc_params%wm(i)+xmu)*zeye(Nso) - Hk(:,:,1)
          call inv(Gkmat)
          do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
             io = iorb + (ispin-1)*Norb
             jo = jorb + (jspin-1)*Norb
             Gk(ispin,jspin,iorb,jorb)%iw(i) = Gkmat(io,jo)
          enddo
       enddo
       !
       allocate(ftau(0:Niw))
       do ispin=1,Nspin
          do iorb=1,Norb
             do jspin=1,Nspin
                do jorb=1,Norb
                   call fft_iw2tau(Gk(ispin,jspin,iorb,jorb)%iw,ftau(0:),cc_params%beta,notail=(io/=jo))
                   call fft_extract_gtau(ftau(0:),Gk(ispin,jspin,iorb,jorb)%mats(0:))
                   Gk(ispin,jspin,iorb,jorb)%less(1,1)  = -xi*Gk(ispin,jspin,iorb,jorb)%mats(L)
                   Gk(ispin,jspin,iorb,jorb)%ret(1,1)   = zero
                   Gk(ispin,jspin,iorb,jorb)%lmix(1,0:L)= -xi*Gk(ispin,jspin,iorb,jorb)%mats(L:0:-1)
                enddo
             enddo
             Gk(ispin,ispin,iorb,iorb)%ret(1,1)  = -xi
          enddo
       enddo
       deallocate(ftau)
       !
       !neq_setup_contour_dgf:
       do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb
          jo = jorb + (jspin-1)*Norb
          !get d/dt G_k^R = -i H(k,0)G_k^R
          HxGk=zero
          do kspin=1,Nspin
             do korb=1,Norb
                io = iorb + (ispin-1)*Norb
                ko = korb + (kspin-1)*Norb              
                HxGk = HxGk + Hk(io,ko,1)*Gk(kspin,jspin,korb,jorb)%ret(1,1)
             enddo
          enddo
          dGk_new(ispin,jspin,iorb,jorb)%ret(1) = - xi*HxGk
          !
          !get d/dt G_k^< = -i H(k,0)G_k^<
          HxGk=zero
          do kspin=1,Nspin
             do korb=1,Norb
                io = iorb + (ispin-1)*Norb
                ko = korb + (kspin-1)*Norb
                HxGk = HxGk + Hk(io,ko,1)*Gk(kspin,jspin,korb,jorb)%less(1,1)
             enddo
          enddo
          dGk_new(ispin,jspin,iorb,jorb)%less(1) = - xi*HxGk
          !
          !get d/dt G_k^\lmix = -xi*H(k,0)*G_k^\lmix
          KxGk(0:)=zero
          do kspin=1,Nspin
             do korb=1,Norb
                io = iorb + (ispin-1)*Norb
                ko = korb + (kspin-1)*Norb
                KxGk(0:) = KxGk(0:) + Hk(io,ko,1)*Gk(kspin,jspin,korb,jorb)%lmix(1,0:)
             enddo
          enddo
          dGk_new(ispin,jspin,iorb,jorb)%lmix(0:)= -xi*KxGk(0:)
       enddo
       !
       return
       !
    end if
    !
    !
    Amat  = zeye(Nso) + 0.5d0*xi*dt*Hk(:,:,N)
    call inv(Amat)
    !
    !TYPE: X=RET,LESS,LMIX (R,<,\LMIX)
    !I D/DT Gk^X(T,:)  = Hk(T)Gk^X(T,:) + Q^X(T,:)
    !
    !Ret component
    !d/dt Gk^R(t,:) = -i*h(t)*Gk^R(t,:) -i*delta(t,:) 
    !TIP t_{N}, t`_{N}
    AxGkmat = -xi*zeye(Nso)
    do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
       io = iorb + (ispin-1)*Norb
       jo = jorb + (jspin-1)*Norb
       Gk(ispin,jspin,iorb,jorb)%ret(N,N)    = AxGkmat(io,jo)
       dGk_new(ispin,jspin,iorb,jorb)%ret(N) = -xi*sum(Hk(io,:,N)*AxGkmat(:,jo))
    end do
    !
    !VERTICAL INTERVAL t_{N}, t'_{j=1,...,N-1}
    do j=1,N-1
       do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb
          jo = jorb + (jspin-1)*Norb
          Gkmat(io,jo) = Gk(ispin,jspin,iorb,jorb)%ret(N-1,j) + 0.5d0*dt*dGk(ispin,jspin,iorb,jorb)%ret(j)
       enddo
       AxGkmat = matmul(Amat,Gkmat)
       do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb
          jo = jorb + (jspin-1)*Norb
          Gk(ispin,jspin,iorb,jorb)%ret(N,j)    = AxGkmat(io,jo)
          dGk_new(ispin,jspin,iorb,jorb)%ret(j) = -xi*sum(Hk(io,:,N)*AxGkmat(:,jo))
       end do
    enddo
    !
    !
    !Lmix component
    !d/dt Gk^\lmix(t,:) = -i*Hk(t)*Gk^\lmix(t,:)
    do jtau=0,L
       do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb
          jo = jorb + (jspin-1)*Norb
          Gkmat(io,jo) = Gk(ispin,jspin,iorb,jorb)%lmix(N-1,jtau) + 0.5d0*dt*dGk(ispin,jspin,iorb,jorb)%lmix(jtau)
       end do
       AxGkmat = matmul(Amat,Gkmat)
       do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb
          jo = jorb + (jspin-1)*Norb
          Gk(ispin,jspin,iorb,jorb)%lmix(N,jtau)    = AxGkmat(io,jo)
          dGk_new(ispin,jspin,iorb,jorb)%lmix(jtau) = -xi*sum(Hk(io,:,N)*AxGkmat(:,jo))
       end do
    enddo
    !
    !Less component
    !d/dt Gk^<(t,:) = -i*Hk(t)*Gk^<(t,:)
    ! Gk^<(t_{N},t_{j}), d/dt Gk^<(t_{N},t_{j}) <== lower-right triangle
    tp_less: do j=1,N-1
       do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb
          jo = jorb + (jspin-1)*Norb
          Gkmat(io,jo) = Gk(ispin,jspin,iorb,jorb)%less(N-1,j) + 0.5d0*dt*dGk(ispin,jspin,iorb,jorb)%less(j)
       end do
       AxGkmat = matmul(Amat,Gkmat)
       do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb
          jo = jorb + (jspin-1)*Norb
          Gk(ispin,jspin,iorb,jorb)%less(N,j)    = AxGkmat(io,jo)
          dGk_new(ispin,jspin,iorb,jorb)%less(j) = -xi*sum(Hk(io,:,N)*AxGkmat(:,jo))
       end do
    enddo tp_less
    !
    ! Gk^<(t_{i},t_{N}), d/dt Gk^<(t_{i},t_{N}) <== upper left triangle
    do concurrent (i=1:N-1,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
       Gk(ispin,jspin,iorb,jorb)%less(i,N)=-conjg(Gk(jspin,ispin,jorb,iorb)%less(N,i))
    end do
    !
    ! d/dt Gk^<(t_{N-1},t_{N})
    do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
       io = iorb + (ispin-1)*Norb
       jo = jorb + (jspin-1)*Norb
       dGkless(io,jo)=zero
       do kspin=1,Nspin
          do korb=1,Norb
             ko = korb + (kspin-1)*Norb
             dGkless(io,jo) = dGkless(io,jo)-xi*Hk(io,ko,N-1)*Gk(kspin,jspin,korb,jorb)%less(N-1,N)
          enddo
       enddo
    enddo
    !
    !Gk^<(N,N), d/dt Gk^<(N,N)
    do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
       io = iorb + (ispin-1)*Norb
       jo = jorb + (jspin-1)*Norb
       Gkmat(io,jo) = Gk(ispin,jspin,iorb,jorb)%less(N-1,N)+0.5d0*dt*dGkless(io,jo)
    end do
    AxGkmat=matmul(Amat,Gkmat)
    do concurrent (ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
       io = iorb + (ispin-1)*Norb
       jo = jorb + (jspin-1)*Norb
       Gk(ispin,jspin,iorb,jorb)%less(N,N)    = AxGkmat(io,jo)
       dGk_new(ispin,jspin,iorb,jorb)%less(N) = -xi*sum(Hk(io,:,N)*AxGkmat(:,jo))
    end do
    !
    deallocate(KxGk,Amat,Gkmat,AxGkmat,dGkless)
    !
  end subroutine free_kb_gf_rank4




  subroutine free_kb_gf_rank6(Hk,Gk,dGk,dGk_new)
    complex(8),dimension(:,:,:)           :: Hk          ![Nlso][Nlso][Nt]
    type(kb_gf),intent(inout)             :: Gk(:,:,:,:,:,:)     ![Nlat][Nlat][Nspin][Nspin][Norb][Norb]
    type(kb_dgf),intent(inout)            :: dGk(:,:,:,:,:,:)    !as Gk
    type(kb_dgf)                          :: dGk_new(:,:,:,:,:,:)!as Gk
    !
    integer                               :: N,L,Nlso,Niw
    real(8)                               :: dt,dtau
    integer                               :: i,j,itau,jtau,s
    integer                               :: io,jo,ko
    complex(8)                            :: HxGk
    complex(8),dimension(:),allocatable   :: KxGk
    real(8),dimension(:),allocatable      :: ftau
    complex(8),dimension(:,:),allocatable :: Amat,AxGkmat,Gkmat,dGkless
    !
    N   = cc_params%Nt                 !<== work with the ACTUAL size of the contour
    L   = cc_params%Ntau
    Niw = cc_params%Niw
    dt  = cc_params%dt
    dtau= cc_params%dtau
    Nlat = size(Gk,1) ; Nspin = size(Gk,3) ; Norb = size(Gk,5) ; Nlso=Nlat*Nspin*Norb
    call assert_shape(Hk,[Nlso,Nlso,cc_params%Ntime],"dyson_kb_gf_rank0","Hk")
    call assert_shape_kb_gf(Gk,[Nlat,Nlat,Nspin,Nspin,Norb,Norb],"dyson_kb_gf_Rank4","Gk")
    call assert_shape_kb_gf(dGk,[Nlat,Nlat,Nspin,Nspin,Norb,Norb],"dyson_kb_gf_Rank4","dGk")
    call assert_shape_kb_gf(dGk_new,[Nlat,Nlat,Nspin,Nspin,Norb,Norb],"dyson_kb_gf_Rank4","dGk_new")
    !
    !
    allocate(KxGk(0:max(N,L)))
    allocate(Amat(Nlso,Nlso))
    allocate(Gkmat(Nlso,Nlso))
    allocate(AxGkmat(Nlso,Nlso))
    allocate(dGkless(Nlso,Nlso))
    Amat   =zero
    Gkmat  =zero
    AxGkmat=zero
    dGkless=zero
    !
    !Treat here the t=0,0 case:
    !We solve the matsubara part using FFT
    if(N==1)then
       !INITIALIZE THE WEISS FIELD Gk^{x=M,<,R,\lmix} for N=1
       !neq_setup_contour_gf:
       do i=1,Niw
          Gkmat = (xi*cc_params%wm(i)+xmu)*zeye(Nlso) - Hk(:,:,1)
          call inv(Gkmat)
          do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
             io = iorb + (ispin-1)*Norb + (ilat-1)*Nspin*Norb
             jo = jorb + (jspin-1)*Norb + (jlat-1)*Nspin*Norb
             Gk(ilat,jlat,ispin,jspin,iorb,jorb)%iw(i) = Gkmat(io,jo)
          enddo
       enddo
       !
       allocate(ftau(0:Niw))
       do ilat=1,Nlat
          do ispin=1,Nspin
             do iorb=1,Norb
                do jlat=1,Nlat
                   do jspin=1,Nspin
                      do jorb=1,Norb
                         call fft_iw2tau(Gk(ilat,jlat,ispin,jspin,iorb,jorb)%iw,ftau(0:),cc_params%beta,notail=(io/=jo))
                         call fft_extract_gtau(ftau(0:),Gk(ilat,jlat,ispin,jspin,iorb,jorb)%mats(0:))
                         Gk(ilat,jlat,ispin,jspin,iorb,jorb)%less(1,1)  = -xi*Gk(ilat,jlat,ispin,jspin,iorb,jorb)%mats(L)
                         Gk(ilat,jlat,ispin,jspin,iorb,jorb)%ret(1,1)   = zero
                         Gk(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(1,0:L)= -xi*Gk(ilat,jlat,ispin,jspin,iorb,jorb)%mats(L:0:-1)
                      enddo
                   enddo
                enddo
                Gk(ilat,ilat,ispin,ispin,iorb,iorb)%ret(1,1)  = -xi
             enddo
          enddo
       enddo
       deallocate(ftau)
       !
       !neq_setup_contour_dgf:
       do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb + (ilat-1)*Nspin*Norb
          jo = jorb + (jspin-1)*Norb + (jlat-1)*Nspin*Norb
          !get d/dt G_k^R = -i H(k,0)G_k^R
          HxGk=zero
          do klat=1,Nlat
             do kspin=1,Nspin
                do korb=1,Norb
                   io = iorb + (ispin-1)*Norb + (ilat-1)*Nspin*Norb
                   ko = korb + (kspin-1)*Norb + (klat-1)*Nspin*Norb              
                   HxGk = HxGk + Hk(io,ko,1)*Gk(klat,jlat,kspin,jspin,korb,jorb)%ret(1,1)
                enddo
             enddo
          enddo
          dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%ret(1) = - xi*HxGk
          !
          !get d/dt G_k^< = -i H(k,0)G_k^<
          HxGk=zero
          do klat=1,Nlat
             do kspin=1,Nspin
                do korb=1,Norb
                   io = iorb + (ispin-1)*Norb + (ilat-1)*Nspin*Norb
                   ko = korb + (kspin-1)*Norb + (klat-1)*Nspin*Norb
                   HxGk = HxGk + Hk(io,ko,1)*Gk(klat,jlat,kspin,jspin,korb,jorb)%less(1,1)
                enddo
             enddo
          enddo
          dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%less(1) = - xi*HxGk
          !
          !get d/dt G_k^\lmix = -xi*H(k,0)*G_k^\lmix
          KxGk(0:)=zero
          do klat=1,Nlat
             do kspin=1,Nspin
                do korb=1,Norb
                   io = iorb + (ispin-1)*Norb + (ilat-1)*Nspin*Norb
                   ko = korb + (kspin-1)*Norb + (klat-1)*Nspin*Norb
                   KxGk(0:) = KxGk(0:) + Hk(io,ko,1)*Gk(klat,jlat,kspin,jspin,korb,jorb)%lmix(1,0:)
                enddo
             enddo
          enddo
          dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(0:)= -xi*KxGk(0:)
       enddo
       !
       return
       !
    end if
    !
    !
    Amat  = zeye(Nlso) + 0.5d0*xi*dt*Hk(:,:,N)
    call inv(Amat)
    !
    !TYPE: X=RET,LESS,LMIX (R,<,\LMIX)
    !I D/DT Gk^X(T,:)  = Hk(T)Gk^X(T,:) + Q^X(T,:)
    !
    !Ret component
    !d/dt Gk^R(t,:) = -i*h(t)*Gk^R(t,:) -i*delta(t,:) 
    !TIP t_{N}, t`_{N}
    AxGkmat = -xi*zeye(Nlso)
    do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
       io = iorb + (ispin-1)*Norb + (ilat-1)*Nspin*Norb
       jo = jorb + (jspin-1)*Norb + (jlat-1)*Nspin*Norb
       Gk(ilat,jlat,ispin,jspin,iorb,jorb)%ret(N,N)    = AxGkmat(io,jo)
       dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%ret(N) = -xi*sum(Hk(io,:,N)*AxGkmat(:,jo))
    end do
    !
    !VERTICAL INTERVAL t_{N}, t'_{j=1,...,N-1}
    do j=1,N-1
       do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb + (ilat-1)*Nspin*Norb
          jo = jorb + (jspin-1)*Norb + (jlat-1)*Nspin*Norb
          Gkmat(io,jo) = Gk(ilat,jlat,ispin,jspin,iorb,jorb)%ret(N-1,j) + 0.5d0*dt*dGk(ilat,jlat,ispin,jspin,iorb,jorb)%ret(j)
       enddo
       AxGkmat = matmul(Amat,Gkmat)
       do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb + (ilat-1)*Nspin*Norb
          jo = jorb + (jspin-1)*Norb + (jlat-1)*Nspin*Norb
          Gk(ilat,jlat,ispin,jspin,iorb,jorb)%ret(N,j)    = AxGkmat(io,jo)
          dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%ret(j) = -xi*sum(Hk(io,:,N)*AxGkmat(:,jo))
       end do
    enddo
    !
    !
    !Lmix component
    !d/dt Gk^\lmix(t,:) = -i*Hk(t)*Gk^\lmix(t,:)
    do jtau=0,L
       do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb + (ilat-1)*Nspin*Norb
          jo = jorb + (jspin-1)*Norb + (jlat-1)*Nspin*Norb
          Gkmat(io,jo) = Gk(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(N-1,jtau) + 0.5d0*dt*dGk(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(jtau)
       end do
       AxGkmat = matmul(Amat,Gkmat)
       do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb + (ilat-1)*Nspin*Norb
          jo = jorb + (jspin-1)*Norb + (jlat-1)*Nspin*Norb
          Gk(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(N,jtau)    = AxGkmat(io,jo)
          dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(jtau) = -xi*sum(Hk(io,:,N)*AxGkmat(:,jo))
       end do
    enddo
    !
    !Less component
    !d/dt Gk^<(t,:) = -i*Hk(t)*Gk^<(t,:)
    ! Gk^<(t_{N},t_{j}), d/dt Gk^<(t_{N},t_{j}) <== lower-right triangle
    tp_less: do j=1,N-1
       do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb + (ilat-1)*Nspin*Norb
          jo = jorb + (jspin-1)*Norb + (jlat-1)*Nspin*Norb
          Gkmat(io,jo) = Gk(ilat,jlat,ispin,jspin,iorb,jorb)%less(N-1,j) + 0.5d0*dt*dGk(ilat,jlat,ispin,jspin,iorb,jorb)%less(j)
       end do
       AxGkmat = matmul(Amat,Gkmat)
       do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
          io = iorb + (ispin-1)*Norb + (ilat-1)*Nspin*Norb
          jo = jorb + (jspin-1)*Norb + (jlat-1)*Nspin*Norb
          Gk(ilat,jlat,ispin,jspin,iorb,jorb)%less(N,j)    = AxGkmat(io,jo)
          dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%less(j) = -xi*sum(Hk(io,:,N)*AxGkmat(:,jo))
       end do
    enddo tp_less
    !
    ! Gk^<(t_{i},t_{N}), d/dt Gk^<(t_{i},t_{N}) <== upper left triangle
    do concurrent (i=1:N-1,ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
       Gk(ilat,jlat,ispin,jspin,iorb,jorb)%less(i,N)=-conjg(Gk(jlat,ilat,jspin,ispin,jorb,iorb)%less(N,i))
    end do
    !
    ! d/dt Gk^<(t_{N-1},t_{N})
    do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
       io = iorb + (ispin-1)*Norb + (ilat-1)*Nspin*Norb
       jo = jorb + (jspin-1)*Norb + (jlat-1)*Nspin*Norb
       dGkless(io,jo)=zero
       do klat=1,Nlat
          do kspin=1,Nspin
             do korb=1,Norb
                ko = korb + (kspin-1)*Norb + (klat-1)*Nspin*Norb
                dGkless(io,jo) = dGkless(io,jo)-xi*Hk(io,ko,N-1)*Gk(klat,jlat,kspin,jspin,korb,jorb)%less(N-1,N)
             enddo
          enddo
       enddo
    enddo
    !
    !Gk^<(N,N), d/dt Gk^<(N,N)
    do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
       io = iorb + (ispin-1)*Norb + (ilat-1)*Nspin*Norb
       jo = jorb + (jspin-1)*Norb + (jlat-1)*Nspin*Norb
       Gkmat(io,jo) = Gk(ilat,jlat,ispin,jspin,iorb,jorb)%less(N-1,N)+0.5d0*dt*dGkless(io,jo)
    end do
    AxGkmat=matmul(Amat,Gkmat)
    do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Nspin,iorb=1:Norb,jorb=1:Norb)
       io = iorb + (ispin-1)*Norb + (ilat-1)*Nspin*Norb
       jo = jorb + (jspin-1)*Norb + (jlat-1)*Nspin*Norb
       Gk(ilat,jlat,ispin,jspin,iorb,jorb)%less(N,N)    = AxGkmat(io,jo)
       dGk_new(ilat,jlat,ispin,jspin,iorb,jorb)%less(N) = -xi*sum(Hk(io,:,N)*AxGkmat(:,jo))
    end do
    !
    deallocate(KxGk,Amat,Gkmat,AxGkmat,dGkless)
    !
  end subroutine free_kb_gf_rank6









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



  subroutine free_kb_gf_d1(H,G,dG,dG_new)
    complex(8),dimension(:,:)  :: H       ![Nk][Nt]
    type(kb_gf),intent(inout)  :: G(:)     !as K
    type(kb_dgf),intent(inout) :: dG(:)    !as K
    type(kb_dgf)               :: dG_new(:)!as K
    Nk = size(H,1)
    call assert_shape(H,[Nk,cc_params%Ntime],"free_kb_gf_d1","H")
    call assert_shape_kb_gf(G,[Nk],"free_kb_gf_d1","G")
    call assert_shape_kb_gf(dG,[Nk],"free_kb_gf_d1","dG")
    call assert_shape_kb_gf(dG_new,[Nk],"free_kb_gf_d1","dG_new")
    !
    do ik=1,Nk
       call free_kb_gf_rank0(H(ik,:),G(ik),dG(ik),dG_new(ik))
    enddo
    !
  end subroutine free_kb_gf_d1



  subroutine free_kb_gf_d2(H,G,dG,dG_new)
    complex(8),dimension(:,:,:) :: H       ![Nso][Nso][Nt]
    type(kb_gf),intent(inout)   :: G(:,:)     !as K
    type(kb_dgf),intent(inout)  :: dG(:,:)    !as K
    type(kb_dgf)                :: dG_new(:,:)!as K
    integer                     :: Nso
    Nso = size(H,1)
    !
    call assert_shape(H,[Nso,Nso,cc_params%Ntime],"free_kb_gf_d2","H")
    call assert_shape_kb_gf(G,[Nso,Nso],"free_kb_gf_d2","G")
    call assert_shape_kb_gf(dG,[Nso,Nso],"free_kb_gf_d2","dG")
    call assert_shape_kb_gf(dG_new,[Nso,Nso],"free_kb_gf_d2","dG_new")
    !
    select case(Nso)
    case(1)
       call free_kb_gf_rank0(H(1,1,:),G(1,1),dG(1,1),dG_new(1,1))
    case default
       call free_kb_gf_rank2(H,G,dG,dG_new)
    end select
    !
  end subroutine free_kb_gf_d2


  subroutine free_kb_gf_d3(H,G,dG,dG_new)
    complex(8),dimension(:,:,:,:) :: H             ![Nso][Nso][Nk][Nt]
    type(kb_gf),intent(inout)     :: G(:,:,:)      !as K
    type(kb_dgf),intent(inout)    :: dG(:,:,:)     !as K
    type(kb_dgf)                  :: dG_new(:,:,:) !as K
    integer                       :: Nk,Nso,ik
    Nso = size(H,1)
    Nk  = size(H,3)
    !
    call assert_shape(H,[Nso,Nso,Nk,cc_params%Ntime],"free_kb_gf_d3","H")
    call assert_shape_kb_gf(G,[Nso,Nso,Nk],"free_kb_gf_d3","G")
    call assert_shape_kb_gf(dG,[Nso,Nso,Nk],"free_kb_gf_d3","dG")
    call assert_shape_kb_gf(dG_new,[Nso,Nso,Nk],"free_kb_gf_d3","dG_new")
    !
    select case(Nso)
    case(1)
       do ik=1,Nk
          call free_kb_gf_rank0(H(1,1,ik,:),G(1,1,ik),dG(1,1,ik),dG_new(1,1,ik))
       enddo
    case default
       do ik=1,Nk
          call free_kb_gf_rank2(H(:,:,ik,:),G(:,:,ik),dG(:,:,ik),dG_new(:,:,ik))
       enddo
    end select
    !
  end subroutine free_kb_gf_d3



  subroutine free_kb_gf_d4(H,G,dG,dG_new)
    complex(8),dimension(:,:,:)             :: H      ![Nso][Nso][Nt]
    type(kb_gf),intent(inout)               :: G(:,:,:,:)     !![Nspin][Nspin][Norb][Norb]
    type(kb_dgf),intent(inout)              :: dG(:,:,:,:)    !as G
    type(kb_dgf)                            :: dG_new(:,:,:,:)!as G
    Nspin = size(G,1)
    Norb  = size(G,3)
    Nso   = Nspin*Norb
    !
    call assert_shape(H,[Nso,Nso,cc_params%Ntime],"free_kb_gf_d4","H")
    call assert_shape_kb_gf(G,[Nspin,Nspin,Norb,Norb],"free_kb_gf_d4","G")
    call assert_shape_kb_gf(dG,[Nspin,Nspin,Norb,Norb],"free_kb_gf_d4","dG")
    call assert_shape_kb_gf(dG_new,[Nspin,Nspin,Norb,Norb],"free_kb_gf_d4","dG_new")
    !
    select case(Nso)
    case(1)
       call free_kb_gf_rank0(H(1,1,:),G(1,1,1,1),dG(1,1,1,1),dG_new(1,1,1,1))
    case default
       call free_kb_gf_rank4(H,G,dG,dG_new)
    end select
  end subroutine free_kb_gf_d4



  subroutine free_kb_gf_d5(H,G,dG,dG_new)
    complex(8),dimension(:,:,:,:)           :: H      ![Nso][Nso][Nk][Nt]
    type(kb_gf),intent(inout)               :: G(:,:,:,:,:)     ![Nspin][Nspin][Norb][Norb][Nk]
    type(kb_dgf),intent(inout)              :: dG(:,:,:,:,:)    !as G
    type(kb_dgf)                            :: dG_new(:,:,:,:,:)!as G
    type(kb_gf),dimension(:,:),allocatable  :: K_,G_
    type(kb_dgf),dimension(:,:),allocatable :: dG_,dG_new_
    integer                                 :: Nk
    Nspin = size(G,1)
    Norb  = size(G,3)
    Nk    = size(H,3)
    Nso   = Nspin*Norb
    !
    call assert_shape(H,[Nso,Nso,Nk,cc_params%Ntime],"free_kb_gf_d5","H")
    call assert_shape_kb_gf(G,[Nspin,Nspin,Norb,Norb,Nk],"free_kb_gf_d5","G")
    call assert_shape_kb_gf(dG,[Nspin,Nspin,Norb,Norb,Nk],"free_kb_gf_d5","dG")
    call assert_shape_kb_gf(dG_new,[Nspin,Nspin,Norb,Norb,Nk],"free_kb_gf_d5","dG_new")
    !
    select case(Nso)
    case(1)
       do ik=1,Nk
          call free_kb_gf_rank0(&
               H(1,1,ik,:),&
               G(1,1,1,1,ik),&
               dG(1,1,1,1,ik),&
               dG_new(1,1,1,1,ik))
       enddo
    case default
       do ik=1,Nk
          call free_kb_gf_rank4(&
               H(:,:,ik,:),&
               G(:,:,:,:,ik),&
               dG(:,:,:,:,ik),&
               dG_new(:,:,:,:,ik))
       enddo
    end select
  end subroutine free_kb_gf_d5



  subroutine free_kb_gf_d6(H,G,dG,dG_new)
    complex(8),dimension(:,:,:)             :: H      ![Nso][Nso][Nt]
    type(kb_gf),intent(inout)               :: G(:,:,:,:,:,:)     !![Nlat][Nlat][Nspin][Nspin][Norb][Norb]
    type(kb_dgf),intent(inout)              :: dG(:,:,:,:,:,:)    !as G
    type(kb_dgf)                            :: dG_new(:,:,:,:,:,:)!as G
    !
    !
    Nlat  = size(G,1)
    Nspin = size(G,3)
    Norb  = size(G,5)
    Nlso  = Nlat*Nspin*Norb
    !
    call assert_shape(H,[Nlso,Nlso,cc_params%Ntime],"free_kb_gf_d6","H")
    call assert_shape_kb_gf(G,[Nlat,Nlat,Nspin,Nspin,Norb,Norb],"free_kb_gf_d6","G")
    call assert_shape_kb_gf(dG,[Nlat,Nlat,Nspin,Nspin,Norb,Norb],"free_kb_gf_d6","dG")
    call assert_shape_kb_gf(dG_new,[Nlat,Nlat,Nspin,Nspin,Norb,Norb],"free_kb_gf_d6","dG_new")
    !
    select case(Nlso)
    case (1)
       call free_kb_gf_rank0(H(1,1,:),G(1,1,1,1,1,1),dG(1,1,1,1,1,1),dG_new(1,1,1,1,1,1))
    case default
       call free_kb_gf_rank6(H,G,dG,dG_new)
    end select
  end subroutine free_kb_gf_d6


  subroutine free_kb_gf_d7(H,G,dG,dG_new)
    complex(8),dimension(:,:,:,:)           :: H      ![Nlso][Nlso][Nk][Nt]
    type(kb_gf),intent(inout)               :: G(:,:,:,:,:,:,:)     ! ![Nlat][Nlat][Nspin][Nspin][Norb][Norb][Nk]
    type(kb_dgf),intent(inout)              :: dG(:,:,:,:,:,:,:)    !as G
    type(kb_dgf)                            :: dG_new(:,:,:,:,:,:,:)!as G
    integer                                 :: Nk
    Nlat  = size(G,1)
    Nspin = size(G,3)
    Norb  = size(G,5)
    Nk    = size(H,3)
    Nlso  = Nlat*Nspin*Norb
    !
    call assert_shape(H,[Nlso,Nlso,Nk,cc_params%Ntime],"free_kb_gf_d7","H")
    call assert_shape_kb_gf(G,[Nlat,Nlat,Nspin,Nspin,Norb,Norb,Nk],"free_kb_gf_d7","G")
    call assert_shape_kb_gf(dG,[Nlat,Nlat,Nspin,Nspin,Norb,Norb,Nk],"free_kb_gf_d7","dG")
    call assert_shape_kb_gf(dG_new,[Nlat,Nlat,Nspin,Nspin,Norb,Norb,Nk],"free_kb_gf_d7","dG_new")
    !
    select case(Nlso)
    case(1)
       do ik=1,Nk
          call free_kb_gf_rank0(&
               H(1,1,ik,:),&
               G(1,1,1,1,1,1,ik),&
               dG(1,1,1,1,1,1,ik),&
               dG_new(1,1,1,1,1,1,ik))
       enddo
    case default
       do ik=1,Nk
          call free_kb_gf_rank6(&
               H(:,:,ik,:),&
               G(:,:,:,:,:,:,ik),&
               dG(:,:,:,:,:,:,ik),&
               dG_new(:,:,:,:,:,:,ik))
       enddo
    end select
  end subroutine free_kb_gf_d7



END MODULE KB_GF_FREE











