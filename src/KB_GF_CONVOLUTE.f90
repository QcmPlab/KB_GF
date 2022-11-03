MODULE KB_GF_CONVOLUTE
  USE KB_VARS_GLOBAL
  USE KB_CONTOUR
  USE KB_AUX
  USE KB_GF_COMMON
  USE SCIFOR, only: one,xi,zero,pi,zeye,inv
  implicit none
  private


  ! CONVOLUTION:
  interface operator(.x.)
     module procedure :: convolute_kb_gf_rank0
     module procedure :: convolute_kb_gf_d1
     module procedure :: convolute_kb_gf_d2
     module procedure :: convolute_kb_gf_d3
     module procedure :: convolute_kb_gf_d4
     module procedure :: convolute_kb_gf_d5
     module procedure :: convolute_kb_gf_d6
     module procedure :: convolute_kb_gf_d7
  end interface operator(.x.)

  interface convolute_kb_gf
     module procedure :: convolute_kb_gf_rank0
     module procedure :: convolute_kb_gf_d1
     module procedure :: convolute_kb_gf_d2
     module procedure :: convolute_kb_gf_d3
     module procedure :: convolute_kb_gf_d4
     module procedure :: convolute_kb_gf_d5
     module procedure :: convolute_kb_gf_d6
     module procedure :: convolute_kb_gf_d7
  end interface convolute_kb_gf


  public :: operator(.x.)
  public :: convolute_kb_gf

contains





  !----------------------------------------------------------------------------
  !C(t,t')=(A*B)(t,t'), with t=t_max && t'=0,t_max
  !----------------------------------------------------------------------------
  function convolute_kb_gf_Rank0(A,B) result(C)
    type(kb_gf), intent(in)             :: A,B
    type(kb_gf)                         :: C
    integer                             :: N,L,Niw
    real(8)                             :: dt,dtau
    real(8),dimension(:),allocatable    :: ftau
    complex(8),dimension(:),allocatable :: AxB    
    integer                             :: i,j,k,itau,jtau
    !
    call C%init()
    !
    N   = cc_params%Nt      !<== work with the ACTUAL size of the contour
    L   = cc_params%Ntau
    Niw = cc_params%Niw
    dt  = cc_params%dt
    dtau= cc_params%dtau
    !
    allocate(AxB(0:max(L,N)))
    !
    if(N==1)then
       !Mats components:
       !C(iw)  =  A(iw)*B(iw) = FT[int_0^beta ds A(tau) B(tau-s)]
       C%iw(:) = A%iw(:)*B%iw(:)
       allocate(ftau(0:Niw))
       call fft_iw2tau(C%iw,ftau(0:),cc_params%beta,notail=.true.)
       call fft_extract_gtau(ftau(0:),C%mats(0:))
       !
       !Ret. component
       !C^R(t,t')=\int_{t'}^t ds A^R(t,s)*B^R(s,t') ==0 
       C%ret(1,1)=zero
       !
       !Lmix. component
       !C^\lmix(t,tau')=\int_0^{beta} ds A^\lmix(t,s)*B^M(s,tau')
       !               = I1
       C%lmix(N,0:L)=zero
       do jtau=0,L
          AxB(0:) = zero
          do k=0,jtau
             AxB(k)=A%lmix(1,k)*B%mats(L+k-jtau)
          end do
          C%lmix(1,jtau)=C%lmix(1,jtau)-dtau*kb_trapz(AxB(0:),0,jtau)
          do k=jtau,L
             AxB(k)=A%lmix(1,k)*B%mats(k-jtau)
          end do
          C%lmix(1,jtau)=C%lmix(1,jtau)+dtau*kb_trapz(AxB(0:),jtau,L)
       enddo
       !
       !Less component
       !C^<(t,t')=-i\int_0^{beta} ds A^\lmix(t,s)*B^\rmix(s,t')
       ! tip (j=1,N-1)
       C%less(1,1)=zero
       AxB(0:) = zero
       do k=0,L
          AxB(k)=A%lmix(1,k)*conjg(B%lmix(1,L-k))
       end do
       C%less(1,1)=C%less(1,1)-xi*dtau*kb_trapz(AxB(0:),0,L)
       !
       return
       !
    endif
    !
    !
    !Ret. component
    !C^R(t,t')=\int_{t'}^t ds A^R(t,s)*B^R(s,t')
    C%ret(N,1:N)=zero
    do j=1,N           !for all t' in 0:t_max
       AxB(0:)  = zero !AxB should be set to zero before integration
       do k=j,N        !store the convolution between t'{=j} and t{=N}
          AxB(k) = A%ret(N,k)*B%ret(k,j)
       enddo
       C%ret(n,j) = C%ret(n,j) + dt*kb_trapz(AxB(0:),j,N)
    enddo
    !
    !Lmix. component
    !C^\lmix(t,tau')=\int_0^{beta} ds A^\lmix(t,s)*B^M(s,tau')
    !                  +\int_0^{t} ds A^R(t,s)*B^\lmix(s,tau')
    !               = I1 + I2
    C%lmix(N,0:L)=zero
    do jtau=0,L
       !I1:
       !break the integral I1 in two parts to take care of the sign of (tau-tau').
       AxB(0:) = zero
       do k=0,jtau
          AxB(k)=A%lmix(N,k)*B%mats(L+k-jtau)
       end do
       C%lmix(N,jtau)=C%lmix(N,jtau)-dtau*kb_trapz(AxB(0:),0,jtau)
       do k=jtau,L
          AxB(k)=A%lmix(N,k)*B%mats(k-jtau)
       end do
       C%lmix(n,jtau)=C%lmix(n,jtau)+dtau*kb_trapz(AxB(0:),jtau,L)
       !
       !I2:
       AxB(0:) = zero
       do k=1,N
          AxB(k) = A%ret(N,k)*B%lmix(k,jtau)
       enddo
       C%lmix(N,jtau) = C%lmix(N,jtau) + dt*kb_trapz(AxB(0:),1,N)
    enddo
    !
    !Less component
    !C^<(t,t')=-i\int_0^{beta} ds A^\lmix(t,s)*B^\rmix(s,t')
    !             +\int_0^{t'} ds A^<(t,s)*B^A(s,t')
    !             +\int_0^{t} ds A^R(t,s)*B^<(s,t')
    ! (t,t')=>(N,j) <==> Vertical side, no tip (j=1,N-1)
    do j=1,N-1
       C%less(N,j)=zero
       !
       AxB(0:) = zero
       do k=0,L
          AxB(k)=A%lmix(N,k)*conjg(B%lmix(j,L-k))
       end do
       C%less(N,j)=C%less(N,j)-xi*dtau*kb_trapz(AxB(0:),0,L)
       !
       AxB(0:) = zero
       do k=1,j
          AxB(k)=A%less(N,k)*conjg(B%ret(j,k))
       end do
       C%less(N,j)=C%less(N,j)+dt*kb_trapz(AxB(0:),1,j)
       !
       AxB(0:) = zero
       do k=1,N
          AxB(k)=A%ret(N,k)*B%less(k,j)
       end do
       C%less(N,j)=C%less(N,j)+dt*kb_trapz(AxB(0:),1,N)
       !
    end do
    !
    ! (t,t')=>(i,N) <==> Horizontal side, w/ tip (i=1,N)
    do i=1,N
       C%less(i,N)=zero
       do k=0,L
          AxB(k)=A%lmix(i,k)*conjg(B%lmix(n,L-k))
       end do
       C%less(i,N)=C%less(i,N)-xi*dtau*kb_trapz(AxB(0:),0,L)
       !
       do k=1,N
          AxB(k)=A%less(i,k)*conjg(B%ret(N,k))
       end do
       C%less(i,N)=C%less(i,N)+dt*kb_trapz(AxB(0:),1,N)
       !
       do k=1,i
          AxB(k)=A%ret(i,k)*B%less(k,N)
       end do
       C%less(i,N)=C%less(i,N)+dt*kb_trapz(AxB(0:),1,i)
    end do
    !      
    deallocate(AxB)
  end function convolute_kb_gf_Rank0


  function convolute_kb_gf_rank2(A,B) result(C)
    type(kb_gf), intent(in)             :: A(:,:)                  ![N1,Nk]
    type(kb_gf), intent(in)             :: B(:,:)                  ![Nk,N2]
    type(kb_gf)                         :: C(size(A,1),size(B,2))  ![N1,N2]
    integer                             :: N,L,N1,N2,Nk,Niw
    real(8)                             :: dt,dtau
    complex(8),dimension(:),allocatable :: AxB
    real(8),dimension(:),allocatable    :: ftau
    integer                             :: i,j,s,itau,jtau,i1,i2,ik
    !
    call C%init();C=zero
    !
    N   = cc_params%Nt      !<== work with the ACTUAL size of the contour
    L   = cc_params%Ntau
    Niw = cc_params%Niw
    dt  = cc_params%dt
    dtau= cc_params%dtau
    !
    N1 = size(A,1)
    Nk = size(A,2)
    N2 = size(B,2)
    call assert_shape_kb_gf(A,[N1,Nk],"convolute_kb_gf_rank2","A")
    call assert_shape_kb_gf(B,[Nk,N2],"convolute_kb_gf_rank2","B")
    call assert_shape_kb_gf(C,[N1,N2],"convolute_kb_gf_rank2","C")
    !
    !
    allocate(AxB(0:max(L,N)));AxB=zero
    !
    !
    !C_ab = (A .x. B)_ab = \sum_k A_ak .x. B_kb
    !Convolute all components
    if(N==1)then
       allocate(ftau(0:Niw))
       do i1=1,N1
          do i2=1,N2
             !Mats components:
             !C_ab(iw)  = sum_k A_ak(iw)*B_kb(iw) = FT[sum_k int_0^beta ds A_ak(tau) B_kb(tau-s)]
             C(i1,i2)%iw=zero
             do ik=1,Nk
                C(i1,i2)%iw = C(i1,i2)%iw + A(i1,ik)%iw*B(ik,i2)%iw
             enddo
             call fft_iw2tau(C(i1,i2)%iw,ftau(0:),cc_params%beta,notail=.true.)
             call fft_extract_gtau(ftau(0:),C(i1,i2)%mats(0:))
             !
             !Ret. component
             !C^R(t,t')=\int_{t'}^t ds A^R(t,s)*B^R(s,t')
             C(i1,i2)%ret(1,1)=zero
             !
             !Lmix. component
             !C^\lmix(t,tau')=\int_0^{beta} ds A^\lmix(t,s)*B^M(s,tau') I1
             C(i1,i2)%lmix(1,0:L)=zero
             do jtau=0,L
                !I1:
                AxB(0:) = zero
                do s=0,jtau
                   do ik=1,Nk
                      AxB(s) = AxB(s) + A(i1,ik)%lmix(1,s)*B(ik,i2)%mats(L+s-jtau)
                   enddo
                enddo
                C(i1,i2)%lmix(1,jtau)=C(i1,i2)%lmix(1,jtau)-dtau*kb_trapz(AxB(0:),0,jtau)
                AxB(0:)=zero
                do s=jtau,L
                   do ik=1,Nk
                      AxB(s) = AxB(s) + A(i1,ik)%lmix(1,s)*B(ik,i2)%mats(s-jtau)
                   enddo
                enddo
                C(i1,i2)%lmix(1,jtau)=C(i1,i2)%lmix(1,jtau)+dtau*kb_trapz(AxB(0:),jtau,L)
                !
             enddo
             !
             !Less component
             !C^<(t,t')=-i\int_0^{beta} ds A^\lmix(t,s)*B^\rmix(s,t') I1.
             !tip (i=1,N)
             C(i1,i2)%less(1,1)=zero
             AxB(0:) = zero
             do s=0,L
                do ik=1,Nk
                   AxB(s) = AxB(s) + A(i1,ik)%lmix(1,s)*conjg( B(ik,i2)%lmix(1,L-s) )
                enddo
             enddo
             C(i1,i2)%less(1,1)=C(i1,i2)%less(1,1)-xi*dtau*kb_trapz(AxB(0:),0,L)
             !
          enddo
       enddo
       deallocate(ftau)
       !
       return
       !
    endif
    !
    !
    do i1=1,N1
       do i2=1,N2
          !
          !Ret. component
          !C^R(t,t')=\int_{t'}^t ds A^R(t,s)*B^R(s,t')
          C(i1,i2)%ret(N,1:N)=zero
          do j=1,N
             AxB(0:) = zero
             do s=j,N
                do ik=1,Nk
                   AxB(s) = AxB(s) + A(i1,ik)%ret(N,s)*B(ik,i2)%ret(s,j)
                enddo
             enddo
          enddo
          C(i1,i2)%ret(n,j) = C(i1,i2)%ret(n,j) + dt*kb_trapz(AxB(0:),j,N)
          !
          !Lmix. component
          !C^\lmix(t,tau')=\int_0^{beta} ds A^\lmix(t,s)*B^M(s,tau') I1
          !                  +\int_0^{t} ds A^R(t,s)*B^\lmix(s,tau') I2
          C(i1,i2)%lmix(N,0:L)=zero
          do jtau=0,L
             !I1:
             AxB(0:) = zero
             do s=0,jtau
                do ik=1,Nk
                   AxB(s) = AxB(s) + A(i1,ik)%lmix(N,s)*B(ik,i2)%mats(L+s-jtau)
                enddo
             enddo
             C(i1,i2)%lmix(N,jtau)=C(i1,i2)%lmix(N,jtau)-dtau*kb_trapz(AxB(0:),0,jtau)
             AxB(0:)=zero
             do s=jtau,L
                do ik=1,Nk
                   AxB(s) = AxB(s) + A(i1,ik)%lmix(N,s)*B(ik,i2)%mats(s-jtau)
                enddo
             enddo
             C(i1,i2)%lmix(n,jtau)=C(i1,i2)%lmix(n,jtau)+dtau*kb_trapz(AxB(0:),jtau,L)
             !
             !I2:
             AxB(0:) = zero
             do s=1,N
                do ik=1,Nk
                   AxB(s) = AxB(s) + A(i1,ik)%ret(N,s)*B(ik,i2)%lmix(s,jtau)
                enddo
             enddo
             C(i1,i2)%lmix(N,jtau) = C(i1,i2)%lmix(N,jtau) + dt*kb_trapz(AxB(0:),1,N)
          enddo
          !
          !Less component
          !C^<(t,t')=-i\int_0^{beta} ds A^\lmix(t,s)*B^\rmix(s,t') I1.
          !             +\int_0^{t'} ds A^<(t,s)*B^A(s,t')         I2. 
          !             +\int_0^{t} ds A^R(t,s)*B^<(s,t')          I3. 
          ! (t,t')=>(N,j) <==> Vertical side, with no tip (j=1,N-1)
          C(i1,i2)%less(N,1:N-1)=zero
          do j=1,N-1
             !I1.
             AxB(0:) = zero
             do s=0,L
                do ik=1,Nk
                   AxB(s) = AxB(s) + A(i1,ik)%lmix(N,s)*conjg( B(ik,i2)%lmix(j,L-s) ) !rmix <-- lmix
                enddo
             enddo
             C(i1,i2)%less(N,j)=C(i1,i2)%less(N,j)-xi*dtau*kb_trapz(AxB(0:),0,L)
             !
             !I2.
             AxB(0:) = zero
             do s=1,j
                do ik=1,Nk
                   AxB(s) = AxB(s) + A(i1,ik)%less(N,s)*conjg( B(ik,i2)%ret(j,s) ) !adv <-- ret
                enddo
             enddo
             C(i1,i2)%less(N,j)=C(i1,i2)%less(N,j)+dt*kb_trapz(AxB(0:),1,j)
             !
             !I3.
             AxB(0:) = zero
             do s=1,N
                do ik=1,Nk
                   AxB(s) = AxB(s) + A(i1,ik)%ret(N,s)*B(ik,i2)%less(s,j)
                enddo
             enddo
             C(i1,i2)%less(N,j)=C(i1,i2)%less(N,j)+dt*kb_trapz(AxB(0:),1,N)
          end do
          !
          !
          ! (t,t')=>(i,N) <==> Horizontal side, w/ tip (i=1,N)
          do i=1,N
             C(i1,i2)%less(i,N)=zero
             AxB(0:) = zero
             do s=0,L
                do ik=1,Nk
                   AxB(s) = AxB(s) + A(i1,ik)%lmix(i,s)*conjg( B(ik,i2)%lmix(n,L-s) )
                enddo
             enddo
             C(i1,i2)%less(i,N)=C(i1,i2)%less(i,N)-xi*dtau*kb_trapz(AxB(0:),0,L)
             !
             AxB(0:) = zero
             do s=1,N
                do ik=1,Nk
                   AxB(s) = AxB(s) + A(i1,ik)%less(i,s)*conjg( B(ik,i2)%ret(N,s) )
                enddo
             enddo
             C(i1,i2)%less(i,N)=C(i1,i2)%less(i,N)+dt*kb_trapz(AxB(0:),1,N)
             !
             AxB(0:) = zero
             do s=1,i
                do ik=1,Nk
                   AxB(s) = AxB(s) + A(i1,ik)%ret(i,s)*B(ik,i2)%less(s,N)
                enddo
             enddo
             C(i1,i2)%less(i,N)=C(i1,i2)%less(i,N)+dt*kb_trapz(AxB(0:),1,i)
          end do
          !
       enddo
    enddo
    !
    deallocate(AxB)
    !
  end function convolute_kb_gf_rank2



  function convolute_kb_gf_rank4(A,B) result(C)
    type(kb_gf), intent(in)             :: A(:,:,:,:)                  ![Nspin,Nspin,Norb,Norb]
    type(kb_gf), intent(in)             :: B(:,:,:,:)                  ![Nspin,Nspin,Norb,Norb]
    type(kb_gf)                         :: C(size(A,1),size(A,2),size(A,3),size(A,4))
    integer                             :: N,L,Nspin,Norb,Nk,Niw
    real(8)                             :: dt,dtau
    complex(8),dimension(:),allocatable :: AxB
    real(8),dimension(:),allocatable    :: ftau
    integer                             :: i,j,s,itau,jtau
    !
    call C%init();C=zero
    !
    N   = cc_params%Nt      !<== work with the ACTUAL size of the contour
    L   = cc_params%Ntau
    Niw = cc_params%Niw
    dt  = cc_params%dt
    dtau= cc_params%dtau
    !
    Nspin = size(A,1)
    Norb = size(A,2)
    call assert_shape_kb_gf(A,[Nspin,Nspin,Norb,Norb],"convolute_kb_gf_rank4","A")
    call assert_shape_kb_gf(B,[Nspin,Nspin,Norb,Norb],"convolute_kb_gf_rank4","B")
    call assert_shape_kb_gf(C,[Nspin,Nspin,Norb,Norb],"convolute_kb_gf_rank4","C")
    !
    !
    allocate(AxB(0:max(L,N)));AxB=zero
    !
    !
    !C_ab = (A .x. B)_ab = \sum_k A_ak .x. B_kb
    !Convolute all components
    if(N==1)then
       allocate(ftau(0:Niw))
       do ispin=1,Nspin        
          do jspin=1,Nspin
             do iorb=1,Norb
                do jorb=1,Norb
                   !Mats components:
                   !C_ab(iw)  = sum_k A_ak(iw)*B_kb(iw) = FT[sum_k int_0^beta ds A_ak(tau) B_kb(tau-s)]
                   C(ispin,jspin,iorb,jorb)%iw=zero
                   do ik=1,Nk
                      C(ispin,jspin,iorb,jorb)%iw = C(ispin,jspin,iorb,jorb)%iw + A(ispin,kspin,iorb,korb)%iw*B(kspin,jspin,korb,jorb)%iw
                   enddo
                   call fft_iw2tau(C(ispin,jspin,iorb,jorb)%iw,ftau(0:),cc_params%beta,notail=.true.)
                   call fft_extract_gtau(ftau(0:),C(ispin,jspin,iorb,jorb)%mats(0:))
                   !
                   !Ret. component
                   !C^R(t,t')=\int_{t'}^t ds A^R(t,s)*B^R(s,t')
                   C(ispin,jspin,iorb,jorb)%ret(1,1)=zero
                   !
                   !Lmix. component
                   !C^\lmix(t,tau')=\int_0^{beta} ds A^\lmix(t,s)*B^M(s,tau') I1
                   C(ispin,jspin,iorb,jorb)%lmix(1,0:L)=zero
                   do jtau=0,L
                      !I1:
                      AxB(0:) = zero
                      do s=0,jtau
                         do ik=1,Nk
                            AxB(s) = AxB(s) + A(ispin,kspin,iorb,korb)%lmix(1,s)*B(kspin,jspin,korb,jorb)%mats(L+s-jtau)
                         enddo
                      enddo
                      C(ispin,jspin,iorb,jorb)%lmix(1,jtau)=C(ispin,jspin,iorb,jorb)%lmix(1,jtau)-dtau*kb_trapz(AxB(0:),0,jtau)
                      AxB(0:)=zero
                      do s=jtau,L
                         do ik=1,Nk
                            AxB(s) = AxB(s) + A(ispin,kspin,iorb,korb)%lmix(1,s)*B(kspin,jspin,korb,jorb)%mats(s-jtau)
                         enddo
                      enddo
                      C(ispin,jspin,iorb,jorb)%lmix(1,jtau)=C(ispin,jspin,iorb,jorb)%lmix(1,jtau)+dtau*kb_trapz(AxB(0:),jtau,L)
                      !
                   enddo
                   !
                   !Less component
                   !C^<(t,t')=-i\int_0^{beta} ds A^\lmix(t,s)*B^\rmix(s,t') I1.
                   !tip (i=1,N)
                   C(ispin,jspin,iorb,jorb)%less(1,1)=zero
                   AxB(0:) = zero
                   do s=0,L
                      do ik=1,Nk
                         AxB(s) = AxB(s) + A(ispin,kspin,iorb,korb)%lmix(1,s)*conjg( B(kspin,jspin,korb,jorb)%lmix(1,L-s) )
                      enddo
                   enddo
                   C(ispin,jspin,iorb,jorb)%less(1,1)=C(ispin,jspin,iorb,jorb)%less(1,1)-xi*dtau*kb_trapz(AxB(0:),0,L)
                   !
                enddo
             enddo
          enddo
       enddo
       deallocate(ftau)
       !
       return
       !
    endif
    !
    !
    do ispin=1,Nspin        
       do jspin=1,Nspin
          do iorb=1,Norb
             do jorb=1,Norb
                !
                !Ret. component
                !C^R(t,t')=\int_{t'}^t ds A^R(t,s)*B^R(s,t')
                C(ispin,jspin,iorb,jorb)%ret(N,1:N)=zero
                do j=1,N
                   AxB(0:) = zero
                   do s=j,N
                      do ik=1,Nk
                         AxB(s) = AxB(s) + A(ispin,kspin,iorb,korb)%ret(N,s)*B(kspin,jspin,korb,jorb)%ret(s,j)
                      enddo
                   enddo
                enddo
                C(ispin,jspin,iorb,jorb)%ret(n,j) = C(ispin,jspin,iorb,jorb)%ret(n,j) + dt*kb_trapz(AxB(0:),j,N)
                !
                !Lmix. component
                !C^\lmix(t,tau')=\int_0^{beta} ds A^\lmix(t,s)*B^M(s,tau') I1
                !                  +\int_0^{t} ds A^R(t,s)*B^\lmix(s,tau') I2
                C(ispin,jspin,iorb,jorb)%lmix(N,0:L)=zero
                do jtau=0,L
                   !I1:
                   AxB(0:) = zero
                   do s=0,jtau
                      do ik=1,Nk
                         AxB(s) = AxB(s) + A(ispin,kspin,iorb,korb)%lmix(N,s)*B(kspin,jspin,korb,jorb)%mats(L+s-jtau)
                      enddo
                   enddo
                   C(ispin,jspin,iorb,jorb)%lmix(N,jtau)=C(ispin,jspin,iorb,jorb)%lmix(N,jtau)-dtau*kb_trapz(AxB(0:),0,jtau)
                   AxB(0:)=zero
                   do s=jtau,L
                      do ik=1,Nk
                         AxB(s) = AxB(s) + A(ispin,kspin,iorb,korb)%lmix(N,s)*B(kspin,jspin,korb,jorb)%mats(s-jtau)
                      enddo
                   enddo
                   C(ispin,jspin,iorb,jorb)%lmix(n,jtau)=C(ispin,jspin,iorb,jorb)%lmix(n,jtau)+dtau*kb_trapz(AxB(0:),jtau,L)
                   !
                   !I2:
                   AxB(0:) = zero
                   do s=1,N
                      do ik=1,Nk
                         AxB(s) = AxB(s) + A(ispin,kspin,iorb,korb)%ret(N,s)*B(kspin,jspin,korb,jorb)%lmix(s,jtau)
                      enddo
                   enddo
                   C(ispin,jspin,iorb,jorb)%lmix(N,jtau) = C(ispin,jspin,iorb,jorb)%lmix(N,jtau) + dt*kb_trapz(AxB(0:),1,N)
                enddo
                !
                !Less component
                !C^<(t,t')=-i\int_0^{beta} ds A^\lmix(t,s)*B^\rmix(s,t') I1.
                !             +\int_0^{t'} ds A^<(t,s)*B^A(s,t')         I2. 
                !             +\int_0^{t} ds A^R(t,s)*B^<(s,t')          I3. 
                ! (t,t')=>(N,j) <==> Vertical side, with no tip (j=1,N-1)
                C(ispin,jspin,iorb,jorb)%less(N,1:N-1)=zero
                do j=1,N-1
                   !I1.
                   AxB(0:) = zero
                   do s=0,L
                      do ik=1,Nk
                         AxB(s) = AxB(s) + A(ispin,kspin,iorb,korb)%lmix(N,s)*conjg( B(kspin,jspin,korb,jorb)%lmix(j,L-s) ) !rmix <-- lmix
                      enddo
                   enddo
                   C(ispin,jspin,iorb,jorb)%less(N,j)=C(ispin,jspin,iorb,jorb)%less(N,j)-xi*dtau*kb_trapz(AxB(0:),0,L)
                   !
                   !I2.
                   AxB(0:) = zero
                   do s=1,j
                      do ik=1,Nk
                         AxB(s) = AxB(s) + A(ispin,kspin,iorb,korb)%less(N,s)*conjg( B(kspin,jspin,korb,jorb)%ret(j,s) ) !adv <-- ret
                      enddo
                   enddo
                   C(ispin,jspin,iorb,jorb)%less(N,j)=C(ispin,jspin,iorb,jorb)%less(N,j)+dt*kb_trapz(AxB(0:),1,j)
                   !
                   !I3.
                   AxB(0:) = zero
                   do s=1,N
                      do ik=1,Nk
                         AxB(s) = AxB(s) + A(ispin,kspin,iorb,korb)%ret(N,s)*B(kspin,jspin,korb,jorb)%less(s,j)
                      enddo
                   enddo
                   C(ispin,jspin,iorb,jorb)%less(N,j)=C(ispin,jspin,iorb,jorb)%less(N,j)+dt*kb_trapz(AxB(0:),1,N)
                end do
                !
                !
                ! (t,t')=>(i,N) <==> Horizontal side, w/ tip (i=1,N)
                do i=1,N
                   C(ispin,jspin,iorb,jorb)%less(i,N)=zero
                   AxB(0:) = zero
                   do s=0,L
                      do ik=1,Nk
                         AxB(s) = AxB(s) + A(ispin,kspin,iorb,korb)%lmix(i,s)*conjg( B(kspin,jspin,korb,jorb)%lmix(n,L-s) )
                      enddo
                   enddo
                   C(ispin,jspin,iorb,jorb)%less(i,N)=C(ispin,jspin,iorb,jorb)%less(i,N)-xi*dtau*kb_trapz(AxB(0:),0,L)
                   !
                   AxB(0:) = zero
                   do s=1,N
                      do ik=1,Nk
                         AxB(s) = AxB(s) + A(ispin,kspin,iorb,korb)%less(i,s)*conjg( B(kspin,jspin,korb,jorb)%ret(N,s) )
                      enddo
                   enddo
                   C(ispin,jspin,iorb,jorb)%less(i,N)=C(ispin,jspin,iorb,jorb)%less(i,N)+dt*kb_trapz(AxB(0:),1,N)
                   !
                   AxB(0:) = zero
                   do s=1,i
                      do ik=1,Nk
                         AxB(s) = AxB(s) + A(ispin,kspin,iorb,korb)%ret(i,s)*B(kspin,jspin,korb,jorb)%less(s,N)
                      enddo
                   enddo
                   C(ispin,jspin,iorb,jorb)%less(i,N)=C(ispin,jspin,iorb,jorb)%less(i,N)+dt*kb_trapz(AxB(0:),1,i)
                end do
             enddo
          enddo
       enddo
    enddo
    !
    deallocate(AxB)
    !
  end function convolute_kb_gf_rank4



  function convolute_kb_gf_rank6(A,B) result(C)
    type(kb_gf), intent(in)             :: A(:,:,:,:,:,:)                  ![Nlat,Nlat,Nspin,Nspin,Norb,Norb]
    type(kb_gf), intent(in)             :: B(:,:,:,:,:,:)                  ![Nlat,Nlat,Nspin,Nspin,Norb,Norb]
    type(kb_gf)                         :: C(size(A,1),size(A,2),size(A,3),size(A,4),size(A,5),size(A,6))
    integer                             :: N,L,Nlat,Nspin,Norb,Nk,Niw
    real(8)                             :: dt,dtau
    complex(8),dimension(:),allocatable :: AxB
    real(8),dimension(:),allocatable    :: ftau
    integer                             :: i,j,s,itau,jtau
    !
    call C%init();C=zero
    !
    N   = cc_params%Nt      !<== work with the ACTUAL size of the contour
    L   = cc_params%Ntau
    Niw = cc_params%Niw
    dt  = cc_params%dt
    dtau= cc_params%dtau
    !
    Nlat  = size(A,1)
    Nspin = size(A,3)
    Norb  = size(A,5)
    call assert_shape_kb_gf(A,[Nlat,Nlat,Nspin,Nspin,Norb,Norb],"convolute_kb_gf_rank6","A")
    call assert_shape_kb_gf(B,[Nlat,Nlat,Nspin,Nspin,Norb,Norb],"convolute_kb_gf_rank6","B")
    call assert_shape_kb_gf(C,[Nlat,Nlat,Nspin,Nspin,Norb,Norb],"convolute_kb_gf_rank6","C")
    !
    !
    allocate(AxB(0:max(L,N)));AxB=zero
    !
    !
    !C_ab = (A .x. B)_ab = \sum_k A_ak .x. B_kb
    !Convolute all components
    if(N==1)then
       allocate(ftau(0:Niw))
       do ilat=1,Nspin        
          do jlat=1,Nspin
             do ispin=1,Nspin        
                do jspin=1,Nspin
                   do iorb=1,Norb
                      do jorb=1,Norb
                         !Mats components:
                         !C_ab(iw)  = sum_k A_ak(iw)*B_kb(iw) = FT[sum_k int_0^beta ds A_ak(tau) B_kb(tau-s)]
                         C(ilat,jlat,ispin,jspin,iorb,jorb)%iw=zero
                         do ik=1,Nk
                            C(ilat,jlat,ispin,jspin,iorb,jorb)%iw = C(ilat,jlat,ispin,jspin,iorb,jorb)%iw + A(ilat,klat,ispin,kspin,iorb,korb)%iw*B(klat,jlat,kspin,jspin,korb,jorb)%iw
                         enddo
                         call fft_iw2tau(C(ilat,jlat,ispin,jspin,iorb,jorb)%iw,ftau(0:),cc_params%beta,notail=.true.)
                         call fft_extract_gtau(ftau(0:),C(ilat,jlat,ispin,jspin,iorb,jorb)%mats(0:))
                         !
                         !Ret. component
                         !C^R(t,t')=\int_{t'}^t ds A^R(t,s)*B^R(s,t')
                         C(ilat,jlat,ispin,jspin,iorb,jorb)%ret(1,1)=zero
                         !
                         !Lmix. component
                         !C^\lmix(t,tau')=\int_0^{beta} ds A^\lmix(t,s)*B^M(s,tau') I1
                         C(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(1,0:L)=zero
                         do jtau=0,L
                            !I1:
                            AxB(0:) = zero
                            do s=0,jtau
                               do ik=1,Nk
                                  AxB(s) = AxB(s) + A(ilat,klat,ispin,kspin,iorb,korb)%lmix(1,s)*B(klat,jlat,kspin,jspin,korb,jorb)%mats(L+s-jtau)
                               enddo
                            enddo
                            C(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(1,jtau)=C(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(1,jtau)-dtau*kb_trapz(AxB(0:),0,jtau)
                            AxB(0:)=zero
                            do s=jtau,L
                               do ik=1,Nk
                                  AxB(s) = AxB(s) + A(ilat,klat,ispin,kspin,iorb,korb)%lmix(1,s)*B(klat,jlat,kspin,jspin,korb,jorb)%mats(s-jtau)
                               enddo
                            enddo
                            C(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(1,jtau)=C(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(1,jtau)+dtau*kb_trapz(AxB(0:),jtau,L)
                            !
                         enddo
                         !
                         !Less component
                         !C^<(t,t')=-i\int_0^{beta} ds A^\lmix(t,s)*B^\rmix(s,t') I1.
                         !tip (i=1,N)
                         C(ilat,jlat,ispin,jspin,iorb,jorb)%less(1,1)=zero
                         AxB(0:) = zero
                         do s=0,L
                            do ik=1,Nk
                               AxB(s) = AxB(s) + A(ilat,klat,ispin,kspin,iorb,korb)%lmix(1,s)*conjg( B(klat,jlat,kspin,jspin,korb,jorb)%lmix(1,L-s) )
                            enddo
                         enddo
                         C(ilat,jlat,ispin,jspin,iorb,jorb)%less(1,1)=C(ilat,jlat,ispin,jspin,iorb,jorb)%less(1,1)-xi*dtau*kb_trapz(AxB(0:),0,L)
                         !
                      enddo
                   enddo
                enddo
             enddo
          enddo
       enddo
       deallocate(ftau)
       !
       return
       !
    endif
    !
    do ilat=1,Nspin        
       do jlat=1,Nspin
          do ispin=1,Nspin        
             do jspin=1,Nspin
                do iorb=1,Norb
                   do jorb=1,Norb
                      !
                      !Ret. component
                      !C^R(t,t')=\int_{t'}^t ds A^R(t,s)*B^R(s,t')
                      C(ilat,jlat,ispin,jspin,iorb,jorb)%ret(N,1:N)=zero
                      do j=1,N
                         AxB(0:) = zero
                         do s=j,N
                            do ik=1,Nk
                               AxB(s) = AxB(s) + A(ilat,klat,ispin,kspin,iorb,korb)%ret(N,s)*B(klat,jlat,kspin,jspin,korb,jorb)%ret(s,j)
                            enddo
                         enddo
                      enddo
                      C(ilat,jlat,ispin,jspin,iorb,jorb)%ret(n,j) = C(ilat,jlat,ispin,jspin,iorb,jorb)%ret(n,j) + dt*kb_trapz(AxB(0:),j,N)
                      !
                      !Lmix. component
                      !C^\lmix(t,tau')=\int_0^{beta} ds A^\lmix(t,s)*B^M(s,tau') I1
                      !                  +\int_0^{t} ds A^R(t,s)*B^\lmix(s,tau') I2
                      C(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(N,0:L)=zero
                      do jtau=0,L
                         !I1:
                         AxB(0:) = zero
                         do s=0,jtau
                            do ik=1,Nk
                               AxB(s) = AxB(s) + A(ilat,klat,ispin,kspin,iorb,korb)%lmix(N,s)*B(klat,jlat,kspin,jspin,korb,jorb)%mats(L+s-jtau)
                            enddo
                         enddo
                         C(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(N,jtau)=C(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(N,jtau)-dtau*kb_trapz(AxB(0:),0,jtau)
                         AxB(0:)=zero
                         do s=jtau,L
                            do ik=1,Nk
                               AxB(s) = AxB(s) + A(ilat,klat,ispin,kspin,iorb,korb)%lmix(N,s)*B(klat,jlat,kspin,jspin,korb,jorb)%mats(s-jtau)
                            enddo
                         enddo
                         C(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(n,jtau)=C(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(n,jtau)+dtau*kb_trapz(AxB(0:),jtau,L)
                         !
                         !I2:
                         AxB(0:) = zero
                         do s=1,N
                            do ik=1,Nk
                               AxB(s) = AxB(s) + A(ilat,klat,ispin,kspin,iorb,korb)%ret(N,s)*B(klat,jlat,kspin,jspin,korb,jorb)%lmix(s,jtau)
                            enddo
                         enddo
                         C(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(N,jtau) = C(ilat,jlat,ispin,jspin,iorb,jorb)%lmix(N,jtau) + dt*kb_trapz(AxB(0:),1,N)
                      enddo
                      !
                      !Less component
                      !C^<(t,t')=-i\int_0^{beta} ds A^\lmix(t,s)*B^\rmix(s,t') I1.
                      !             +\int_0^{t'} ds A^<(t,s)*B^A(s,t')         I2. 
                      !             +\int_0^{t} ds A^R(t,s)*B^<(s,t')          I3. 
                      ! (t,t')=>(N,j) <==> Vertical side, with no tip (j=1,N-1)
                      C(ilat,jlat,ispin,jspin,iorb,jorb)%less(N,1:N-1)=zero
                      do j=1,N-1
                         !I1.
                         AxB(0:) = zero
                         do s=0,L
                            do ik=1,Nk
                               AxB(s) = AxB(s) + A(ilat,klat,ispin,kspin,iorb,korb)%lmix(N,s)*conjg( B(klat,jlat,kspin,jspin,korb,jorb)%lmix(j,L-s) ) !rmix <-- lmix
                            enddo
                         enddo
                         C(ilat,jlat,ispin,jspin,iorb,jorb)%less(N,j)=C(ilat,jlat,ispin,jspin,iorb,jorb)%less(N,j)-xi*dtau*kb_trapz(AxB(0:),0,L)
                         !
                         !I2.
                         AxB(0:) = zero
                         do s=1,j
                            do ik=1,Nk
                               AxB(s) = AxB(s) + A(ilat,klat,ispin,kspin,iorb,korb)%less(N,s)*conjg( B(klat,jlat,kspin,jspin,korb,jorb)%ret(j,s) ) !adv <-- ret
                            enddo
                         enddo
                         C(ilat,jlat,ispin,jspin,iorb,jorb)%less(N,j)=C(ilat,jlat,ispin,jspin,iorb,jorb)%less(N,j)+dt*kb_trapz(AxB(0:),1,j)
                         !
                         !I3.
                         AxB(0:) = zero
                         do s=1,N
                            do ik=1,Nk
                               AxB(s) = AxB(s) + A(ilat,klat,ispin,kspin,iorb,korb)%ret(N,s)*B(klat,jlat,kspin,jspin,korb,jorb)%less(s,j)
                            enddo
                         enddo
                         C(ilat,jlat,ispin,jspin,iorb,jorb)%less(N,j)=C(ilat,jlat,ispin,jspin,iorb,jorb)%less(N,j)+dt*kb_trapz(AxB(0:),1,N)
                      end do
                      !
                      !
                      ! (t,t')=>(i,N) <==> Horizontal side, w/ tip (i=1,N)
                      do i=1,N
                         C(ilat,jlat,ispin,jspin,iorb,jorb)%less(i,N)=zero
                         AxB(0:) = zero
                         do s=0,L
                            do ik=1,Nk
                               AxB(s) = AxB(s) + A(ilat,klat,ispin,kspin,iorb,korb)%lmix(i,s)*conjg( B(klat,jlat,kspin,jspin,korb,jorb)%lmix(n,L-s) )
                            enddo
                         enddo
                         C(ilat,jlat,ispin,jspin,iorb,jorb)%less(i,N)=C(ilat,jlat,ispin,jspin,iorb,jorb)%less(i,N)-xi*dtau*kb_trapz(AxB(0:),0,L)
                         !
                         AxB(0:) = zero
                         do s=1,N
                            do ik=1,Nk
                               AxB(s) = AxB(s) + A(ilat,klat,ispin,kspin,iorb,korb)%less(i,s)*conjg( B(klat,jlat,kspin,jspin,korb,jorb)%ret(N,s) )
                            enddo
                         enddo
                         C(ilat,jlat,ispin,jspin,iorb,jorb)%less(i,N)=C(ilat,jlat,ispin,jspin,iorb,jorb)%less(i,N)+dt*kb_trapz(AxB(0:),1,N)
                         !
                         AxB(0:) = zero
                         do s=1,i
                            do ik=1,Nk
                               AxB(s) = AxB(s) + A(ilat,klat,ispin,kspin,iorb,korb)%ret(i,s)*B(klat,jlat,kspin,jspin,korb,jorb)%less(s,N)
                            enddo
                         enddo
                         C(ilat,jlat,ispin,jspin,iorb,jorb)%less(i,N)=C(ilat,jlat,ispin,jspin,iorb,jorb)%less(i,N)+dt*kb_trapz(AxB(0:),1,i)
                      end do
                   enddo
                enddo
             enddo
          enddo
       enddo
    enddo
    !
    deallocate(AxB)
    !
  end function convolute_kb_gf_rank6




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


  function convolute_kb_gf_d1(A,B) result(C)
    type(kb_gf),intent(in) :: A(:)
    type(kb_gf),intent(in) :: B(size(A))
    type(kb_gf)            :: C(size(A))
    call C%init()
    do i1=1,size(A)
       C(i1) = convolute_kb_gf_rank0(A(i1),B(i1))
    enddo
  end function convolute_kb_gf_d1


  function convolute_kb_gf_d2(A,B) result(C)
    type(kb_gf), intent(in) :: A(:,:)                  ![N1,Nk]
    type(kb_gf), intent(in) :: B(:,:)                  ![Nk,N2]
    type(kb_gf)             :: C(size(A,1),size(B,2))  ![N1,N2]
    integer                 :: Nso        
    Nso = size(A,1)
    call C%init()
    select case(Nso)
    case (1)
       C(1,1) = convolute_kb_gf_rank0(A(1,1),B(1,1))
    case default
       C = convolute_kb_gf_rank2(A,B)
    end select
  end function convolute_kb_gf_d2


  function convolute_kb_gf_d3(A,B) result(C)
    type(kb_gf), intent(in) :: A(:,:,:)                  ![N1,Nk,:]
    type(kb_gf), intent(in) :: B(:,:,:)                  ![Nk,N2,:]
    type(kb_gf)             :: C(size(A,1),size(B,2),size(A,3))  ![N1,N2,:]
    integer                 :: ik
    call C%init()
    if(size(A,3) /= size(B,3)) stop "ERROR convolute_kb_gf_d3: size(B,3) /= size(A,3)"
    if(size(A,3) /= size(C,3)) stop "ERROR convolute_kb_gf_d3: size(C,3) /= size(A,3)"
    do ik=1,size(A,3)
       C(:,:,ik) = convolute_kb_gf_rank2(A(:,:,ik),B(:,:,ik))
    enddo
  end function convolute_kb_gf_d3


  function convolute_kb_gf_d4(A,B) result(C)
    type(kb_gf), intent(in) :: A(:,:,:,:)
    type(kb_gf), intent(in) :: B(:,:,:,:)
    type(kb_gf)             :: C(size(A,1),size(A,2),size(A,3),size(A,4))
    integer                 :: Nspin,Norb,Nso
    !
    call C%init()
    !
    Nspin = size(A,1) ; Norb = size(A,3) ; Nso=Nspin*Norb
    call assert_shape_kb_gf(A,[Nspin,Nspin,Norb,Norb],"convolute_kb_gf_d4","A")
    call assert_shape_kb_gf(B,[Nspin,Nspin,Norb,Norb],"convolute_kb_gf_d4","B")
    call assert_shape_kb_gf(C,[Nspin,Nspin,Norb,Norb],"convolute_kb_gf_d4","C")
    !
    select case(Nso)
    case (1)
       C(1,1,1,1) = convolute_kb_gf_rank0(A(1,1,1,1),B(1,1,1,1))
    case default
       C = convolute_kb_gf_rank4(A,B)
    end select
    !
  end function convolute_kb_gf_d4


  function convolute_kb_gf_d5(A,B) result(C)
    type(kb_gf), intent(in) :: A(:,:,:,:,:)
    type(kb_gf), intent(in) :: B(:,:,:,:,:)
    type(kb_gf)             :: C(size(A,1),size(A,2),size(A,3),size(A,4),size(A,5))
    integer                 :: ik,Nk,Norb,Nspin
    Nspin = size(A,1)
    Norb  = size(A,3)
    Nk    = size(A,5)
    call C%init()
    call assert_shape_kb_gf(A,[Nspin,Nspin,Norb,Norb,Nk],"convolute_kb_gf_d5","A")
    call assert_shape_kb_gf(B,[Nspin,Nspin,Norb,Norb,Nk],"convolute_kb_gf_d5","B")
    call assert_shape_kb_gf(C,[Nspin,Nspin,Norb,Norb,Nk],"convolute_kb_gf_d5","C")
    do ik=1,Nk
       C(:,:,:,:,ik) = convolute_kb_gf_rank4(A(:,:,:,:,ik),B(:,:,:,:,ik))
    enddo
  end function convolute_kb_gf_d5


  function convolute_kb_gf_d6(A,B) result(C)
    type(kb_gf), intent(in) :: A(:,:,:,:,:,:)
    type(kb_gf), intent(in) :: B(:,:,:,:,:,:)
    type(kb_gf)             :: C(size(A,1),size(A,2),size(A,3),size(A,4),size(A,5),size(A,6))
    integer                 :: Nlat,Nspin,Norb,Nlso
    !    
    call C%init()
    !
    Nlat = size(A,1) ; Nspin = size(A,3) ;  Norb = size(A,5) ; Nlso = Nlat*Nspin*Norb
    call assert_shape_kb_gf(A,[Nlat,Nlat,Nspin,Nspin,Norb,Norb],"convolute_kb_gf_d6","A")
    call assert_shape_kb_gf(B,[Nlat,Nlat,Nspin,Nspin,Norb,Norb],"convolute_kb_gf_d6","B")
    call assert_shape_kb_gf(C,[Nlat,Nlat,Nspin,Nspin,Norb,Norb],"convolute_kb_gf_d6","C")
    !
    select case(Nso)
    case (1)
       C(1,1,1,1,1,1) = convolute_kb_gf_rank0(A(1,1,1,1,1,1),B(1,1,1,1,1,1))
    case default
       C = convolute_kb_gf_rank6(A,B)
    end select
    !
  end function convolute_kb_gf_d6


  function convolute_kb_gf_d7(A,B) result(C)
    type(kb_gf), intent(in) :: A(:,:,:,:,:,:,:)
    type(kb_gf), intent(in) :: B(:,:,:,:,:,:,:)
    type(kb_gf)             :: C(size(A,1),size(A,2),size(A,3),size(A,4),size(A,5),size(A,6),size(A,7))
    integer                 :: ik,Nk,N1,N2,N3
    !
    call C%init()
    !
    N1 = size(A,1)
    N2 = size(A,3)
    N3 = size(A,5)
    Nk = size(A,7)
    call assert_shape_kb_gf(A,[N1,N1,N2,N2,N3,N3,Nk],"convolute_kb_gf_d7","A")
    call assert_shape_kb_gf(B,[N1,N1,N2,N2,N3,N3,Nk],"convolute_kb_gf_d7","B")
    call assert_shape_kb_gf(C,[N1,N1,N2,N2,N3,N3,Nk],"convolute_kb_gf_d7","C")
    do ik=1,Nk
       C(:,:,:,:,:,:,ik) = convolute_kb_gf_rank6(A(:,:,:,:,:,:,ik),B(:,:,:,:,:,:,ik))
    enddo
  end function convolute_kb_gf_d7



END MODULE KB_GF_CONVOLUTE











