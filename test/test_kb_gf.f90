program test
  USE SCIFOR  
  USE KB_LIB

  implicit none
  integer                                     :: i,j,ik,itime,Lk,Nx,Nso
  logical                                     :: converged
  real(8)                                     :: ts,time
  character(len=16)                           :: finput

  type(kb_gf)                                 :: K,G,F
  type(kb_gf),allocatable,dimension(:)        :: Kk,Gk,Fk
  type(kb_gf),allocatable,dimension(:,:,:)    :: Gkk

  integer                                     :: io,jo,ko
  real(8)                                     :: x(2)
  logical                                     :: bool
  integer                                     :: N,L,Lf

  !READ THE INPUT FILE (in vars_global):
  call parse_cmd_variable(finput,"FINPUT",default='input.conf')
  call parse_input_variable(ts,"ts",finput,default=1d0,comment="hopping")
  call parse_input_variable(Nx,"Nx",finput,default=21,comment="Number of k-points")
  !
  call kb_read_input(trim(finput))


  !BUILD TIME GRIDS AND NEQ-PARAMETERS:
  call setup_kb_contour_params()

  !Retrieve constants from global contour
  N = cc_params%Ntime
  L = cc_params%Ntau
  Lf= cc_params%Niw


  !Initialization
  print*,"Before init:",G%status!,F%status,K%status
  call G%init()
  ! F = kb_gf(N,L,Lf)
  ! K = kb_gf()
  print*,"After init (1) call G%init():",G%status
  ! print*,"After init (1) kb_gf(N,L,Lf):",F%status
  ! print*,"After init (3) kb_gf()      :",K%status
  print*,""

  !Destructor:
  print*,"Destructor:"
  call G%free
  print*,G%status
  print*,""



  !Elemental initialization: can init any array of objects!
  print*,"Init/Free Gk(10):"
  print*,"allocated Gk",allocated(Gk)
  allocate(Gk(10))  
  call Gk%init
  print*,"init 10:",Gk(:)%status
  call Gk%free
  print*,"free 10:",Gk(:)%status
  print*,"allocated Gk",allocated(Gk)
  print*,""

  print*,"Init/Free Gk(2,2,5):"
  print*,"allocated Gkk",allocated(Gkk)
  allocate(Gkk(2,2,5))
  call Gkk%init()
  print*,"init 20",Gkk(:,:,:)%status
  call Gkk%free()
  print*,"free 20",Gkk(:,:,:)%status
  print*,"allocated Gkk",allocated(Gkk)
  print*,""


  call G%init()
  call F%init()
  call K%init()

  G = dcmplx(pi,0d0)
  F = dcmplx(-pi,0d0)
  print*,"G%less(1,1:2)",G%less(1,1:2)
  print*,"F%less(1,1:2)",F%less(1,1:2)
  K = F
  print*,"K%less(1,1:2)",K%less(1,1:2)
  K = G + F
  print*,"(G+F)%less(1,1:2)",K%less(1,1:2)
  print*,""



  G=zero
  F=zero
  K=zero
  G = dcmplx(1d0,0d0)
  G = sqrt2*G
  print*,"(sqrt2*G)%less(1,1:2)",G%less(1,1:2)
  print*,""


  print*,"G%check()"
  print*, G%check()
  print*,"G(:)%check()"
  print*,Gk(:)%check()

  G = dcmplx(1d0,0d0)
  F = dcmplx(1d0,0d0)
  K = pi*G + (-pi)*F
  
  print*,"G%less(1,1:2)",G%less(1,1:2)
  print*,"F%less(1,1:2)",F%less(1,1:2)
  print*,"[pi*G+ (-pi*F)]%less(1,1:2)",K%less(1,1:2)
  print*,""

  G = dcmplx(1d0,0d0)
  F = dcmplx(1d0,0d0)
  K = pi*G - pi*F
  print*,"G%less(1,1:2)",G%less(1,1:2)
  print*,"F%less(1,1:2)",F%less(1,1:2)
  print*,"[pi*G - pi*F]%less(1,1:2)",K%less(1,1:2)
  print*,""

  G = dcmplx(10d0,0d0)
  F = G/10d0
  print*,"G%less(1,1:2)",G%less(1,1:2)
  print*,"[F/10]%less(1,1:2)",F%less(1,1:2)
  print*,""

  G = dcmplx(1d0,0d0)
  print*,"G%less(1,1:2)",G%less(1,1:2)
  K=zero
  do i=1,20
     K = K + G/20
     print*,"[sum(G)/10]%less(1,1:2)",K%less(1,1:2)
  enddo
  print*,""


  call Gk%init
  Gk = dcmplx(pi,0d0)/size(Gk)
  print*,"Gk(1:10)%less(1,1:2)"
  do i=1,size(Gk)
     print*,Gk(i)%less(1,1:2)
  enddo

  G = sum(Gk,1)
  print*,"Sum border 1=",G%less(1,1:2)


  G = sum(Gk)
  print*,"Sum whole   =",G%less(1,1:2)


  G = dcmplx(1d0,0d0)
  F = dcmplx(1d0,0d0)
  K = G.x.F
  print*,"Test Convolution as operator:"
  print*,"[GxF]%ret(1,1:2)",K%ret(1,1)
  print*,"[GxF]%less(1,1:2)",K%less(1,1)
  print*,"[GxF]%lmix(1,0:1)",K%lmix(1,0:1)
  print*,""


  allocate(Kk(10),Fk(10))
  call Gk%init()
  call Fk%init()
  call Kk%init()
  Gk = dcmplx(1d0,0d0)
  Fk = dcmplx(1d0,0d0)
  Kk = Gk.x.Fk
  print*,"Test Convolution as operator for arrays:"
  print*,"[GxF]%ret(1,1:2)",(Kk(i)%ret(1,1),i=1,5)
  print*,"[GxF]%less(1,1:2)",(Kk(i)%less(1,1),i=1,5)
  print*,"[GxF]%lmix(1,0:1)",(Kk(i)%lmix(1,0:1),i=1,5)
  print*,""



  print*,"Test do concurrent and sum"
  x = 0d0
  do concurrent(io=1:2,ik=1:10)
     x(io) = x(io) + 0.1d0
     print*,io,x(io)
  enddo


  G=zero
  print*,"G=0 -> G%is_zero:",G%is_zero()
  G = one
  print*,"G=1 -> G%is_zero:",G%is_zero()

  Gk = zero
  print*,"Gk=0  ->  Gk%is_zero:",Gk%is_zero()
  print*,"Gk=0,any(Gk%is_zero):",any(Gk%is_zero())
  print*,"Gk=0,all(Gk%is_zero):",all(Gk%is_zero())
  Gk(1::4) = one
  print*,"G(1::4)=1   ->    G%is_zero:",Gk%is_zero()
  print*,"Gk(1::4)=0, any(Gk%is_zero):",any(Gk%is_zero())
  print*,"Gk(1::4)=0, all(Gk%is_zero):",all(Gk%is_zero())
  Gk = one

  print*,"Gk=1  ->    Gk%is_zero:",Gk%is_zero()
  print*,"Gk=1,  any(Gk%is_zero):",any(Gk%is_zero())
  print*,"Gk=1,  all(Gk%is_zero):",all(Gk%is_zero())

  stop



end program test





