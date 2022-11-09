MODULE KB_GF_COMMON
  USE KB_CONTOUR
  USE KB_VARS_GLOBAL
  USE KB_AUX
  USE SCIFOR, only: one,xi,zero,pi,zeye,inv,assert_shape
  implicit none
  private



  ! KADANOFF-BAYM CONTOUR GREEN'S FUNCTIONS:
  type :: kb_gf
     complex(8),dimension(:,:),allocatable :: less
     complex(8),dimension(:,:),allocatable :: ret
     complex(8),dimension(:,:),allocatable :: lmix
     real(8),dimension(:),allocatable      :: mats
     complex(8),dimension(:),allocatable   :: iw
     logical                               :: status=.false.
   contains
     procedure :: init        => init_kb_gf_params   !constructor
     procedure :: extrapolate => extrapolate_kb_gf   !extrapolate
     procedure :: free        => free_kb_gf          !destructor
     procedure :: del         => del_kb_gf           !set to zero at the boundary
     procedure :: check       => check_kb_gf         !check allocation (return bool)
     procedure :: is_zero     => is_zero_kb_gf       !check if zero (return bool)
     final :: deallocate_kb_gf                       !destructor final
  end type kb_gf



  ! KADANOFF-BAYM CONTOUR GREEN'S FUNCTIONS DERIVATIVE
  type :: kb_dgf
     complex(8),dimension(:),allocatable :: less,gtr
     complex(8),dimension(:),allocatable :: ret
     complex(8),dimension(:),allocatable :: lmix
     logical                             :: status=.false.
   contains
     procedure :: init        => init_kb_dgf_params   !constructor
     procedure :: free        => free_kb_dgf          !destructor
     procedure :: del         => del_kb_dgf           !set to zero at the boundary
     procedure :: check       => check_kb_dgf         !check allocation (return bool)
     procedure :: is_zero     => is_zero_kb_dgf       !check if zero (return bool)
     final :: deallocate_kb_dgf                       !destructor final     
  end type kb_dgf


  interface operator (.plus.)
     module procedure :: kb_gf_plus_gf
     module procedure :: kb_dgf_plus_dgf
  end interface operator (.plus.)

  interface operator (.minus.)
     module procedure :: kb_gf_minus_gf
     module procedure :: kb_dgf_minus_dgf
  end interface operator (.minus.)

  interface operator (+)
     module procedure :: kb_gf_add_gf
     module procedure :: kb_dgf_add_dgf
  end interface operator (+)

  interface operator (-)
     module procedure :: kb_gf_subtract_gf
     module procedure :: kb_dgf_subtract_dgf
  end interface operator (-)


  ! PRODUCT WITH SCALAR (dble,complx):
  interface operator(*)
     module procedure :: kb_gf_left_times_int
     module procedure :: kb_gf_left_times_dble
     module procedure :: kb_gf_left_times_cmplx
     !
     module procedure :: kb_dgf_left_times_int
     module procedure :: kb_dgf_left_times_dble
     module procedure :: kb_dgf_left_times_cmplx
  end interface operator(*)


  ! DIVISION BY SCALAR (dble,complx):
  interface operator(/)
     module procedure :: kb_gf_right_division_int
     module procedure :: kb_gf_right_division_dble
     module procedure :: kb_gf_right_division_cmplx
     !
     module procedure :: kb_dgf_right_division_int
     module procedure :: kb_dgf_right_division_dble
     module procedure :: kb_dgf_right_division_cmplx
  end interface operator(/)


  !EQUALITY with scalar and function (G=F, G=c)
  interface assignment(=)
     module procedure :: kb_gf_equal_scalar
     module procedure :: kb_gf_equal_gf
     !
     module procedure :: kb_dgf_equal_scalar
     module procedure :: kb_dgf_equal_dgf
  end interface assignment(=)


  !ASSERT_SHAPE:
  interface assert_shape_kb_gf
     module procedure :: kb_gf_assert_shape_d1
     module procedure :: kb_gf_assert_shape_d2
     module procedure :: kb_gf_assert_shape_d3
     module procedure :: kb_gf_assert_shape_d4
     module procedure :: kb_gf_assert_shape_d5
     module procedure :: kb_gf_assert_shape_d6
     module procedure :: kb_gf_assert_shape_d7
     !
     module procedure :: kb_dgf_assert_shape_d1
     module procedure :: kb_dgf_assert_shape_d2
     module procedure :: kb_dgf_assert_shape_d3
     module procedure :: kb_dgf_assert_shape_d4
     module procedure :: kb_dgf_assert_shape_d5
     module procedure :: kb_dgf_assert_shape_d6
     module procedure :: kb_dgf_assert_shape_d7
  end interface assert_shape_kb_gf

  interface reshape_kb_gf
     module procedure :: kb_gf_reshape_nn2nso
     module procedure :: kb_gf_reshape_nso2nn
     module procedure :: kb_gf_reshape_nnn2nlso
     module procedure :: kb_gf_reshape_nlso2nnn
     !
     module procedure :: kb_dgf_reshape_nn2nso
     module procedure :: kb_dgf_reshape_nso2nn
     module procedure :: kb_dgf_reshape_nnn2nlso
     module procedure :: kb_dgf_reshape_nlso2nnn
  end interface reshape_kb_gf


  public :: kb_gf
  public :: kb_dgf
  !
  public :: operator(+)
  public :: operator(-)
  public :: operator(*)
  public :: operator(/)
  public :: operator(.plus.)
  public :: operator(.minus.)
  public :: assignment(=)
  public :: assert_shape_kb_gf
  public :: reshape_kb_gf
  public :: revert_kb_gf


  integer,public :: N1,N2,N3,N4,N5,N6,N7,Nk
  integer,public :: i1,i2,i3,i4,i5,i6,i7,ik
  integer,public :: Nlat,Nso,Nlso
  integer,public :: ilat,ispin,iorb,io
  integer,public :: jlat,jspin,jorb,jo
  integer,public :: klat,kspin,korb,ko



contains





  ! INIT
  elemental subroutine init_kb_gf_params(G)
    class(kb_gf),intent(inout) :: G
    integer                    :: N,L,Lf
    if(allocated(G%less))deallocate(G%less)
    if(allocated(G%ret)) deallocate(G%ret)
    if(allocated(G%lmix))deallocate(G%lmix)
    if(allocated(G%mats))deallocate(G%mats)
    if(allocated(G%iw))deallocate(G%iw)
    N = cc_params%Ntime            !<== allocate at maximum time
    L = cc_params%Ntau
    Lf= cc_params%Niw
    allocate(G%less(N,N))  ; G%less=zero
    allocate(G%ret(N,N))   ; G%ret=zero
    allocate(G%lmix(N,0:L)); G%lmix=zero
    allocate(G%mats(0:L))  ; G%mats=0d0
    allocate(G%iw(Lf))     ; G%iw=zero
    G%status=.true.    
  end subroutine init_kb_gf_params


  elemental subroutine init_kb_dgf_params(dG)
    class(kb_dgf),intent(inout) :: dG
    integer                     :: i,j,N,L
    !
    if(allocated(dG%less))deallocate(dG%less)
    if(allocated(dG%ret)) deallocate(dG%ret)
    if(allocated(dG%lmix))deallocate(dG%lmix)
    N=cc_params%Ntime           !<== allocate at maximum time
    L=cc_params%Ntau
    allocate(dG%less(N))  ; dG%less=zero
    allocate(dG%ret(N))   ; dG%ret=zero
    allocate(dG%lmix(0:L)); dG%lmix=zero
    ! allocate(dG%gtr(N))   ; dG%gtr=zero
    dG%status=.true.
  end subroutine init_kb_dgf_params



  !FREE
  elemental subroutine free_kb_gf(G)
    class(kb_gf),intent(inout) :: G
    if(allocated(G%less))deallocate(G%less)
    if(allocated(G%ret)) deallocate(G%ret)
    if(allocated(G%lmix))deallocate(G%lmix)
    if(allocated(G%mats))deallocate(G%mats)
    if(allocated(G%iw))deallocate(G%iw)
    G%status=.false.
  end subroutine free_kb_gf

  elemental subroutine free_kb_dgf(dG)
    class(kb_dgf),intent(inout) :: dG
    if(allocated(dG%less))deallocate(dG%less)
    if(allocated(dG%ret)) deallocate(dG%ret)
    if(allocated(dG%lmix))deallocate(dG%lmix)
    if(allocated(dG%gtr))deallocate(dG%gtr)
    dG%status=.false.
  end subroutine free_kb_dgf




  elemental subroutine del_kb_gf(G)
    class(kb_gf),intent(inout) :: G
    integer                    :: N
    N   = cc_params%Nt   !<== work with the ACTUAL size of the contour
    if(N==1)then
       G%iw   = zero
       G%mats = 0d0
    endif
    G%ret(N,1:N)   = zero
    G%less(N,1:N)  = zero
    G%lmix(N,0:)   = zero
  end subroutine del_kb_gf

  elemental subroutine del_kb_dgf(dG)
    class(kb_dgf),intent(inout) :: dG
    integer                     :: N
    N   = cc_params%Nt   !<== work with the ACTUAL size of the contour
    dG%ret(1:N)   = zero
    dG%less(1:N)  = zero
    dG%lmix(0:)   = zero
  end subroutine del_kb_dgf


  !DESTRUCTOR
  subroutine deallocate_kb_gf(G)
    type(kb_gf) :: G
    if(allocated(G%less))deallocate(G%less)
    if(allocated(G%ret)) deallocate(G%ret)
    if(allocated(G%lmix))deallocate(G%lmix)
    if(allocated(G%mats))deallocate(G%mats)
    if(allocated(G%iw))deallocate(G%iw)
    G%status=.false.
  end subroutine deallocate_kb_gf

  subroutine deallocate_kb_dgf(dG)
    type(kb_dgf) :: dG
    if(allocated(dG%less))deallocate(dG%less)
    if(allocated(dG%ret)) deallocate(dG%ret)
    if(allocated(dG%lmix))deallocate(dG%lmix)
    if(allocated(dG%gtr))deallocate(dG%gtr)
    dG%status=.false.
  end subroutine deallocate_kb_dgf




  !################################################################################
  !################################################################################
  !################################################################################
  !################################################################################
  !################################################################################


  !C(t,t')=A(t,t') +/- B(t,t'), with t,t'=0,t_max
  !INTERFACE OPERATORS .PLUS., .MINUS.
  !PLUS
  elemental function kb_gf_plus_gf(a,b) result(c)
    type(kb_gf), intent(in) :: a,b
    type(kb_gf)             :: c
    call c%init()
    c%less = a%less + b%less
    c%ret  = a%ret  + b%ret
    c%lmix = a%lmix + b%lmix
    c%mats = a%mats + b%mats
    c%iw   = a%iw   + b%iw
  end function kb_gf_plus_gf

  !MINUS
  elemental function kb_gf_minus_gf(a,b) result(c)
    type(kb_gf), intent(in) :: a,b
    type(kb_gf)             :: c
    call c%init()
    c%less = a%less - b%less
    c%ret  = a%ret  - b%ret
    c%lmix = a%lmix - b%lmix
    c%mats = a%mats - b%mats
    c%iw   = a%iw   - b%iw
  end function kb_gf_minus_gf





  elemental function kb_dgf_plus_dgf(a,b) result(c)
    type(kb_dgf), intent(in) :: a,b
    type(kb_dgf)             :: c
    call c%init()
    c%less = a%less + b%less
    c%ret  = a%ret  + b%ret
    c%lmix = a%lmix + b%lmix
  end function kb_dgf_plus_dgf

  !MINUS
  elemental function kb_dgf_minus_dgf(a,b) result(c)
    type(kb_dgf), intent(in) :: a,b
    type(kb_dgf)             :: c
    call c%init()
    c%less = a%less - b%less
    c%ret  = a%ret  - b%ret
    c%lmix = a%lmix - b%lmix
  end function kb_dgf_minus_dgf


  !################################################################################
  !################################################################################
  !################################################################################
  !################################################################################
  !################################################################################



  !C(t,t')=A(t,t') +/- B(t,t') along contour, t=t_max && t'=0,t_max
  !INTERFACE OPERATORS +, -
  !ADD:
  elemental function kb_gf_add_gf(a,b) result(c)
    type(kb_gf),intent(in)  :: a,b
    type(kb_gf)             :: c
    integer                 :: N
    N   = cc_params%Nt   !<== work with the ACTUAL size of the contour
    call c%init()
    if(N==1)then
       C%mats(0:) = A%mats(0:) + B%mats(0:)
       C%iw(:)    = A%iw(:)    + B%iw(:)
    endif
    C%ret(N,1:N)   = A%ret(N,1:N)   + B%ret(N,1:N)
    C%less(N,1:N)  = A%less(N,1:N)  + B%less(N,1:N)
    C%lmix(N,0:)   = A%lmix(N,0:)   + B%lmix(N,0:)
    ! !THIS SHOULD NOT BE INVOLVED IN THE CALCULATION:
    ! C%less(1:N-1,N)= -conjg(C%less(N,1:N-1))
  end function kb_gf_add_gf

  !SUBTRACT:
  elemental function kb_gf_subtract_gf(a,b) result(c)
    type(kb_gf), intent(in) :: a,b
    type(kb_gf)             :: c
    integer                 :: N
    N   = cc_params%Nt   !<== work with the ACTUAL size of the contour
    call c%init()
    if(N==1)then
       C%mats(0:) = A%mats(0:) - B%mats(0:)
       C%iw(:)    = A%iw(:)    - B%iw(:)
    endif
    C%ret(N,1:N)   = A%ret(N,1:N)   - B%ret(N,1:N)
    C%less(N,1:N)  = A%less(N,1:N)  - B%less(N,1:N)
    C%lmix(N,0:)   = A%lmix(N,0:)   - B%lmix(N,0:)
    ! !THIS SHOULD NOT BE INVOLVED IN THE CALCULATION:
    ! C%less(1:N-1,N)= -conjg(C%less(N,1:N-1))
  end function kb_gf_subtract_gf



  elemental function kb_dgf_add_dgf(a,b) result(c)
    type(kb_dgf),intent(in)  :: a,b
    type(kb_dgf)             :: c
    integer                 :: N
    N   = cc_params%Nt   !<== work with the ACTUAL size of the contour
    call c%init()
    C%ret(1:N)   = A%ret(1:N)   + B%ret(1:N)
    C%less(1:N)  = A%less(1:N)  + B%less(1:N)
    C%lmix(0:)   = A%lmix(0:)   + B%lmix(0:)
  end function kb_dgf_add_dgf

  !SUBTRACT:
  elemental function kb_dgf_subtract_dgf(a,b) result(c)
    type(kb_dgf), intent(in) :: a,b
    type(kb_dgf)             :: c
    integer                 :: N
    N   = cc_params%Nt   !<== work with the ACTUAL size of the contour
    call c%init()
    C%ret(1:N)   = A%ret(1:N)   - B%ret(1:N)
    C%less(1:N)  = A%less(1:N)  - B%less(1:N)
    C%lmix(0:)   = A%lmix(0:)   - B%lmix(0:)
  end function kb_dgf_subtract_dgf




  !################################################################################
  !################################################################################
  !################################################################################
  !################################################################################
  !################################################################################





  !EQUALITY with scalar and function (G=F, G=c)
  elemental subroutine kb_gf_equal_scalar(A,C)
    type(kb_gf),intent(inout) :: A
    complex(8),intent(in)     :: C
    A%less = C
    A%ret  = C
    A%lmix = C
    A%mats = C
    A%iw   = C
  end subroutine kb_gf_equal_scalar

  elemental subroutine kb_gf_equal_gf(A,B)
    type(kb_gf),intent(inout) :: A
    type(kb_gf),intent(in)    :: B
    A%less(:,:)  = B%less(:,:)
    A%ret(:,:)   = B%ret(:,:)
    A%lmix(:,0:) = B%lmix(:,0:)
    A%mats(0:)   = B%mats(0:)
    A%iw(:)      = B%iw(:)
  end subroutine kb_gf_equal_gf



  elemental subroutine kb_dgf_equal_scalar(A,C)
    type(kb_dgf),intent(inout) :: A
    complex(8),intent(in)     :: C
    A%less = C
    A%ret  = C
    A%lmix = C
  end subroutine kb_dgf_equal_scalar

  elemental subroutine kb_dgf_equal_dgf(A,B)
    type(kb_dgf),intent(inout) :: A
    type(kb_dgf),intent(in)    :: B
    A%less(:)  = B%less(:)
    A%ret(:)   = B%ret(:)
    A%lmix(0:) = B%lmix(0:)
  end subroutine kb_dgf_equal_dgf


  ! !##################################################################
  ! !##################################################################
  ! !##################################################################
  ! !##################################################################



  ! PRODUCT WITH SCALAR (int,dble,complx):
  elemental function kb_gf_left_times_int(C,Gin) result(Gout)
    integer,intent(in)     :: C
    type(kb_gf),intent(in) :: Gin
    type(kb_gf)            :: Gout
    call Gout%init
    Gout%less(:,:) = C*Gin%less(:,:)
    Gout%ret(:,:)  = C*Gin%ret(:,:)
    Gout%lmix(:,0:)= C*Gin%lmix(:,0:)
    Gout%mats(0:)  = C*Gin%mats(0:)
    Gout%iw(:)     = C*Gin%iw(:)
  end function kb_gf_left_times_int

  elemental function kb_gf_left_times_dble(C,Gin) result(Gout)
    real(8),intent(in)     :: C
    type(kb_gf),intent(in) :: Gin
    type(kb_gf)            :: Gout
    call Gout%init
    Gout%less(:,:) = C*Gin%less(:,:)
    Gout%ret(:,:)  = C*Gin%ret(:,:)
    Gout%lmix(:,0:)= C*Gin%lmix(:,0:)
    Gout%mats(0:)  = C*Gin%mats(0:)
    Gout%iw(:)     = C*Gin%iw(:)
  end function kb_gf_Left_times_dble

  elemental function kb_gf_left_times_cmplx(C,Gin) result(Gout)
    complex(8),intent(in)  :: C
    type(kb_gf),intent(in) :: Gin
    type(kb_gf)            :: Gout
    call Gout%init
    Gout%less(:,:) = C*Gin%less(:,:)
    Gout%ret(:,:)  = C*Gin%ret(:,:)
    Gout%lmix(:,0:)= C*Gin%lmix(:,0:)
    Gout%mats(0:)  = C*Gin%mats(0:)
    Gout%iw(:)     = C*Gin%iw(:)
  end function kb_gf_left_times_cmplx



  ! PRODUCT WITH SCALAR (int,dble,complx):
  elemental function kb_dgf_left_times_int(C,Gin) result(Gout)
    integer,intent(in)     :: C
    type(kb_dgf),intent(in) :: Gin
    type(kb_dgf)            :: Gout
    call Gout%init
    Gout%less(:) = C*Gin%less(:)
    Gout%ret(:)  = C*Gin%ret(:)
    Gout%lmix(0:)= C*Gin%lmix(0:)
  end function kb_dgf_left_times_int

  elemental function kb_dgf_left_times_dble(C,Gin) result(Gout)
    real(8),intent(in)     :: C
    type(kb_dgf),intent(in) :: Gin
    type(kb_dgf)            :: Gout
    call Gout%init
    Gout%less(:) = C*Gin%less(:)
    Gout%ret(:)  = C*Gin%ret(:)
    Gout%lmix(0:)= C*Gin%lmix(0:)
  end function kb_dgf_Left_times_dble

  elemental function kb_dgf_left_times_cmplx(C,Gin) result(Gout)
    complex(8),intent(in)  :: C
    type(kb_dgf),intent(in) :: Gin
    type(kb_dgf)            :: Gout
    call Gout%init
    Gout%less(:) = C*Gin%less(:)
    Gout%ret(:)  = C*Gin%ret(:)
    Gout%lmix(0:)= C*Gin%lmix(0:)
  end function kb_dgf_left_times_cmplx




  !################################################################################
  !################################################################################
  !################################################################################
  !################################################################################
  !################################################################################



  ! DIVISION BY SCALAR (int,dble,complx):
  elemental function kb_gf_right_division_int(Gin,C) result(Gout)
    integer,intent(in)     :: C
    type(kb_gf),intent(in) :: Gin
    type(kb_gf)            :: Gout
    call Gout%init
    Gout%less(:,:) = Gin%less(:,:)/C
    Gout%ret(:,:)  = Gin%ret(:,:)/C
    Gout%lmix(:,0:)= Gin%lmix(:,0:)/C
    Gout%mats(0:)  = Gin%mats(0:)/C
    Gout%iw(:)     = Gin%iw(:)/C
  end function kb_gf_right_division_int

  elemental function kb_gf_right_division_dble(Gin,C) result(Gout)
    real(8),intent(in)     :: C
    type(kb_gf),intent(in) :: Gin
    type(kb_gf)            :: Gout
    call Gout%init
    Gout%less(:,:) = Gin%less(:,:)/C
    Gout%ret(:,:)  = Gin%ret(:,:)/C
    Gout%lmix(:,0:)= Gin%lmix(:,0:)/C
    Gout%mats(0:)  = Gin%mats(0:)/C
    Gout%iw(:)     = Gin%iw(:)/C
  end function kb_gf_right_division_dble

  elemental function kb_gf_right_division_cmplx(Gin,C) result(Gout)
    complex(8),intent(in)  :: C
    type(kb_gf),intent(in) :: Gin
    type(kb_gf)            :: Gout
    call Gout%init
    Gout%less(:,:) = Gin%less(:,:)/C
    Gout%ret(:,:)  = Gin%ret(:,:)/C
    Gout%lmix(:,0:)= Gin%lmix(:,0:)/C
    Gout%mats(0:)  = Gin%mats(0:)/C
    Gout%iw(:)     = Gin%iw(:)/C
  end function kb_gf_right_division_cmplx




  ! DIVISION BY SCALAR (int,dble,complx):
  elemental function kb_dgf_right_division_int(Gin,C) result(Gout)
    integer,intent(in)     :: C
    type(kb_dgf),intent(in) :: Gin
    type(kb_dgf)            :: Gout
    call Gout%init
    Gout%less(:) = Gin%less(:)/C
    Gout%ret(:)  = Gin%ret(:)/C
    Gout%lmix(0:)= Gin%lmix(0:)/C
  end function kb_dgf_right_division_int

  elemental function kb_dgf_right_division_dble(Gin,C) result(Gout)
    real(8),intent(in)     :: C
    type(kb_dgf),intent(in) :: Gin
    type(kb_dgf)            :: Gout
    call Gout%init
    Gout%less(:) = Gin%less(:)/C
    Gout%ret(:)  = Gin%ret(:)/C
    Gout%lmix(0:)= Gin%lmix(0:)/C
  end function kb_dgf_right_division_dble

  elemental function kb_dgf_right_division_cmplx(Gin,C) result(Gout)
    complex(8),intent(in)  :: C
    type(kb_dgf),intent(in) :: Gin
    type(kb_dgf)            :: Gout
    call Gout%init
    Gout%less(:) = Gin%less(:)/C
    Gout%ret(:)  = Gin%ret(:)/C
    Gout%lmix(0:)= Gin%lmix(0:)/C
  end function kb_dgf_right_division_cmplx



  !##################################################################
  !##################################################################
  !##################################################################
  !##################################################################


  elemental function check_kb_gf(G) result(bool)
    class(kb_gf),intent(in) :: G
    logical                 :: bool
    integer                 :: N,L,Lf
    logical                 :: check(4)
    !
    N=cc_params%Ntime              !<== check size at maximum time
    L=cc_params%Ntau
    !
    check(1) = any(shape(G%less)/=[N,N])
    check(2) = any(shape(G%ret)/=[N,N])
    check(3) = any(shape(G%lmix)/=[N,L+1])
    check(4) = any(shape(G%mats)/=[L+1])
    bool = .not.any(check)
  end function check_kb_gf



  elemental function check_kb_dgf(G) result(bool)
    class(kb_dgf),intent(in) :: G
    logical                  :: bool
    integer                  :: N,L,Lf
    logical                  :: check(4)
    !
    N=cc_params%Ntime              !<== check size at maximum time
    L=cc_params%Ntau
    !
    check(1) = any(shape(G%less)/=[N])
    check(2) = any(shape(G%ret)/=[N])
    check(3) = any(shape(G%lmix)/=[L+1])
    bool = .not.any(check)
  end function check_kb_dgf


  !##################################################################
  !##################################################################
  !##################################################################
  !##################################################################


  elemental function is_zero_kb_gf(G) result(bool)
    class(kb_gf),intent(in) :: G
    logical                 :: bool
    logical                 :: b(4)
    !
    b(1) = all(G%less==zero) !T: G==0, F: G/=0
    b(2) = all(G%ret==zero)
    b(3) = all(G%lmix==zero)
    b(4) = all(G%mats==zero)
    bool = b(1).AND.b(2).AND.b(3).AND.b(4)
  end function is_zero_kb_gf



  elemental function is_zero_kb_dgf(G) result(bool)
    class(kb_dgf),intent(in) :: G
    logical                  :: bool
    logical                  :: b(3)
    !
    b(1) = all(G%less==zero)
    b(2) =  all(G%ret==zero)
    b(3) = all(G%lmix==zero)
    bool = b(1).AND.b(2).AND.b(3)
  end function is_zero_kb_dgf




  !##################################################################
  !##################################################################
  !##################################################################
  !##################################################################

  !EXTRAPOLATE
  elemental subroutine extrapolate_kb_gf(G)
    class(kb_gf),intent(inout) :: G
    integer                    :: i,j,k,N,L
    N   = cc_params%Nt      !<== work with the ACTUAL size of the contour
    L   = cc_params%Ntau
    select case(N)
    case(1)
       return
    case(2)
       !GUESS G AT THE NEXT STEP, GIVEN THE INITIAL CONDITIONS
       do j=1,N
          g%ret(N,j) =g%ret(1,1)
          g%less(N,j)=g%less(1,1)
       end do
       do i=1,N-1
          g%less(i,N)=g%less(1,1)
       end do
       do j=0,L
          g%lmix(N,j)=g%lmix(1,j)
       end do
    case default
       !EXTEND G FROM THE [N-1,N-1] TO THE [N,N] SQUARE TO START DMFT
       !USING QUADRATIC EXTRAPOLATION
       do k=1,N-1
          g%less(N,k)=2.d0*g%less(N-1,k)-g%less(N-2,k)
          g%less(k,N)=2.d0*g%less(k,N-1)-g%less(k,N-2)
       end do
       g%less(N,N)=2.d0*g%less(N-1,N-1)-g%less(N-2,N-2)
       !
       do k=0,L
          g%lmix(N,k)=2.d0*g%lmix(N-1,k)-g%lmix(N-2,k)
       end do
       !
       g%ret(N,N)=-xi
       do k=1,N-2
          g%ret(N,k)=2.d0*g%ret(N-1,k)-g%ret(N-2,k)
       end do
       g%ret(N,N-1)=0.5d0*(g%ret(N,N)+g%ret(N,N-2))
    end select
  end subroutine extrapolate_kb_gf





  !##################################################################
  !##################################################################
  !##################################################################
  !##################################################################


  elemental subroutine revert_kb_gf(C,coeff)
    type(kb_gf),intent(inout) :: C
    complex(8),intent(in)     :: coeff
    integer                   :: N,L
    N   = cc_params%Nt      !<== work with the ACTUAL size of the contour
    L   = cc_params%Ntau
    C%lmix(N,0:L)  = coeff*C%lmix(N,0:L)
    C%less(N,1:N-1)= coeff*C%less(N,1:N-1)
    C%less(1:N,N)  = coeff*C%less(1:N,N)
    C%ret(N,1:N)   = coeff*C%ret(N,1:N)
  end subroutine revert_kb_gf


  !##################################################################
  !##################################################################
  !##################################################################
  !##################################################################




  function kb_gf_reshape_nn2nso(A,Nspin,Norb) result(B)
    integer,intent(in)                                      :: Nspin,Norb
    type(kb_gf),dimension(Nspin,Nspin,Norb,Norb),intent(in) :: A
    type(kb_gf),dimension(Nspin*Norb,Nspin*Norb)            :: B
    integer                                                 :: ispin,iorb,jspin,jorb
    call B%init()
    do concurrent (ispin=1:Nspin,jspin=1:Norb,iorb=1:Norb,jorb=1:Norb)
       io = iorb + (ispin-1)*Norb
       jo = jorb + (jspin-1)*Norb
       B(io,jo) = A(ispin,jspin,iorb,jorb)
    enddo
  end function kb_gf_reshape_nn2nso

  function kb_gf_reshape_nso2nn(A,Nspin,Norb) result(B)
    integer,intent(in)                                      :: Nspin,Norb
    type(kb_gf),dimension(Nspin*Norb,Nspin*Norb),intent(in) :: A
    type(kb_gf),dimension(Nspin,Nspin,Norb,Norb)            :: B
    integer                                                 :: ispin,iorb,jspin,jorb
    call B%init()
    do concurrent (ispin=1:Nspin,jspin=1:Norb,iorb=1:Norb,jorb=1:Norb)
       io = iorb + (ispin-1)*Norb
       jo = jorb + (jspin-1)*Norb
       B(ispin,jspin,iorb,jorb) = A(io,jo)
    enddo
  end function kb_gf_reshape_nso2nn


  function kb_gf_reshape_nnn2nlso(A,Nlat,Nspin,Norb) result(B)
    integer,intent(in)                                                :: Nlat,Nspin,Norb
    type(kb_gf),dimension(Nlat,Nlat,Nspin,Nspin,Norb,Norb),intent(in) :: A
    type(kb_gf),dimension(Nlat*Nspin*Norb,Nlat*Nspin*Norb)            :: B
    integer                                                           :: ilat,jlat,ispin,iorb,jspin,jorb
    call B%init()
    do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Norb,iorb=1:Norb,jorb=1:Norb)
       io = iorb + (ispin-1)*Norb + (ilat-1)*Norb*Nspin
       jo = jorb + (jspin-1)*Norb + (jlat-1)*Norb*Nspin
       B(io,jo) = A(ilat,jlat,ispin,jspin,iorb,jorb)
    enddo
  end function kb_gf_reshape_nnn2nlso

  function kb_gf_reshape_nlso2nnn(A,Nlat,Nspin,Norb) result(B)
    integer,intent(in)                                                :: Nlat,Nspin,Norb
    type(kb_gf),dimension(Nlat*Nspin*Norb,Nlat*Nspin*Norb),intent(in) :: A
    type(kb_gf),dimension(Nlat,Nlat,Nspin,Nspin,Norb,Norb)            :: B
    integer                                                           :: ilat,jlat,ispin,iorb,jspin,jorb
    call B%init()
    do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Norb,iorb=1:Norb,jorb=1:Norb)
       io = iorb + (ispin-1)*Norb + (ilat-1)*Norb*Nspin
       jo = jorb + (jspin-1)*Norb + (jlat-1)*Norb*Nspin
       B(ilat,jlat,ispin,jspin,iorb,jorb) = A(io,jo)
    enddo
  end function kb_gf_reshape_nlso2nnn









  function kb_dgf_reshape_nn2nso(A,Nspin,Norb) result(B)
    integer,intent(in)                                       :: Nspin,Norb
    type(kb_dgf),dimension(Nspin,Nspin,Norb,Norb),intent(in) :: A
    type(kb_dgf),dimension(Nspin*Norb,Nspin*Norb)            :: B
    integer                                                  :: ispin,iorb,jspin,jorb
    call B%init()
    do concurrent (ispin=1:Nspin,jspin=1:Norb,iorb=1:Norb,jorb=1:Norb)
       io = iorb + (ispin-1)*Norb
       jo = jorb + (jspin-1)*Norb
       B(io,jo) = A(ispin,jspin,iorb,jorb)
    enddo
  end function kb_dgf_reshape_nn2nso

  function kb_dgf_reshape_nso2nn(A,Nspin,Norb) result(B)
    integer,intent(in)                                       :: Nspin,Norb
    type(kb_dgf),dimension(Nspin*Norb,Nspin*Norb),intent(in) :: A
    type(kb_dgf),dimension(Nspin,Nspin,Norb,Norb)            :: B
    integer                                                  :: ispin,iorb,jspin,jorb
    call B%init()
    do concurrent (ispin=1:Nspin,jspin=1:Norb,iorb=1:Norb,jorb=1:Norb)
       io = iorb + (ispin-1)*Norb
       jo = jorb + (jspin-1)*Norb
       B(ispin,jspin,iorb,jorb) = A(io,jo)
    enddo
  end function kb_dgf_reshape_nso2nn


  function kb_dgf_reshape_nnn2nlso(A,Nlat,Nspin,Norb) result(B)
    integer,intent(in)                                                :: Nlat,Nspin,Norb
    type(kb_dgf),dimension(Nlat,Nlat,Nspin,Nspin,Norb,Norb),intent(in) :: A
    type(kb_dgf),dimension(Nlat*Nspin*Norb,Nlat*Nspin*Norb)            :: B
    integer                                                           :: ilat,jlat,ispin,iorb,jspin,jorb
    call B%init()
    do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Norb,iorb=1:Norb,jorb=1:Norb)
       io = iorb + (ispin-1)*Norb + (ilat-1)*Norb*Nspin
       jo = jorb + (jspin-1)*Norb + (jlat-1)*Norb*Nspin
       B(io,jo) = A(ilat,jlat,ispin,jspin,iorb,jorb)
    enddo
  end function kb_dgf_reshape_nnn2nlso

  function kb_dgf_reshape_nlso2nnn(A,Nlat,Nspin,Norb) result(B)
    integer,intent(in)                                                :: Nlat,Nspin,Norb
    type(kb_dgf),dimension(Nlat*Nspin*Norb,Nlat*Nspin*Norb),intent(in) :: A
    type(kb_dgf),dimension(Nlat,Nlat,Nspin,Nspin,Norb,Norb)            :: B
    integer                                                           :: ilat,jlat,ispin,iorb,jspin,jorb
    call B%init()
    do concurrent (ilat=1:Nlat,jlat=1:Nlat,ispin=1:Nspin,jspin=1:Norb,iorb=1:Norb,jorb=1:Norb)
       io = iorb + (ispin-1)*Norb + (ilat-1)*Norb*Nspin
       jo = jorb + (jspin-1)*Norb + (jlat-1)*Norb*Nspin
       B(ilat,jlat,ispin,jspin,iorb,jorb) = A(io,jo)
    enddo
  end function kb_dgf_reshape_nlso2nnn






  !##################################################################
  !##################################################################
  !##################################################################
  !##################################################################





  subroutine kb_gf_assert_shape_d1(A,Ndim,routine,matname)
    type(kb_gf),dimension(:),intent(in)             :: A
    integer,dimension(:),intent(in)                 :: Ndim
    character(len=*),optional                       :: routine, matname
    if(any(shape(A) /= Ndim)) then
       if(present(routine).AND.present(matname))&
            write(*,"(A,10I2)")trim(routine)//" error: "//trim(matname)//" has illegal shape"
       stop "assert_shape error: wrong matrix shape"
    end if
  end subroutine kb_gf_assert_shape_d1
  subroutine kb_gf_assert_shape_d2(A,Ndim,routine,matname)
    type(kb_gf),dimension(:,:),intent(in)           :: A
    integer,dimension(:),intent(in)                 :: Ndim
    character(len=*),optional                       :: routine, matname
    if(any(shape(A) /= Ndim)) then
       if(present(routine).AND.present(matname))&
            write(*,"(A,10I2)")trim(routine)//" error: "//trim(matname)//" has illegal shape"
       stop "assert_shape error: wrong matrix shape"
    end if
  end subroutine kb_gf_assert_shape_d2
  subroutine kb_gf_assert_shape_d3(A,Ndim,routine,matname)
    type(kb_gf),dimension(:,:,:),intent(in)         :: A
    integer,dimension(:),intent(in)                 :: Ndim
    character(len=*),optional                       :: routine, matname
    if(any(shape(A) /= Ndim)) then
       if(present(routine).AND.present(matname))&
            write(*,"(A,10I2)")trim(routine)//" error: "//trim(matname)//" has illegal shape"
       stop "assert_shape error: wrong matrix shape"
    end if
  end subroutine kb_gf_assert_shape_d3
  subroutine kb_gf_assert_shape_d4(A,Ndim,routine,matname)
    type(kb_gf),dimension(:,:,:,:),intent(in)       :: A
    integer,dimension(:),intent(in)                 :: Ndim
    character(len=*),optional                       :: routine, matname
    if(any(shape(A) /= Ndim)) then
       if(present(routine).AND.present(matname))&
            write(*,"(A,10I2)")trim(routine)//" error: "//trim(matname)//" has illegal shape"
       stop "assert_shape error: wrong matrix shape"
    end if
  end subroutine kb_gf_assert_shape_d4
  subroutine kb_gf_assert_shape_d5(A,Ndim,routine,matname)
    type(kb_gf),dimension(:,:,:,:,:),intent(in)     :: A
    integer,dimension(:),intent(in)                 :: Ndim
    character(len=*),optional                       :: routine, matname
    if(any(shape(A) /= Ndim)) then
       if(present(routine).AND.present(matname))&
            write(*,"(A,10I2)")trim(routine)//" error: "//trim(matname)//" has illegal shape"
       stop "assert_shape error: wrong matrix shape"
    end if
  end subroutine kb_gf_assert_shape_d5
  subroutine kb_gf_assert_shape_d6(A,Ndim,routine,matname)
    type(kb_gf),dimension(:,:,:,:,:,:),intent(in)   :: A
    integer,dimension(:),intent(in)                 :: Ndim
    character(len=*),optional                       :: routine, matname
    if(any(shape(A) /= Ndim)) then
       if(present(routine).AND.present(matname))&
            write(*,"(A,10I2)")trim(routine)//" error: "//trim(matname)//" has illegal shape"
       stop "assert_shape error: wrong matrix shape"
    end if
  end subroutine kb_gf_assert_shape_d6
  subroutine kb_gf_assert_shape_d7(A,Ndim,routine,matname)
    type(kb_gf),dimension(:,:,:,:,:,:,:),intent(in) :: A
    integer,dimension(:),intent(in)                 :: Ndim
    character(len=*),optional                       :: routine, matname
    if(any(shape(A) /= Ndim)) then
       if(present(routine).AND.present(matname))&
            write(*,"(A,10I2)")trim(routine)//" error: "//trim(matname)//" has illegal shape"
       stop "assert_shape error: wrong matrix shape"
    end if
  end subroutine kb_gf_assert_shape_d7







  subroutine kb_dgf_assert_shape_d1(A,Ndim,routine,matname)
    type(kb_dgf),dimension(:),intent(in)             :: A
    integer,dimension(:),intent(in)                 :: Ndim
    character(len=*),optional                       :: routine, matname
    if(any(shape(A) /= Ndim)) then
       if(present(routine).AND.present(matname))&
            write(*,"(A,10I2)")trim(routine)//" error: "//trim(matname)//" has illegal shape"
       stop "assert_shape error: wrong matrix shape"
    end if
  end subroutine kb_dgf_assert_shape_d1
  subroutine kb_dgf_assert_shape_d2(A,Ndim,routine,matname)
    type(kb_dgf),dimension(:,:),intent(in)           :: A
    integer,dimension(:),intent(in)                 :: Ndim
    character(len=*),optional                       :: routine, matname
    if(any(shape(A) /= Ndim)) then
       if(present(routine).AND.present(matname))&
            write(*,"(A,10I2)")trim(routine)//" error: "//trim(matname)//" has illegal shape"
       stop "assert_shape error: wrong matrix shape"
    end if
  end subroutine kb_dgf_assert_shape_d2
  subroutine kb_dgf_assert_shape_d3(A,Ndim,routine,matname)
    type(kb_dgf),dimension(:,:,:),intent(in)         :: A
    integer,dimension(:),intent(in)                 :: Ndim
    character(len=*),optional                       :: routine, matname
    if(any(shape(A) /= Ndim)) then
       if(present(routine).AND.present(matname))&
            write(*,"(A,10I2)")trim(routine)//" error: "//trim(matname)//" has illegal shape"
       stop "assert_shape error: wrong matrix shape"
    end if
  end subroutine kb_dgf_assert_shape_d3
  subroutine kb_dgf_assert_shape_d4(A,Ndim,routine,matname)
    type(kb_dgf),dimension(:,:,:,:),intent(in)       :: A
    integer,dimension(:),intent(in)                 :: Ndim
    character(len=*),optional                       :: routine, matname
    if(any(shape(A) /= Ndim)) then
       if(present(routine).AND.present(matname))&
            write(*,"(A,10I2)")trim(routine)//" error: "//trim(matname)//" has illegal shape"
       stop "assert_shape error: wrong matrix shape"
    end if
  end subroutine kb_dgf_assert_shape_d4
  subroutine kb_dgf_assert_shape_d5(A,Ndim,routine,matname)
    type(kb_dgf),dimension(:,:,:,:,:),intent(in)     :: A
    integer,dimension(:),intent(in)                 :: Ndim
    character(len=*),optional                       :: routine, matname
    if(any(shape(A) /= Ndim)) then
       if(present(routine).AND.present(matname))&
            write(*,"(A,10I2)")trim(routine)//" error: "//trim(matname)//" has illegal shape"
       stop "assert_shape error: wrong matrix shape"
    end if
  end subroutine kb_dgf_assert_shape_d5
  subroutine kb_dgf_assert_shape_d6(A,Ndim,routine,matname)
    type(kb_dgf),dimension(:,:,:,:,:,:),intent(in)   :: A
    integer,dimension(:),intent(in)                 :: Ndim
    character(len=*),optional                       :: routine, matname
    if(any(shape(A) /= Ndim)) then
       if(present(routine).AND.present(matname))&
            write(*,"(A,10I2)")trim(routine)//" error: "//trim(matname)//" has illegal shape"
       stop "assert_shape error: wrong matrix shape"
    end if
  end subroutine kb_dgf_assert_shape_d6
  subroutine kb_dgf_assert_shape_d7(A,Ndim,routine,matname)
    type(kb_dgf),dimension(:,:,:,:,:,:,:),intent(in) :: A
    integer,dimension(:),intent(in)                 :: Ndim
    character(len=*),optional                       :: routine, matname
    if(any(shape(A) /= Ndim)) then
       if(present(routine).AND.present(matname))&
            write(*,"(A,10I2)")trim(routine)//" error: "//trim(matname)//" has illegal shape"
       stop "assert_shape error: wrong matrix shape"
    end if
  end subroutine kb_dgf_assert_shape_d7

END MODULE KB_GF_COMMON















! !##################################################################
! !##################################################################
! !##################################################################
! !##################################################################


! !Constructor: this conflicts with assignment(=) overload
! interface kb_gf
!    module procedure :: new_kb_gf_int
!    module procedure :: new_kb_gf_params
! end interface kb_gf

! !CONSTRUCTOR: THIS *CONFLICTS WITH ASSIGNMENT(=) OVERLOAD*
! elemental function new_kb_gf_int(N,L,Lf) result(G)
!   type(kb_gf)        :: G
!   integer,intent(in) :: N,L,Lf
!   if(allocated(G%less))deallocate(G%less)
!   if(allocated(G%ret)) deallocate(G%ret)
!   if(allocated(G%lmix))deallocate(G%lmix)
!   if(allocated(G%mats))deallocate(G%mats)
!   if(allocated(G%iw))deallocate(G%iw)
!   allocate(G%less(N,N))  ; G%less=zero
!   allocate(G%ret(N,N))   ; G%ret=zero
!   allocate(G%lmix(N,0:L)); G%lmix=zero
!   allocate(G%mats(0:L))  ; G%mats=0d0
!   allocate(G%iw(Lf))     ; G%iw=zero
!   G%status=.true.    
! end function new_kb_gf_int

! elemental function new_kb_gf_params() result(G)
!   type(kb_gf) :: G
!   integer     :: N,L,Lf
!   if(allocated(G%less))deallocate(G%less)
!   if(allocated(G%ret)) deallocate(G%ret)
!   if(allocated(G%lmix))deallocate(G%lmix)
!   if(allocated(G%mats))deallocate(G%mats)
!   if(allocated(G%iw))deallocate(G%iw)
!   N = cc_params%Ntime            !<== allocate at maximum time
!   L = cc_params%Ntau
!   Lf= cc_params%Niw
!   allocate(G%less(N,N))  ; G%less=zero
!   allocate(G%ret(N,N))   ; G%ret=zero
!   allocate(G%lmix(N,0:L)); G%lmix=zero
!   allocate(G%mats(0:L))  ; G%mats=0d0
!   allocate(G%iw(Lf))     ; G%iw=zero
!   G%status=.true.    
! end function new_kb_gf_params
