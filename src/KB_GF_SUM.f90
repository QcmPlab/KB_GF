MODULE KB_GF_SUM
  USE KB_VARS_GLOBAL
  USE KB_CONTOUR
  USE KB_AUX
  USE KB_GF_COMMON
  USE SF_CONSTANTS, only: one,xi,zero,pi
  implicit none
  private

  intrinsic :: sum
  interface sum
     module procedure :: kb_gf_flat_rank0
     module procedure :: kb_gf_flat_rank1
     module procedure :: kb_gf_flat_rank2
     module procedure :: kb_gf_flat_rank3
     module procedure :: kb_gf_flat_rank4
     module procedure :: kb_gf_flat_rank5
     module procedure :: kb_gf_flat_rank6
     !
     module procedure :: kb_gf_reduce_rank0
     module procedure :: kb_gf_reduce_rank1
     module procedure :: kb_gf_reduce_rank2
     module procedure :: kb_gf_reduce_rank3
     module procedure :: kb_gf_reduce_rank4
     module procedure :: kb_gf_reduce_rank5
     module procedure :: kb_gf_reduce_rank6     
  end interface sum


  interface reduce_kb_gf
     module procedure :: kb_gf_flat_rank0
     module procedure :: kb_gf_flat_rank1
     module procedure :: kb_gf_flat_rank2
     module procedure :: kb_gf_flat_rank3
     module procedure :: kb_gf_flat_rank4
     module procedure :: kb_gf_flat_rank5
     module procedure :: kb_gf_flat_rank6     
     !
     module procedure :: kb_gf_reduce_rank0
     module procedure :: kb_gf_reduce_rank1
     module procedure :: kb_gf_reduce_rank2
     module procedure :: kb_gf_reduce_rank3
     module procedure :: kb_gf_reduce_rank4
     module procedure :: kb_gf_reduce_rank5
     module procedure :: kb_gf_reduce_rank6     
  end interface reduce_kb_gf


  !These functions reduce a rank-D KB_GF to a rank-(D-1) one either the whole or along the boundary.
  !BOUNDARY: requires integer index N as a second input argument sum(G,itime)
  !WHOLE: just the function sum(G)
  public :: sum                 !extend intrinsic sum
  public :: reduce_kb_gf        !reduce as a callable function 

contains

  !Flat function
  function kb_gf_flat_rank0(A,N) result(C)
    class(kb_gf),intent(in) :: A(:)
    type(kb_gf)             :: C
    integer                 :: i
    integer,intent(in)      :: N
    !
    call C%init
    ! N   = cc_params%Nt   !<== work with the ACTUAL size of the contour
    !
    do i=1,size(a)
       if(N==1)then
          C%mats(0:) = C%mats(0:) + A(i)%mats(0:)
          C%iw(:)    = C%iw(:)    + A(i)%iw(:)
       endif
       C%ret(N,1:N)   = C%ret(N,1:N)  + A(i)%ret(N,1:N)
       C%less(N,1:N)  = C%less(N,1:N) + A(i)%less(N,1:N)
       C%lmix(N,0:)   = C%lmix(N,0:)  + A(i)%lmix(N,0:)
    enddo
    ! !THIS SHOULD NOT BE INVOLVED IN THE CALCULATION:
    ! C%less(1:N-1,N)= -conjg(C%less(N,1:N-1))
  end function kb_gf_flat_rank0


  function kb_gf_flat_rank1(A,N) result(C)
    class(kb_gf),intent(in) :: A(:,:)
    type(kb_gf)             :: C(size(A,1))
    integer                 :: i
    integer,intent(in)      :: N
    !    
    call C%init
    !
    ! N   = cc_params%Nt   !<== work with the ACTUAL size of the contour
    !
    do i1=1,size(A,1)
       do i=1,size(a,2)
          if(N==1)then
             C(i1)%mats(0:) = C(i1)%mats(0:) + A(i1,i)%mats(0:)
             C(i1)%iw(:)    = C(i1)%iw(:)    + A(i1,i)%iw(:)
          endif
          C(i1)%ret(N,1:N)   = C(i1)%ret(N,1:N)  + A(i1,i)%ret(N,1:N)
          C(i1)%less(N,1:N)  = C(i1)%less(N,1:N) + A(i1,i)%less(N,1:N)
          C(i1)%lmix(N,0:)   = C(i1)%lmix(N,0:)  + A(i1,i)%lmix(N,0:)
       enddo
    enddo
  end function kb_gf_flat_rank1


  function kb_gf_flat_rank2(A,N) result(C)
    class(kb_gf),intent(in) :: A(:,:,:)
    type(kb_gf)             :: C(size(A,1),size(A,2))
    integer                 :: i
    integer,intent(in)      :: N
    !    
    call C%init
    !
    ! N   = cc_params%Nt   !<== work with the ACTUAL size of the contour
    !
    do i1=1,size(A,1)
       do i2=1,size(A,2)
          do i=1,size(a,3)
             if(N==1)then
                C(i1,i2)%mats(0:) = C(i1,i2)%mats(0:) + A(i1,i2,i)%mats(0:)
                C(i1,i2)%iw(:)    = C(i1,i2)%iw(:)    + A(i1,i2,i)%iw(:)
             endif
             C(i1,i2)%ret(N,1:N)   = C(i1,i2)%ret(N,1:N)  + A(i1,i2,i)%ret(N,1:N)
             C(i1,i2)%less(N,1:N)  = C(i1,i2)%less(N,1:N) + A(i1,i2,i)%less(N,1:N)
             C(i1,i2)%lmix(N,0:)   = C(i1,i2)%lmix(N,0:)  + A(i1,i2,i)%lmix(N,0:)
          enddo
       enddo
    enddo
  end function kb_gf_flat_rank2


  function kb_gf_flat_rank3(A,N) result(C)
    class(kb_gf),intent(in) :: A(:,:,:,:)
    type(kb_gf)             :: C(size(A,1),size(A,2),size(A,3))
    integer                 :: i
    integer,intent(in)      :: N
    !    
    call C%init
    !
    ! N   = cc_params%Nt   !<== work with the ACTUAL size of the contour
    !
    do i1=1,size(A,1)
       do i2=1,size(A,2)
          do i3=1,size(A,3)
             do i=1,size(a,4)
                if(N==1)then
                   C(i1,i2,i3)%mats(0:) = C(i1,i2,i3)%mats(0:) + A(i1,i2,i3,i)%mats(0:)
                   C(i1,i2,i3)%iw(:)    = C(i1,i2,i3)%iw(:)    + A(i1,i2,i3,i)%iw(:)
                endif
                C(i1,i2,i3)%ret(N,1:N)   = C(i1,i2,i3)%ret(N,1:N)  + A(i1,i2,i3,i)%ret(N,1:N)
                C(i1,i2,i3)%less(N,1:N)  = C(i1,i2,i3)%less(N,1:N) + A(i1,i2,i3,i)%less(N,1:N)
                C(i1,i2,i3)%lmix(N,0:)   = C(i1,i2,i3)%lmix(N,0:)  + A(i1,i2,i3,i)%lmix(N,0:)
             enddo
          enddo
       enddo
    enddo
  end function kb_gf_flat_rank3

  function kb_gf_flat_rank4(A,N) result(C)
    class(kb_gf),intent(in) :: A(:,:,:,:,:)
    type(kb_gf)             :: C(size(A,1),size(A,2),size(A,3),size(A,4))
    integer                 :: i
    integer,intent(in)      :: N
    !    
    call C%init
    !
    ! N   = cc_params%Nt   !<== work with the ACTUAL size of the contour
    !
    do i1=1,size(A,1)
       do i2=1,size(A,2)
          do i3=1,size(A,3)
             do i4=1,size(a,4)
                do i=1,size(a,5)
                   if(N==1)then
                      C(i1,i2,i3,i4)%mats(0:) = C(i1,i2,i3,i4)%mats(0:) + A(i1,i2,i3,i4,i)%mats(0:)
                      C(i1,i2,i3,i4)%iw(:)    = C(i1,i2,i3,i4)%iw(:)    + A(i1,i2,i3,i4,i)%iw(:)
                   endif
                   C(i1,i2,i3,i4)%ret(N,1:N)   = C(i1,i2,i3,i4)%ret(N,1:N)  + A(i1,i2,i3,i4,i)%ret(N,1:N)
                   C(i1,i2,i3,i4)%less(N,1:N)  = C(i1,i2,i3,i4)%less(N,1:N) + A(i1,i2,i3,i4,i)%less(N,1:N)
                   C(i1,i2,i3,i4)%lmix(N,0:)   = C(i1,i2,i3,i4)%lmix(N,0:)  + A(i1,i2,i3,i4,i)%lmix(N,0:)
                enddo
             enddo
          enddo
       enddo
    enddo
  end function kb_gf_flat_rank4


  function kb_gf_flat_rank5(A,N) result(C)
    class(kb_gf),intent(in) :: A(:,:,:,:,:,:)
    type(kb_gf)             :: C(size(A,1),size(A,2),size(A,3),size(A,4),size(A,5))
    integer                 :: i
    integer,intent(in)      :: N
    !    
    call C%init
    !
    ! N   = cc_params%Nt   !<== work with the ACTUAL size of the contour
    !
    do i1=1,size(A,1)
       do i2=1,size(A,2)
          do i3=1,size(A,3)
             do i4=1,size(A,4)
                do i5=1,size(A,5)
                   do i=1,size(a,6)
                      if(N==1)then
                         C(i1,i2,i3,i4,i5)%mats(0:) = C(i1,i2,i3,i4,i5)%mats(0:) + A(i1,i2,i3,i4,i5,i)%mats(0:)
                         C(i1,i2,i3,i4,i5)%iw(:)    = C(i1,i2,i3,i4,i5)%iw(:)    + A(i1,i2,i3,i4,i5,i)%iw(:)
                      endif
                      C(i1,i2,i3,i4,i5)%ret(N,1:N)   = C(i1,i2,i3,i4,i5)%ret(N,1:N)  + A(i1,i2,i3,i4,i5,i)%ret(N,1:N)
                      C(i1,i2,i3,i4,i5)%less(N,1:N)  = C(i1,i2,i3,i4,i5)%less(N,1:N) + A(i1,i2,i3,i4,i5,i)%less(N,1:N)
                      C(i1,i2,i3,i4,i5)%lmix(N,0:)   = C(i1,i2,i3,i4,i5)%lmix(N,0:)  + A(i1,i2,i3,i4,i5,i)%lmix(N,0:)
                   enddo
                enddo
             enddo
          enddo
       enddo
    enddo
  end function kb_gf_flat_rank5


  function kb_gf_flat_rank6(A,N) result(C)
    class(kb_gf),intent(in) :: A(:,:,:,:,:,:,:)
    type(kb_gf)             :: C(size(A,1),size(A,2),size(A,3),size(A,4),size(A,5),size(A,6))
    integer                 :: i
    integer,intent(in)      :: N
    !    
    call C%init
    !
    ! N   = cc_params%Nt   !<== work with the ACTUAL size of the contour
    !
    do i1=1,size(A,1)
       do i2=1,size(A,2)
          do i3=1,size(A,3)
             do i4=1,size(A,4)
                do i5=1,size(A,5)
                   do i6=1,size(A,6)
                      do i=1,size(a,7)
                         if(N==1)then
                            C(i1,i2,i3,i4,i5,i6)%mats(0:) = C(i1,i2,i3,i4,i5,i6)%mats(0:) + A(i1,i2,i3,i4,i5,i6,i)%mats(0:)
                            C(i1,i2,i3,i4,i5,i6)%iw(:)    = C(i1,i2,i3,i4,i5,i6)%iw(:)    + A(i1,i2,i3,i4,i5,i6,i)%iw(:)
                         endif
                         C(i1,i2,i3,i4,i5,i6)%ret(N,1:N)   = C(i1,i2,i3,i4,i5,i6)%ret(N,1:N)  + A(i1,i2,i3,i4,i5,i6,i)%ret(N,1:N)
                         C(i1,i2,i3,i4,i5,i6)%less(N,1:N)  = C(i1,i2,i3,i4,i5,i6)%less(N,1:N) + A(i1,i2,i3,i4,i5,i6,i)%less(N,1:N)
                         C(i1,i2,i3,i4,i5,i6)%lmix(N,0:)   = C(i1,i2,i3,i4,i5,i6)%lmix(N,0:)  + A(i1,i2,i3,i4,i5,i6,i)%lmix(N,0:)
                      enddo
                   enddo
                enddo
             enddo
          enddo
       enddo
    enddo
  end function kb_gf_flat_rank6






  !################################################################################
  !################################################################################
  !################################################################################
  !################################################################################
  !################################################################################





  !Reduce function
  function kb_gf_reduce_rank0(A) result(C)
    class(kb_gf),intent(in) :: A(:)
    type(kb_gf)             :: C
    integer                 :: i
    !
    call C%init
    !
    do i=1,size(a)
       C%mats(0:) = C%mats(0:) + A(i)%mats(0:)
       C%iw(:)    = C%iw(:)    + A(i)%iw(:)       
       C%ret(:,:)   = C%ret(:,:)  + A(i)%ret(:,:)
       C%less(:,:)  = C%less(:,:) + A(i)%less(:,:)
       C%lmix(:,0:)   = C%lmix(:,0:)  + A(i)%lmix(:,0:)
    enddo
    ! !THIS SHOULD NOT BE INVOLVED IN THE CALCULATION:
    ! C%less(1:N-1,N)= -conjg(C%less(N,1:N-1))
  end function kb_gf_reduce_rank0


  function kb_gf_reduce_rank1(A) result(C)
    class(kb_gf),intent(in) :: A(:,:)
    type(kb_gf)             :: C(size(A,1))
    integer                 :: i
    !    
    call C%init
    !
    !
    do i1=1,size(A,1)
       do i=1,size(a,2)   
          C(i1)%mats(0:) = C(i1)%mats(0:) + A(i1,i)%mats(0:)
          C(i1)%iw(:)    = C(i1)%iw(:)    + A(i1,i)%iw(:)          
          C(i1)%ret(:,:)   = C(i1)%ret(:,:)  + A(i1,i)%ret(:,:)
          C(i1)%less(:,:)  = C(i1)%less(:,:) + A(i1,i)%less(:,:)
          C(i1)%lmix(:,0:)   = C(i1)%lmix(:,0:)  + A(i1,i)%lmix(:,0:)
       enddo
    enddo
  end function kb_gf_reduce_rank1


  function kb_gf_reduce_rank2(A) result(C)
    class(kb_gf),intent(in) :: A(:,:,:)
    type(kb_gf)             :: C(size(A,1),size(A,2))
    integer                 :: i
    !    
    call C%init
    !
    !
    do i1=1,size(A,1)
       do i2=1,size(A,2)
          do i=1,size(a,3)      
             C(i1,i2)%mats(0:) = C(i1,i2)%mats(0:) + A(i1,i2,i)%mats(0:)
             C(i1,i2)%iw(:)    = C(i1,i2)%iw(:)    + A(i1,i2,i)%iw(:)             
             C(i1,i2)%ret(:,:)   = C(i1,i2)%ret(:,:)  + A(i1,i2,i)%ret(:,:)
             C(i1,i2)%less(:,:)  = C(i1,i2)%less(:,:) + A(i1,i2,i)%less(:,:)
             C(i1,i2)%lmix(:,0:)   = C(i1,i2)%lmix(:,0:)  + A(i1,i2,i)%lmix(:,0:)
          enddo
       enddo
    enddo
  end function kb_gf_reduce_rank2


  function kb_gf_reduce_rank3(A) result(C)
    class(kb_gf),intent(in) :: A(:,:,:,:)
    type(kb_gf)             :: C(size(A,1),size(A,2),size(A,3))
    integer                 :: i
    !    
    call C%init
    !
    !
    do i1=1,size(A,1)
       do i2=1,size(A,2)
          do i3=1,size(A,3)
             do i=1,size(a,4)         
                C(i1,i2,i3)%mats(0:) = C(i1,i2,i3)%mats(0:) + A(i1,i2,i3,i)%mats(0:)
                C(i1,i2,i3)%iw(:)    = C(i1,i2,i3)%iw(:)    + A(i1,i2,i3,i)%iw(:)                
                C(i1,i2,i3)%ret(:,:)   = C(i1,i2,i3)%ret(:,:)  + A(i1,i2,i3,i)%ret(:,:)
                C(i1,i2,i3)%less(:,:)  = C(i1,i2,i3)%less(:,:) + A(i1,i2,i3,i)%less(:,:)
                C(i1,i2,i3)%lmix(:,0:)   = C(i1,i2,i3)%lmix(:,0:)  + A(i1,i2,i3,i)%lmix(:,0:)
             enddo
          enddo
       enddo
    enddo
  end function kb_gf_reduce_rank3

  function kb_gf_reduce_rank4(A) result(C)
    class(kb_gf),intent(in) :: A(:,:,:,:,:)
    type(kb_gf)             :: C(size(A,1),size(A,2),size(A,3),size(A,4))
    integer                 :: i
    !    
    call C%init
    !
    !
    do i1=1,size(A,1)
       do i2=1,size(A,2)
          do i3=1,size(A,3)
             do i4=1,size(a,4)
                do i=1,size(a,5)
                   C(i1,i2,i3,i4)%mats(0:) = C(i1,i2,i3,i4)%mats(0:) + A(i1,i2,i3,i4,i)%mats(0:)
                   C(i1,i2,i3,i4)%iw(:)    = C(i1,i2,i3,i4)%iw(:)    + A(i1,i2,i3,i4,i)%iw(:)
                   C(i1,i2,i3,i4)%ret(:,:)   = C(i1,i2,i3,i4)%ret(:,:)  + A(i1,i2,i3,i4,i)%ret(:,:)
                   C(i1,i2,i3,i4)%less(:,:)  = C(i1,i2,i3,i4)%less(:,:) + A(i1,i2,i3,i4,i)%less(:,:)
                   C(i1,i2,i3,i4)%lmix(:,0:)   = C(i1,i2,i3,i4)%lmix(:,0:)  + A(i1,i2,i3,i4,i)%lmix(:,0:)
                enddo
             enddo
          enddo
       enddo
    enddo
  end function kb_gf_reduce_rank4


  function kb_gf_reduce_rank5(A) result(C)
    class(kb_gf),intent(in) :: A(:,:,:,:,:,:)
    type(kb_gf)             :: C(size(A,1),size(A,2),size(A,3),size(A,4),size(A,5))
    integer                 :: i
    !    
    call C%init
    !
    !
    do i1=1,size(A,1)
       do i2=1,size(A,2)
          do i3=1,size(A,3)
             do i4=1,size(A,4)
                do i5=1,size(A,5)
                   do i=1,size(a,6)
                      C(i1,i2,i3,i4,i5)%mats(0:) = C(i1,i2,i3,i4,i5)%mats(0:) + A(i1,i2,i3,i4,i5,i)%mats(0:)
                      C(i1,i2,i3,i4,i5)%iw(:)    = C(i1,i2,i3,i4,i5)%iw(:)    + A(i1,i2,i3,i4,i5,i)%iw(:)
                      C(i1,i2,i3,i4,i5)%ret(:,:)   = C(i1,i2,i3,i4,i5)%ret(:,:)  + A(i1,i2,i3,i4,i5,i)%ret(:,:)
                      C(i1,i2,i3,i4,i5)%less(:,:)  = C(i1,i2,i3,i4,i5)%less(:,:) + A(i1,i2,i3,i4,i5,i)%less(:,:)
                      C(i1,i2,i3,i4,i5)%lmix(:,0:)   = C(i1,i2,i3,i4,i5)%lmix(:,0:)  + A(i1,i2,i3,i4,i5,i)%lmix(:,0:)
                   enddo
                enddo
             enddo
          enddo
       enddo
    enddo
  end function kb_gf_reduce_rank5


  function kb_gf_reduce_rank6(A) result(C)
    class(kb_gf),intent(in) :: A(:,:,:,:,:,:,:)
    type(kb_gf)             :: C(size(A,1),size(A,2),size(A,3),size(A,4),size(A,5),size(A,6))
    integer                 :: i
    !    
    call C%init
    !
    !
    do i1=1,size(A,1)
       do i2=1,size(A,2)
          do i3=1,size(A,3)
             do i4=1,size(A,4)
                do i5=1,size(A,5)
                   do i6=1,size(A,6)
                      do i=1,size(a,7)
                         C(i1,i2,i3,i4,i5,i6)%mats(0:) = C(i1,i2,i3,i4,i5,i6)%mats(0:) + A(i1,i2,i3,i4,i5,i6,i)%mats(0:)
                         C(i1,i2,i3,i4,i5,i6)%iw(:)    = C(i1,i2,i3,i4,i5,i6)%iw(:)    + A(i1,i2,i3,i4,i5,i6,i)%iw(:)
                         C(i1,i2,i3,i4,i5,i6)%ret(:,:)   = C(i1,i2,i3,i4,i5,i6)%ret(:,:)  + A(i1,i2,i3,i4,i5,i6,i)%ret(:,:)
                         C(i1,i2,i3,i4,i5,i6)%less(:,:)  = C(i1,i2,i3,i4,i5,i6)%less(:,:) + A(i1,i2,i3,i4,i5,i6,i)%less(:,:)
                         C(i1,i2,i3,i4,i5,i6)%lmix(:,0:)   = C(i1,i2,i3,i4,i5,i6)%lmix(:,0:)  + A(i1,i2,i3,i4,i5,i6,i)%lmix(:,0:)
                      enddo
                   enddo
                enddo
             enddo
          enddo
       enddo
    enddo
  end function kb_gf_reduce_rank6




END MODULE KB_GF_SUM
