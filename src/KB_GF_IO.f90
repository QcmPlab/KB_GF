MODULE KB_GF_IO
  USE KB_VARS_GLOBAL
  USE KB_CONTOUR
  USE KB_AUX
  USE KB_GF_COMMON
  USE SF_IOTOOLS
  implicit none
  private


  !SAVE:
  interface save_kb_gf
     module procedure :: save_kb_gf_main
     module procedure :: save_kb_gf_d1
     module procedure :: save_kb_gf_d2
     module procedure :: save_kb_gf_d3
     module procedure :: save_kb_gf_d4
     module procedure :: save_kb_gf_d5
     module procedure :: save_kb_gf_d6
     module procedure :: save_kb_gf_d7
     !
     module procedure :: save_kb_dgf_main
     module procedure :: save_kb_dgf_d1
     module procedure :: save_kb_dgf_d2
     module procedure :: save_kb_dgf_d3
     module procedure :: save_kb_dgf_d4
     module procedure :: save_kb_dgf_d5
     module procedure :: save_kb_dgf_d6
     module procedure :: save_kb_dgf_d7
  end interface save_kb_gf



  !READ:
  interface read_kb_gf
     module procedure :: read_kb_gf_main
     module procedure :: read_kb_gf_d1
     module procedure :: read_kb_gf_d2
     module procedure :: read_kb_gf_d3
     module procedure :: read_kb_gf_d4
     module procedure :: read_kb_gf_d5
     module procedure :: read_kb_gf_d6
     module procedure :: read_kb_gf_d7
     !
     module procedure :: read_kb_dgf_main
     module procedure :: read_kb_dgf_d1
     module procedure :: read_kb_dgf_d2
     module procedure :: read_kb_dgf_d3
     module procedure :: read_kb_dgf_d4
     module procedure :: read_kb_dgf_d5
     module procedure :: read_kb_dgf_d6
     module procedure :: read_kb_dgf_d7
  end interface read_kb_gf




  !PLOT:
  interface plot_kb_gf
     module procedure :: plot_kb_gf_main
     module procedure :: plot_kb_gf_d1
     module procedure :: plot_kb_gf_d2
     module procedure :: plot_kb_gf_d3
     module procedure :: plot_kb_gf_d4
     module procedure :: plot_kb_gf_d5
     module procedure :: plot_kb_gf_d6
     module procedure :: plot_kb_gf_d7
  end interface plot_kb_gf

  public :: save_kb_gf
  public :: read_kb_gf
  public :: plot_kb_gf



contains


  subroutine save_kb_gf_main(G,file)
    type(kb_gf)      :: G
    character(len=*) :: file
    integer          :: unit
    call save_array(reg(file)//"_less.data",G%less(:,:))
    call save_array(reg(file)//"_ret.data", G%ret(:,:))
    call save_array(reg(file)//"_lmix.data",G%lmix(:,0:))
    call save_array(reg(file)//"_mats.data",G%mats(0:))
    call save_array(reg(file)//"_iw.data",G%iw(:))
  end subroutine save_kb_gf_main

  subroutine save_kb_gf_d1(G,file)
    type(kb_gf)      :: G(:)
    character(len=*) :: file
    integer          :: unit
    do i1=1,size(G,1)
       call save_kb_gf_main(G(i1),&
            file//&
            "_i"//str(i1)&
            )
    enddo
  end subroutine save_kb_gf_d1

  subroutine save_kb_gf_d2(G,file)
    type(kb_gf)      :: G(:,:)
    character(len=*) :: file
    integer          :: unit
    do i1=1,size(G,1)
       do i2=1,size(G,2)
          call save_kb_gf_main(G(i1,i2),&
               file//&
               "_io"//str(i1)//&
               "_jo"//str(i2)&
               )
       enddo
    enddo
  end subroutine save_kb_gf_d2

  subroutine save_kb_gf_d3(G,file)
    type(kb_gf)      :: G(:,:,:)
    character(len=*) :: file
    integer          :: unit
    do i1=1,size(G,1)
       do i2=1,size(G,2)
          do i3=1,size(G,3)
             call save_kb_gf_main(G(i1,i2,i3),&
                  file//&
                  "_i"//str(i1)//&
                  "_j"//str(i2)//&
                  "_k"//str(i3)&
                  )
          enddo
       enddo
    enddo
  end subroutine save_kb_gf_d3

  subroutine save_kb_gf_d4(G,file)
    type(kb_gf)      :: G(:,:,:,:)
    character(len=*) :: file
    integer          :: unit
    Nspin=size(G,1)
    Norb =size(G,3)
    do ispin=1,Nspin
       do jspin=1,Nspin
          do iorb=1,Norb
             do jorb=1,Norb
                call save_kb_gf_main(G(ispin,jspin,iorb,jorb),&
                     file//"_l"//str(iorb)//str(jorb)//"_s"//str(ispin)//str(jspin))
             enddo
          enddo
       enddo
    enddo
  end subroutine save_kb_gf_d4

  subroutine save_kb_gf_d5(G,file)
    type(kb_gf)      :: G(:,:,:,:,:)
    character(len=*) :: file
    integer          :: unit
    do i1=1,size(G,1)
       do i2=1,size(G,2)
          do i3=1,size(G,3)
             do i4=1,size(G,4)
                do i5=1,size(G,5)
                   call save_kb_gf_main(G(i1,i2,i3,i4,i5),&
                        file//&
                        "_i"//str(i1)//&
                        "_j"//str(i2)//&
                        "_k"//str(i3)//&
                        "_r"//str(i4)//&
                        "_s"//str(i5)&
                        )
                enddo
             enddo
          enddo
       enddo
    enddo
  end subroutine save_kb_gf_d5

  subroutine save_kb_gf_d6(G,file)
    type(kb_gf)      :: G(:,:,:,:,:,:)
    character(len=*) :: file
    integer          :: unit
    do i1=1,size(G,1)
       do i2=1,size(G,2)
          do i3=1,size(G,3)
             do i4=1,size(G,4)
                do i5=1,size(G,5)
                   do i6=1,size(G,6)
                      call save_kb_gf_main(G(i1,i2,i3,i4,i5,i6),&
                           file//&
                           "_i"//str(i1)//&
                           "_j"//str(i2)//&
                           "_k"//str(i3)//&
                           "_r"//str(i4)//&
                           "_s"//str(i5)//&
                           "_t"//str(i6)&
                           )
                   enddo
                enddo
             enddo
          enddo
       enddo
    enddo
  end subroutine save_kb_gf_d6

  subroutine save_kb_gf_d7(G,file)
    type(kb_gf)      :: G(:,:,:,:,:,:,:)
    character(len=*) :: file
    integer          :: unit
    do i1=1,size(G,1)
       do i2=1,size(G,2)
          do i3=1,size(G,3)
             do i4=1,size(G,4)
                do i5=1,size(G,5)
                   do i6=1,size(G,6)
                      do i7=1,size(G,7)
                         call save_kb_gf_main(G(i1,i2,i3,i4,i5,i6,i7),&
                              file//&
                              "_i"//str(i1)//&
                              "_j"//str(i2)//&
                              "_k"//str(i3)//&
                              "_r"//str(i4)//&
                              "_s"//str(i5)//&
                              "_t"//str(i6)//&
                              "_z"//str(i7)&
                              )
                      enddo
                   enddo
                enddo
             enddo
          enddo
       enddo
    enddo
  end subroutine save_kb_gf_d7








  !##################################################################
  !##################################################################
  !##################################################################
  !##################################################################
  !##################################################################





  subroutine read_kb_gf_main(G,file)
    type(kb_gf)      :: G
    character(len=*) :: file
    logical          :: check
    check = inquire_kb_gf(file)
    if(.not.G%status.OR..not.check)stop "contour_gf/read_kb_gf: G not allocated"
    call read_array(trim(file)//"_less.data",G%less(:,:))
    call read_array(trim(file)//"_ret.data",G%ret(:,:))
    call read_array(trim(file)//"_lmix.data",G%lmix(:,0:))
    call read_array(trim(file)//"_mats.data",G%mats(0:))
    call read_array(trim(file)//"_iw.data",G%iw(:))
  end subroutine read_kb_gf_main

  subroutine read_kb_gf_d1(G,file)
    type(kb_gf)      :: G(:)
    character(len=*) :: file
    integer          :: unit
    do i1=1,size(G,1)
       call read_kb_gf_main(G(i1),&
            file//&
            "_i"//str(i1)&
            )
    enddo
  end subroutine read_kb_gf_d1

  subroutine read_kb_gf_d2(G,file)
    type(kb_gf)      :: G(:,:)
    character(len=*) :: file
    integer          :: unit
    do i1=1,size(G,1)
       do i2=1,size(G,2)
          call read_kb_gf_main(G(i1,i2),&
               file//&
               "_io"//str(i1)//&
               "_jo"//str(i2)&
               )
       enddo
    enddo
  end subroutine read_kb_gf_d2

  subroutine read_kb_gf_d3(G,file)
    type(kb_gf)      :: G(:,:,:)
    character(len=*) :: file
    integer          :: unit
    do i1=1,size(G,1)
       do i2=1,size(G,2)
          do i3=1,size(G,3)
             call read_kb_gf_main(G(i1,i2,i3),&
                  file//&
                  "_i"//str(i1)//&
                  "_j"//str(i2)//&
                  "_k"//str(i3)&
                  )
          enddo
       enddo
    enddo
  end subroutine read_kb_gf_d3

  subroutine read_kb_gf_d4(G,file)
    type(kb_gf)      :: G(:,:,:,:)
    character(len=*) :: file
    integer          :: unit
    Nspin=size(G,1)
    Norb =size(G,3)
    do ispin=1,Nspin
       do jspin=1,Nspin
          do iorb=1,Norb
             do jorb=1,Norb
                call read_kb_gf_main(G(ispin,jspin,iorb,jorb),&
                     file//"_l"//str(iorb)//str(jorb)//"_s"//str(ispin)//str(jspin))
             enddo
          enddo
       enddo
    enddo
  end subroutine read_kb_gf_d4

  subroutine read_kb_gf_d5(G,file)
    type(kb_gf)      :: G(:,:,:,:,:)
    character(len=*) :: file
    integer          :: unit
    do i1=1,size(G,1)
       do i2=1,size(G,2)
          do i3=1,size(G,3)
             do i4=1,size(G,4)
                do i5=1,size(G,5)
                   call read_kb_gf_main(G(i1,i2,i3,i4,i5),&
                        file//&
                        "_i"//str(i1)//&
                        "_j"//str(i2)//&
                        "_k"//str(i3)//&
                        "_r"//str(i4)//&
                        "_s"//str(i5)&
                        )
                enddo
             enddo
          enddo
       enddo
    enddo
  end subroutine read_kb_gf_d5

  subroutine read_kb_gf_d6(G,file)
    type(kb_gf)      :: G(:,:,:,:,:,:)
    character(len=*) :: file
    integer          :: unit
    do i1=1,size(G,1)
       do i2=1,size(G,2)
          do i3=1,size(G,3)
             do i4=1,size(G,4)
                do i5=1,size(G,5)
                   do i6=1,size(G,6)
                      call read_kb_gf_main(G(i1,i2,i3,i4,i5,i6),&
                           file//&
                           "_i"//str(i1)//&
                           "_j"//str(i2)//&
                           "_k"//str(i3)//&
                           "_r"//str(i4)//&
                           "_s"//str(i5)//&
                           "_t"//str(i6)&
                           )
                   enddo
                enddo
             enddo
          enddo
       enddo
    enddo
  end subroutine read_kb_gf_d6

  subroutine read_kb_gf_d7(G,file)
    type(kb_gf)      :: G(:,:,:,:,:,:,:)
    character(len=*) :: file
    integer          :: unit
    do i1=1,size(G,1)
       do i2=1,size(G,2)
          do i3=1,size(G,3)
             do i4=1,size(G,4)
                do i5=1,size(G,5)
                   do i6=1,size(G,6)
                      do i7=1,size(G,7)
                         call read_kb_gf_main(G(i1,i2,i3,i4,i5,i6,i7),&
                              file//&
                              "_i"//str(i1)//&
                              "_j"//str(i2)//&
                              "_k"//str(i3)//&
                              "_r"//str(i4)//&
                              "_s"//str(i5)//&
                              "_t"//str(i6)//&
                              "_z"//str(i7)&
                              )
                      enddo
                   enddo
                enddo
             enddo
          enddo
       enddo
    enddo
  end subroutine read_kb_gf_d7





  !##################################################################
  !##################################################################
  !##################################################################
  !##################################################################
  !##################################################################






  subroutine plot_kb_gf_main(G,file)
    character(len=*) :: file
    type(kb_gf)      :: G
    integer          :: Nt
    if(.not.G%status)stop "contour_gf/plot_kb_gf: G is not allocated" 
    Nt=cc_params%Ntime
    call splot3d(reg(file)//"_less_t_t.neqipt",cc_params%t(:Nt),cc_params%t(:Nt),G%less(:Nt,:Nt))
    call splot3d(reg(file)//"_ret_t_t.neqipt",cc_params%t(:Nt),cc_params%t(:Nt),G%ret(:Nt,:Nt))
    call splot3d(reg(file)//"_lmix_t_tau.neqipt",cc_params%t(:Nt),cc_params%tau(0:),G%lmix(:Nt,0:))
    call splot(reg(file)//"_mats_tau.neqipt",cc_params%tau(0:),G%mats(0:))
    call splot(reg(file)//"_mats_iw.neqipt",cc_params%wm(:),G%iw(:))
  end subroutine plot_kb_gf_main


  subroutine plot_kb_gf_d1(G,file)
    type(kb_gf) :: G(:)
    character(len=*)    :: file
    integer             :: unit
    do i1=1,size(G,1)
       call plot_kb_gf_main(G(i1),&
            file//&
            "_i"//str(i1))
    enddo
  end subroutine plot_kb_gf_d1

  subroutine plot_kb_gf_d2(G,file)
    type(kb_gf) :: G(:,:)
    character(len=*)    :: file
    integer             :: unit
    do i1=1,size(G,1)
       do i2=1,size(G,2)
          call plot_kb_gf_main(G(i1,i2),&
               file//&
               "_io"//str(i1)//&
               "_jo"//str(i2))
       enddo
    enddo
  end subroutine plot_kb_gf_d2

  subroutine plot_kb_gf_d3(G,file)
    type(kb_gf) :: G(:,:,:)
    character(len=*)    :: file
    integer             :: unit
    do i1=1,size(G,1)
       do i2=1,size(G,2)
          do i3=1,size(G,3)
             call plot_kb_gf_main(G(i1,i2,i3),&
                  file//&
                  "_i"//str(i1)//&
                  "_j"//str(i2)//&
                  "_k"//str(i3))
          enddo
       enddo
    enddo
  end subroutine plot_kb_gf_d3

  subroutine plot_kb_gf_d4(G,file)
    type(kb_gf) :: G(:,:,:,:)
    character(len=*)    :: file
    integer             :: unit
    Nspin=size(G,1)
    Norb =size(G,3)
    do ispin=1,Nspin
       do jspin=1,Nspin
          do iorb=1,Norb
             do jorb=1,Norb
                call plot_kb_gf_main(G(ispin,jspin,iorb,jorb),&
                     file//"_l"//str(iorb)//str(jorb)//"_s"//str(ispin)//str(jspin))
             enddo
          enddo
       enddo
    enddo
  end subroutine plot_kb_gf_d4

  subroutine plot_kb_gf_d5(G,file)
    type(kb_gf) :: G(:,:,:,:,:)
    character(len=*)    :: file
    integer             :: unit
    do i1=1,size(G,1)
       do i2=1,size(G,2)
          do i3=1,size(G,3)
             do i4=1,size(G,4)
                do i5=1,size(G,5)
                   call plot_kb_gf_main(G(i1,i2,i3,i4,i5),&
                        file//&
                        "_i"//str(i1)//&
                        "_j"//str(i2)//&
                        "_k"//str(i3)//&
                        "_r"//str(i4)//&
                        "_s"//str(i5))
                enddo
             enddo
          enddo
       enddo
    enddo
  end subroutine plot_kb_gf_d5

  subroutine plot_kb_gf_d6(G,file)
    type(kb_gf) :: G(:,:,:,:,:,:)
    character(len=*)    :: file
    integer             :: unit
    do i1=1,size(G,1)
       do i2=1,size(G,2)
          do i3=1,size(G,3)
             do i4=1,size(G,4)
                do i5=1,size(G,5)
                   do i6=1,size(G,6)
                      call plot_kb_gf_main(G(i1,i2,i3,i4,i5,i6),&
                           file//&
                           "_i"//str(i1)//&
                           "_j"//str(i2)//&
                           "_k"//str(i3)//&
                           "_r"//str(i4)//&
                           "_s"//str(i5)//&
                           "_t"//str(i6))
                   enddo
                enddo
             enddo
          enddo
       enddo
    enddo
  end subroutine plot_kb_gf_d6

  subroutine plot_kb_gf_d7(G,file)
    type(kb_gf) :: G(:,:,:,:,:,:,:)
    character(len=*)    :: file
    integer             :: unit
    do i1=1,size(G,1)
       do i2=1,size(G,2)
          do i3=1,size(G,3)
             do i4=1,size(G,4)
                do i5=1,size(G,5)
                   do i6=1,size(G,6)
                      do i7=1,size(G,7)
                         call plot_kb_gf_main(G(i1,i2,i3,i4,i5,i6,i7),&
                              file//&
                              "_i"//str(i1)//&
                              "_j"//str(i2)//&
                              "_k"//str(i3)//&
                              "_r"//str(i4)//&
                              "_s"//str(i5)//&
                              "_t"//str(i6)//&
                              "_z"//str(i7))
                      enddo
                   enddo
                enddo
             enddo
          enddo
       enddo
    enddo
  end subroutine plot_kb_gf_d7









  !##################################################################
  !##################################################################
  !##################################################################
  !##################################################################
  !##################################################################





  subroutine save_kb_dgf_main(dG,file)
    type(kb_dgf)     :: dG
    character(len=*) :: file
    integer          :: unit
    call save_array(reg(file)//"_less.data",dG%less(:))
    call save_array(reg(file)//"_ret.data", dG%ret(:))
    call save_array(reg(file)//"_lmix.data",dG%lmix(0:))
  end subroutine save_kb_dgf_main

  subroutine save_kb_dgf_d1(dG,file)
    type(kb_dgf)     :: dG(:)
    character(len=*) :: file
    integer          :: unit
    do i1=1,size(dG,1)
       call save_kb_dgf_main(dG(i1),&
            file//&
            "_i"//str(i1)&
            )
    enddo
  end subroutine save_kb_dgf_d1

  subroutine save_kb_dgf_d2(dG,file)
    type(kb_dgf)     :: dG(:,:)
    character(len=*) :: file
    integer          :: unit
    do i1=1,size(dG,1)
       do i2=1,size(dG,2)
          call save_kb_dgf_main(dG(i1,i2),&
               file//&
               "_io"//str(i1)//&
               "_jo"//str(i2)&
               )
       enddo
    enddo
  end subroutine save_kb_dgf_d2

  subroutine save_kb_dgf_d3(dG,file)
    type(kb_dgf)     :: dG(:,:,:)
    character(len=*) :: file
    integer          :: unit
    do i1=1,size(dG,1)
       do i2=1,size(dG,2)
          do i3=1,size(dG,3)
             call save_kb_dgf_main(dG(i1,i2,i3),&
                  file//&
                  "_i"//str(i1)//&
                  "_j"//str(i2)//&
                  "_k"//str(i3)&
                  )
          enddo
       enddo
    enddo
  end subroutine save_kb_dgf_d3

  subroutine save_kb_dgf_d4(dG,file)
    type(kb_dgf)     :: dG(:,:,:,:)
    character(len=*) :: file
    integer          :: unit
    Nspin=size(dG,1)
    Norb =size(dG,3)
    do ispin=1,Nspin
       do jspin=1,Nspin
          do iorb=1,Norb
             do jorb=1,Norb
                call save_kb_dgf_main(dG(ispin,jspin,iorb,jorb),&
                     file//"_l"//str(iorb)//str(jorb)//"_s"//str(ispin)//str(jspin))
             enddo
          enddo
       enddo
    enddo
  end subroutine save_kb_dgf_d4

  subroutine save_kb_dgf_d5(dG,file)
    type(kb_dgf)     :: dG(:,:,:,:,:)
    character(len=*) :: file
    integer          :: unit
    do i1=1,size(dG,1)
       do i2=1,size(dG,2)
          do i3=1,size(dG,3)
             do i4=1,size(dG,4)
                do i5=1,size(dG,5)
                   call save_kb_dgf_main(dG(i1,i2,i3,i4,i5),&
                        file//&
                        "_i"//str(i1)//&
                        "_j"//str(i2)//&
                        "_k"//str(i3)//&
                        "_r"//str(i4)//&
                        "_s"//str(i5)&
                        )
                enddo
             enddo
          enddo
       enddo
    enddo
  end subroutine save_kb_dgf_d5

  subroutine save_kb_dgf_d6(dG,file)
    type(kb_dgf)     :: dG(:,:,:,:,:,:)
    character(len=*) :: file
    integer          :: unit
    do i1=1,size(dG,1)
       do i2=1,size(dG,2)
          do i3=1,size(dG,3)
             do i4=1,size(dG,4)
                do i5=1,size(dG,5)
                   do i6=1,size(dG,6)
                      call save_kb_dgf_main(dG(i1,i2,i3,i4,i5,i6),&
                           file//&
                           "_i"//str(i1)//&
                           "_j"//str(i2)//&
                           "_k"//str(i3)//&
                           "_r"//str(i4)//&
                           "_s"//str(i5)//&
                           "_t"//str(i6)&
                           )
                   enddo
                enddo
             enddo
          enddo
       enddo
    enddo
  end subroutine save_kb_dgf_d6

  subroutine save_kb_dgf_d7(dG,file)
    type(kb_dgf)     :: dG(:,:,:,:,:,:,:)
    character(len=*) :: file
    integer          :: unit
    do i1=1,size(dG,1)
       do i2=1,size(dG,2)
          do i3=1,size(dG,3)
             do i4=1,size(dG,4)
                do i5=1,size(dG,5)
                   do i6=1,size(dG,6)
                      do i7=1,size(dG,7)
                         call save_kb_dgf_main(dG(i1,i2,i3,i4,i5,i6,i7),&
                              file//&
                              "_i"//str(i1)//&
                              "_j"//str(i2)//&
                              "_k"//str(i3)//&
                              "_r"//str(i4)//&
                              "_s"//str(i5)//&
                              "_t"//str(i6)//&
                              "_z"//str(i7)&
                              )
                      enddo
                   enddo
                enddo
             enddo
          enddo
       enddo
    enddo
  end subroutine save_kb_dgf_d7





  !##################################################################
  !##################################################################
  !##################################################################
  !##################################################################
  !##################################################################







  subroutine read_kb_dgf_main(dG,file)
    type(kb_dgf) :: dG
    character(len=*)     :: file
    logical              :: check
    check = inquire_kb_dgf(file)
    if(.not.dG%status.OR..not.check)stop "contour_gf/read_kb_dgf: dG not allocated"
    call read_array(trim(file)//"_less.data",dG%less(:))
    call read_array(trim(file)//"_ret.data",dG%ret(:))
    call read_array(trim(file)//"_lmix.data",dG%lmix(0:))
  end subroutine read_kb_dgf_main

  subroutine read_kb_dgf_d1(dG,file)
    type(kb_dgf) :: dG(:)
    character(len=*)    :: file
    integer             :: unit
    do i1=1,size(dG,1)
       call read_kb_dgf_main(dG(i1),&
            file//&
            "_i"//str(i1)&
            )
    enddo
  end subroutine read_kb_dgf_d1

  subroutine read_kb_dgf_d2(dG,file)
    type(kb_dgf) :: dG(:,:)
    character(len=*)    :: file
    integer             :: unit
    do i1=1,size(dG,1)
       do i2=1,size(dG,2)
          call read_kb_dgf_main(dG(i1,i2),&
               file//&
               "_io"//str(i1)//&
               "_jo"//str(i2)&
               )
       enddo
    enddo
  end subroutine read_kb_dgf_d2

  subroutine read_kb_dgf_d3(dG,file)
    type(kb_dgf) :: dG(:,:,:)
    character(len=*)    :: file
    integer             :: unit
    do i1=1,size(dG,1)
       do i2=1,size(dG,2)
          do i3=1,size(dG,3)
             call read_kb_dgf_main(dG(i1,i2,i3),&
                  file//&
                  "_i"//str(i1)//&
                  "_j"//str(i2)//&
                  "_k"//str(i3)&
                  )
          enddo
       enddo
    enddo
  end subroutine read_kb_dgf_d3

  subroutine read_kb_dgf_d4(dG,file)
    type(kb_dgf) :: dG(:,:,:,:)
    character(len=*)    :: file
    integer             :: unit
    Nspin=size(dG,1)
    Norb =size(dG,3)
    do ispin=1,Nspin
       do jspin=1,Nspin
          do iorb=1,Norb
             do jorb=1,Norb
                call read_kb_dgf_main(dG(ispin,jspin,iorb,jorb),&
                     file//"_l"//str(iorb)//str(jorb)//"_s"//str(ispin)//str(jspin))
             enddo
          enddo
       enddo
    enddo
  end subroutine read_kb_dgf_d4

  subroutine read_kb_dgf_d5(dG,file)
    type(kb_dgf) :: dG(:,:,:,:,:)
    character(len=*)    :: file
    integer             :: unit
    do i1=1,size(dG,1)
       do i2=1,size(dG,2)
          do i3=1,size(dG,3)
             do i4=1,size(dG,4)
                do i5=1,size(dG,5)
                   call read_kb_dgf_main(dG(i1,i2,i3,i4,i5),&
                        file//&
                        "_i"//str(i1)//&
                        "_j"//str(i2)//&
                        "_k"//str(i3)//&
                        "_r"//str(i4)//&
                        "_s"//str(i5)&
                        )
                enddo
             enddo
          enddo
       enddo
    enddo
  end subroutine read_kb_dgf_d5

  subroutine read_kb_dgf_d6(dG,file)
    type(kb_dgf) :: dG(:,:,:,:,:,:)
    character(len=*)    :: file
    integer             :: unit
    do i1=1,size(dG,1)
       do i2=1,size(dG,2)
          do i3=1,size(dG,3)
             do i4=1,size(dG,4)
                do i5=1,size(dG,5)
                   do i6=1,size(dG,6)
                      call read_kb_dgf_main(dG(i1,i2,i3,i4,i5,i6),&
                           file//&
                           "_i"//str(i1)//&
                           "_j"//str(i2)//&
                           "_k"//str(i3)//&
                           "_r"//str(i4)//&
                           "_s"//str(i5)//&
                           "_t"//str(i6)&
                           )
                   enddo
                enddo
             enddo
          enddo
       enddo
    enddo
  end subroutine read_kb_dgf_d6

  subroutine read_kb_dgf_d7(dG,file)
    type(kb_dgf) :: dG(:,:,:,:,:,:,:)
    character(len=*)    :: file
    integer             :: unit
    do i1=1,size(dG,1)
       do i2=1,size(dG,2)
          do i3=1,size(dG,3)
             do i4=1,size(dG,4)
                do i5=1,size(dG,5)
                   do i6=1,size(dG,6)
                      do i7=1,size(dG,7)
                         call read_kb_dgf_main(dG(i1,i2,i3,i4,i5,i6,i7),&
                              file//&
                              "_i"//str(i1)//&
                              "_j"//str(i2)//&
                              "_k"//str(i3)//&
                              "_r"//str(i4)//&
                              "_s"//str(i5)//&
                              "_t"//str(i6)//&
                              "_z"//str(i7)&
                              )
                      enddo
                   enddo
                enddo
             enddo
          enddo
       enddo
    enddo
  end subroutine read_kb_dgf_d7






  !##################################################################
  !##################################################################
  !##################################################################
  !##################################################################
  !##################################################################







  function inquire_kb_gf(file) result(check)
    integer          :: i
    logical          :: check,bool(5)
    character(len=*) :: file
    character(len=16),dimension(5)  :: ctype=([ character(len=5) :: 'less','ret','lmix','mats','iw'])
    check=.true.
    do i=1,5
       inquire(file=reg(file)//"_"//reg(ctype(i))//".data",exist=bool(i))
       if(.not.bool(i))inquire(file=reg(file)//"_"//reg(ctype(i))//".data.gz",exist=bool(i))
       check=check.AND.bool(i)
    enddo
  end function inquire_kb_gf
  !
  function inquire_kb_dgf(file) result(check)
    integer          :: i
    logical          :: check,bool(3)
    character(len=*) :: file
    character(len=16),dimension(3)  :: ctype=([character(len=5) :: 'less','ret','lmix'])
    check=.true.
    do i=1,3
       inquire(file=reg(file)//"_"//reg(ctype(i))//".data",exist=bool(i))
       if(.not.bool(i))inquire(file=reg(file)//"_"//reg(ctype(i))//".data.gz",exist=bool(i))
       check=check.AND.bool(i)
    enddo
  end function inquire_kb_dgf




END MODULE KB_GF_IO
