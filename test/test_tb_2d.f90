program TB2d
  USE KB_LIB
  USE SCIFOR
  USE DMFT_TOOLS
  implicit none
  integer                                     :: i,ik,itime,Lk,Nx,Nso
  real(8)                                     :: v,mh,ts(2),dMh,ton,toff
  character(len=16)                           :: finput
  !
  type(kb_gf),dimension(:,:),allocatable      :: Gloc
  type(kb_gf),dimension(:,:,:),allocatable    :: Gk
  type(kb_dgf),dimension(:,:,:),allocatable   :: dGk,dGk_old
  !RESULTS:
  complex(8),dimension(:,:,:,:),allocatable   :: Hkt !
  real(8),dimension(:,:),allocatable          :: kgrid
  real(8),dimension(:),allocatable            :: dens1

  !READ THE INPUT FILE (in vars_global):
  call parse_cmd_variable(finput,"FINPUT",default='input.conf')
  call parse_input_variable(ts,"TS",finput,default=[1d0,1d0,1d0],comment="intra-orbital hopping")
  call parse_input_variable(v,"V",finput,default=0d0,comment="inter-orbital non-local amplitude")
  call parse_input_variable(mh,"MH",finput,default=0d0,comment="Crystal field")
  call parse_input_variable(dMh,"dMh",finput,default=0d0,comment="Crystal field increment")
  call parse_input_variable(Nx,"Nx",finput,default=21,comment="Number of k-points")
  call parse_input_variable(ton,"TON",finput,default=0.5d0,comment="")
  call parse_input_variable(toff,"TOFF",finput,default=2d0,comment="")
  call kb_read_input(trim(finput))

  Nso = Nspin*Norb
  if(Nso/=1)stop "This code is under testing. Nso!=1 is not allowed"

  !Add DMFT CTRL Variables:
  call add_ctrl_var(Norb,"norb")
  call add_ctrl_var(Nspin,"nspin")
  call add_ctrl_var(beta,"beta")
  call add_ctrl_var(xmu,"xmu")
  call add_ctrl_var((-1d0*5d0),'wini')
  call add_ctrl_var(5d0,'wfin')
  call add_ctrl_var(0.01d0,"eps")

  !BUILD TIME GRIDS AND NEQ-PARAMETERS:
  call setup_kb_contour_params()


  !BUILD THE LATTICE STRUCTURE (use tight_binding):
  Lk = Nx*Nx ; write(*,*) "Using Nk_total="//txtfy(Lk)

  allocate(kgrid(Lk,2))
  call TB_set_bk([pi2,0d0],[0d0,pi2])
  call TB_build_kgrid([Nx,Nx],kgrid)


  allocate(Hkt(Nso,Nso,Lk,Ntime))
  do i=1,cc_params%Ntime
     do ik=1,Lk
        Hkt(:,:,ik,i) = hkt_model(kgrid(ik,:),cc_params%t(i),Nso)
     enddo
  enddo


  !ALLOCATE ALL THE FUNCTIONS INVOLVED IN THE CALCULATION:
  allocate(Gloc(Nso,Nso))
  allocate(Gk(Nso,Nso,Lk),dGk(Nso,Nso,Lk),dGk_old(Nso,Nso,Lk))
  call Gloc%init()
  call Gk%init()
  call dGk%init()
  call dGk_old%init()


  allocate(dens1(Nso))
  !START THE TIME_STEP LOOP  1<=t<=Nt
  do itime=1,cc_params%Ntime
     write(*,"(A20,I10)",advance="no")"time step=",itime
     cc_params%Nt=itime
     !
     dGk_old = dGk
     !
     !evolve the G_k(t,t') and sum into G_loc
     call free_kb_gf(Hkt,Gk,dGk_old,dGk)
     Gloc = sum(Gk)/Lk
     !
     ! !EVALUATE AND PRINT THE RESULTS OF THE CALCULATION
     dens1(1:Nso) = get_dens_gk(itime,Nso)

     write(*,"(10F20.12)")&
          (dens1(i),i=1,Nso), (dimag(Gloc(i,i)%less(itime,itime)),i=1,Nso),&
          (dreal(sum(Hkt(i,i,:,itime)))/Lk,i=1,Nso)

  enddo

  call plot_kb_gf(Gloc(1,1),"Gloc")


contains


  function get_dens_gk(itime,N) result(ndens)
    integer :: itime,N
    real(8) :: ndens(N)
    integer :: ik,io
    ndens=0d0
    do ik=1,Lk
       do io=1,N
          ndens(io) = ndens(io) + dimag(Gk(io,io,ik)%less(itime,itime))/Lk
       enddo
    enddo
  end function get_dens_gk



  function hk_model(kpoint,N) result(hk)
    real(8),dimension(:)      :: kpoint
    integer                   :: N
    real(8)                   :: ek,vk
    real(8)                   :: kx,ky
    complex(8),dimension(N,N) :: hk
    kx=kpoint(1)
    ky=kpoint(2)
    ek = -2*(cos(kx)+cos(ky))
    vk = cos(kx)-cos(ky)!1d0
    Hk =  ek*diag(ts)
    if(N==1)Hk = Hk + Mh
    if(N==2)Hk = Hk + Mh*pauli_tau_z + v*vk*pauli_tau_x
  end function hk_model




  function hkt_model(kpoint,time,N) result(hk)
    real(8),dimension(:) :: kpoint
    real(8)              :: time
    integer              :: N
    real(8)              :: kx,ky,alpha
    complex(8)           :: hk(N,N)
    kx=kpoint(1)
    ky=kpoint(2)
    hk = hk_model([kx,ky],N)
    if(time<=ton)then
       alpha=0d0
    elseif(time>ton.AND.time<=toff)then
       alpha=1d0/(toff-ton)*(time-ton)
    else
       alpha=1d0
    endif
    if(N==1)hk = hk + alpha*dMh
    if(N==2)hk = hk + alpha*dMh*pauli_sigma_z
  end function Hkt_Model





end PROGRAM TB2d



