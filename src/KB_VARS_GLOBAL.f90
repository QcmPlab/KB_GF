MODULE KB_VARS_GLOBAL
  USE KB_VERSION
  USE SF_VERSION
  USE SF_PARSE_INPUT
  implicit none

  !Gloabl  variables
  integer                 :: Norb=1          !Norb =# of impurity orbitals
  integer                 :: Nspin=1         !Nspin=# spin degeneracy (max 2)
  integer                 :: Ntime=1         !Number of Time steps
  integer                 :: Ntau=64         !Imaginary time slices
  integer                 :: Niw=4096        !Number of Matsubara frequencies
  integer                 :: Nwr=1000        !Number of real-axis frequencies
  real(8)                 :: dt=0.1d0        !real-time step
  real(8)                 :: beta=10d0       !inverse temperature
  real(8)                 :: wmax=5d0        !max freq. range
  real(8)                 :: xmu=0d0         !chemical potential (shifted by Uloc/2)

contains

  subroutine kb_read_input(inputFILE)
    character(len=*)               :: inputFILE
    call parse_input_variable(Norb       , "NORB",inputFILe,default=1,comment="Number of impurity orbitals.")
    call parse_input_variable(Nspin      , "NSPIN",inputFILE,default=1,comment="Number of spin degeneracy (max 2)")
    call parse_input_variable(Ntime      , "NTIME" , inputFILE , default      =100 , comment="Number of Real-Time steps")
    call parse_input_variable(Ntau       , "NTAU" , inputFILE , default       =64 , comment="Number of Imag-Time steps")
    call parse_input_variable(Niw        , "NIW" , inputFILE , default        =4096 , comment="Number of Matsubara Frequencies")
    call parse_input_variable(Nwr        , "NWR" , inputFILE , default        =1000 , comment="Number of real-axis Frequencies")
    call parse_input_variable(dt         , "DT" , inputFILE , default         =0.1d0 , comment="Real-time step")
    call parse_input_variable(beta       , "BETA" , inputFILE , default       =10d0 , comment="Inverse temperature")
    call parse_input_variable(wmax       , "WMAX",inputFILE, default          =5d0, comment="Max frequency")
    call parse_input_variable(xmu        , "XMU" , inputFILE , default        =0.d0 , comment="chemical potential")
    call save_input(inputFILE)
    call scifor_version()
    call code_version(version)
  end subroutine kb_read_input




end module KB_VARS_GLOBAL

