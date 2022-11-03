# NON-EQUILIBRIUM KELDYSH GREEN'S FUNCTIONS LIBRARY

##### *Library is in beta state: use with caution*

A Fortran library for Keldysh or Kadanoff-Baym non-equilibroum Green's functions calculations. 
The library provides interface to the most conventional operations involging KB Green's functions. 



### Dependencies

The code is based on:  

* SciFortran [https://github.com/aamaricci/SciFortran](https://github.com/aamaricci/SciFortran)  

* MPI  



### Installation

Installation is  available using CMake. In the current v0.0.1 API are only provided in Fortran.   


Clone the repo:

`git clone https://github.com/QcmPlab/kb_gf kb_gf`


And from the repository directory make a standard out-of-source CMake compilation:

`mkdir build`  
`cd build`  
`cmake ..` OR `cmake -GNinka ..`     
`make`     OR `ninja`  
`make install` OR `ninja install`    

Please follow the instructions on the screen to complete installation and load the library in your environment, using one of these, automatically generated, procedures:  

* pkg-config file in `~/.pkg-config.d/kb_gf.pc`  
* environment module file `~/.modules.d/kb_gf/<PLAT>`  
* homebrew `bash` script `<PREFIX>/bin/configvars.sh`


The `CMake` compilation can be controlled using the following additional variables, default values between `< >`:   

* `-DPREFIX=prefix directory <~/opt/dmft_ed/VERSION/PLAT/[GIT_BRANCH]>` 

* `-DUSE_MPI=<yes>/no`  

* `-DVERBOSE=yes/<no> `  

* `-DBUILD_TYPE=<RELEASE>/TESTING/DEBUG`  


For any information contact the author as:  
adriano DOT amaricci @ gmail DOT com


--

***COPYRIGHT & LICENSING***  
Copyright 2022 -  (c), Adriano Amaricci.  
All rights reserved. 

The software is provided with no license, as such it is protected by copyright. The software is provided as it is and can be read and copied, in agreement with the Terms of Service of GITHUB. Use of the code is constrained to author agreement.   

