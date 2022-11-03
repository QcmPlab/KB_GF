# NON-EQUILIBRIUM KELDYSH GREEN'S FUNCTIONS LIBRARY

##### *Library is in beta state: use with caution*

A Fortran library for Keldysh or Kadanoff-Baym non-equilibroum Green's functions calculations. 
The library provides interface to the most conventional operations involging KB Green's functions. 



### Dependencies

The code is based on:  

* [GNU Fortran (`gfortran`)](https://gcc.gnu.org/fortran/) > 5.0 **OR** [Intel Fortran Compiler Classic (`ifort`)](https://www.intel.com/content/www/us/en/developer/tools/oneapi/fortran-compiler.html)  > 13.0

* [CMake](https://cmake.org/) ≥ 3.0 [> 3.16 for ninja support] 
* [Make](https://www.gnu.org/software/make/) **OR** [Ninja](https://ninja-build.org/) ≥ 1.10 
* [SciFortran](https://github.com/aamaricci/SciFortran)  

Further details are available at the SciFortran repo page. Succeding to install SciFortran usually means that all dependencies are automatically statisfied already.  


### Installation

Installation is  available using CMake. In the current v0.0.1 API are only provided in Fortran.   


Clone the repo:

```
git clone https://github.com/QcmPlab/kb_gf kb_gf
```

Optionally define the fortran compiler:

```
export FC=mpif90 # or gfortran or ifort if you don't need MPI
```


And from the repository directory make a standard out-of-source CMake compilation:

<details>
<summary> Using <tt>make</tt> (click to expand) </summary>
Default CMake workflow, with widest version support (CMake > 3.0).

```
mkdir build 
cd build  
cmake .. 
make
```      

</details>

<details>
<summary> Using <tt>ninja</tt> (click to expand)</summary>

If a fortran-capable[^3] version of `ninja` ( https://ninja-build.org ) is available in your system (and CMake can[^4] take advantage of it), you can use it to build the library at lightning, multi-threaded, speed. 

```
mkdir build    
cd build  
cmake -GNinja ..  
ninja
```       

</details>

  

Please follow the instructions on the screen to complete installation and load the library in your environment, using one of these, automatically generated, procedures:  

* pkg-config file in `~/.pkg-config.d/kb_gf.pc`  
* environment module file `~/.modules.d/kb_gf/<PLAT>`  
* homebrew `bash` script `<PREFIX>/bin/configvars.sh`


The `CMake` compilation can be controlled using the following additional variables, default values between `< >`:   

* `-DPREFIX=prefix directory <~/opt/dmft_ed/VERSION/PLAT/[GIT_BRANCH]>` 

* `-DUSE_MPI=<yes>/no`  

* `-DVERBOSE=yes/<no> `  

* `-DBUILD_TYPE=<RELEASE>/TESTING/DEBUG`  



## INSTALL

System-wide installation is completed after the build step using either: 

```
make install
```  

or   

```
ninja install
```  
 
To actually link the library we provide some alternatives:

* A generated [pkg-config](https://github.com/freedesktop/pkg-config) file to, installed to `~/.pkgconfig.d/kb_gf.pc`  
* A generated [environment module](https://github.com/cea-hpc/modules), installed to `~/.modules.d/kb_gf/<PLAT>/<VERSION>`  
* A generated `bash` script at `<PREFIX>/bin/configvars.sh`, to be sourced for permanent loading.

which you can choose among by following the instructions printed on screen.


## UNINSTALL

CMake does not officially provide uninstall procedures in the generated Make/Ninja files. Hence SciFortran supplies a homebrew method to remove the generated files by calling (from the relevant build folder): `make uninstall` / `ninja uninstall`.


## DOCUMENTATION
work in progress...


## KNOWN PROBLEMS
work in progress... report any if you test this library


### CONTACT

If you encounter bugs or difficulties, please [file an issue](https://github.com/QcmPlab/KB_GF/issues/new/choose). For any other communication, please reach out to:   
adriano DOT amaricci @ gmail DOT com



--

***COPYRIGHT & LICENSING***  
Copyright 2022 -  (c), Adriano Amaricci.  
All rights reserved. 

The software is provided with no license, as such it is protected by copyright. The software is provided as it is and can be read and copied, in agreement with the Terms of Service of GITHUB. Use of the code is constrained to author agreement.   

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License (LGPL) as published by
the Free Software Foundation, either version 3 of the License, or any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU LGPL for more details.

You should have received a copy of the GNU LGPL along with this program.  If not, see <http://www.gnu.org/licenses/>.




