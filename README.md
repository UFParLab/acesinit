acesinit
========

aces4 initialization program.
Preprocessing program for aces4 [https://github.com/UFParLab/aces4].

It reads in a file containing the molecule, quantum mechanical method and basis set and creates an initialization file.

Prerequisites
-------------
```
C, C++ & Fortran Compiler
automake
autoconf
libtool
```

To Compile
----------
```BASH
./autogen.sh
mkdir BUILD
cd BUILD
../configure
make 
```
This should create the ```acesinit``` executable.

An alternative is to use CMake
```BASH
mkdir BUILD
cd BUILD
cmake ../
make 
```

To Use
------
Copy all the files in ```src/test``` and the ```acesinit``` executable to a separate directory. This folder should contain these files:
```BASH
GENBAS
ZMAT
acesinit
```
The ZMAT file contains the molecule configuration and other configuration parameters.
The GENBAS file contains basis set information.

In this directory, run the acesinit file.
The generated ```data.dat``` binary file contains initialization data for aces4
and should replicate the results from the Aces4 Sial_QM.second_ccsdpt_test
google test.

Default job flows are listed at [https://github.com/UFParLab/aces4/wiki/Quantum-Chemistry-Domain-Capabilities#ground-state-jobflows]

Utility
-------
The "init_file_print" executable can be used to dump the contents of an initialization file to screen. Example usage : ```./init_file_print data.dat```.


