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
default_jobflows
GENBAS
sial_config
ZMAT
acesinit
```
The ZMAT file contains the molecule configuration and other configuration parameters.
The GENBAS file contains basis set information.
The sial_config file contains configuration for each of the SIAL files.
The default_jobflows file contains "jobs" or sets of SIAL files to be run one after the other to execute a particular quantum mechanical method.

In this directory, run the acesinit file.
The generated ```data.dat``` binary file contains initialization data for aces4.

Utility
-------
The "init_file_print" executable can be used to dump the contents of an initialization file to screen. Example usage : ```./init_file_print data.dat```.


