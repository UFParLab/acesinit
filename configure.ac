#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.63])
AC_INIT([ACES4INIT], [1.0], [sanders@cise.ufl.edu])
AC_CONFIG_MACRO_DIR([m4])
m4_include([m4/ax_blas.m4])
m4_include([m4/ax_lapack.m4])
m4_include([m4/ax_prog_cc_mpi.m4])
m4_include([m4/ax_prog_cxx_mpi.m4])
m4_include([m4/ax_prog_f77_mpi.m4])
m4_include([m4/ax_prog_fc_mpi.m4])
m4_include([m4/ax_append_flag.m4])
m4_include([m4/ax_check_compile_flag.m4])
m4_include([m4/ax_append_compile_flags.m4])
m4_include([m4/ax_config_feature.m4])
m4_include([m4/ax_compiler_vendor.m4])
m4_include([m4/ax_gcc_archflag.m4])
m4_include([m4/ax_gcc_x86_cpuid.m4])
m4_include([m4/ax_cc_maxopt.m4])
m4_include([m4/ax_ext.m4])
m4_include([m4/acx_pthread.m4])
m4_include([m4/ax_openmp.m4])
m4_include([m4/ax_prog_doxygen.m4])

AC_CONFIG_SRCDIR([src/include])
AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_PROG_CXX([icpc g++])
AC_PROG_CC([icc gcc])
AC_PROG_F77([ifort gfortran])
AC_PROG_FC([ifort gfortran])


# Doxygen
DX_HTML_FEATURE(ON)
DX_CHM_FEATURE(OFF)
DX_CHI_FEATURE(OFF)
DX_MAN_FEATURE(OFF)
DX_RTF_FEATURE(OFF)
DX_XML_FEATURE(OFF)
DX_PDF_FEATURE(ON)
DX_PS_FEATURE(OFF)

DX_INIT_DOXYGEN([$PACKAGE_NAME],[doxygen.cfg])


# Enabling compiler vendor specific flags for which 
# there was no easily available macro
AX_COMPILER_VENDOR
case $ax_cv_c_compiler_vendor in
	gnu)
		AC_LANG([C])
		AC_LANG([C++])
		AC_LANG([Fortran])
        	AX_APPEND_COMPILE_FLAGS([-finit-local-zero])
	        AX_APPEND_COMPILE_FLAGS([-Wuninitialized])
		AC_LANG([Fortran 77])
	        AX_APPEND_COMPILE_FLAGS([-finit-local-zero])
	        AX_APPEND_COMPILE_FLAGS([-Wuninitialized])
	;;
	intel)
		AC_LANG([C])
		AC_LANG([C++])
		AC_LANG([Fortran])
		AX_APPEND_COMPILE_FLAGS([-zero])		  # init saved vars to zero
		AX_APPEND_COMPILE_FLAGS([-Zp8])
		AC_LANG([Fortran 77])
		AX_APPEND_COMPILE_FLAGS([-zero])		  # init saved vars to zero
		AX_APPEND_COMPILE_FLAGS([-Zp8])
	;;
	portland)
		AC_LANG([C])
		AC_LANG([C++])
		AC_LANG([Fortran])
		AC_LANG([Fortran 77])
	;;
esac

AC_LANG([C])

# For Libtool
AC_PROG_LIBTOOL

# For using target specific Primaries in Makefile.am
AM_PROG_CC_C_O

# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([inttypes.h limits.h stddef.h stdlib.h string.h strings.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_TYPE_INT64_T
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_MALLOC
AC_CHECK_FUNCS([memmove memset mkdir strcasecmp strcspn strncasecmp strpbrk strspn])


# OpenMP Flags for Compiling available in OPENMP_CFLAGS, OPENMP_FCFLAGS, etc.
# AC_LANG([Fortran])
# AX_OPENMP
# AC_LANG([Fortran 77])
# AX_OPENMP

# For pthread library linking
AC_LANG([C])
ACX_PTHREAD
AC_SUBST(PTHREAD_LIBS)
AC_SUBST(PTHREAD_CFLAGS)
AC_SUBST(PTHREAD_CC)

# Defines C macros to properly mangle the names of C/C++ identifiers, 
# and identifiers with underscores so that they 
# match the name-mangling scheme used by the Fortran compiler. 
AC_F77_WRAPPERS
AC_FC_WRAPPERS


AC_F77_LIBRARY_LDFLAGS
AC_FC_LIBRARY_LDFLAGS

# Takes care of certain compilers trying to declare their own MAIN_
AC_F77_MAIN
AC_FC_MAIN

# To deal with the ACES2 part in the initialization executable
AC_LANG([Fortran])
AX_APPEND_COMPILE_FLAGS([-D__fortran -D__fortran77])
AC_LANG([Fortran 77])
AX_APPEND_COMPILE_FLAGS([-D__fortran -D__fortran77])



AM_INIT_AUTOMAKE([foreign -Wall tar-ustar])
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
