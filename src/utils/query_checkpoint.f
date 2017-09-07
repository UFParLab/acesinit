!>  Interfaces for routines to support getting reading persistent
!>   static arrays from a checkpoint file
!>
!>  Usage:  include this file in the program
!>  Option 1
!>  Begin with a call to open_checkpoint and
!>   and end with a call to finalize_setup.
!>
!>  @see query_checkpoint.h and query_checkpoint.cpp for C++ implementation
!>
!>  @warning  ISO Fortran 2003 standard is used For compatibility with C/C++.
!>      All strings  must be null terminated.
!>      Example: call open_checkpoint('file_name'//C_NULL_CHAR)
!>
!>      Variables that will given as parameters should be declared with type
!>         integer(C_INT) or real(C_DOUBLE)
!>
!> These definitions are made available by including the following statement
!>      use, intrinsic :: ISO_C_BINDING




!>  Open and read the checkpoint file.
!>  @param name The null terminated name of the checkpoint file.
      interface
      subroutine open_checkpoint (name) bind(C)
      use, intrinsic::ISO_C_BINDING
      implicit none
      character(kind=c_char, len=1), intent(in) :: name(*)      
      end subroutine open_checkpoint
      end interface
      interface


      subroutine get_persistent_static(label,num_elems,
     *                                  extents,vals) bind(C)
      use, intrinsic :: ISO_C_BINDING
      implicit none
      character(kind=c_char, len=1), intent(in) :: label(*)
      integer (C_INT), intent(out)::num_elems
      TYPE(C_PTR), intent(out)::extents
      TYPE(C_PTR), intent(out)::vals
      end subroutine get_persistent_static
      end interface



!> Closes checkpoint file and release resources
      interface
      subroutine close_checkpoint() bind(C)
      end subroutine close_checkpoint
      end interface

