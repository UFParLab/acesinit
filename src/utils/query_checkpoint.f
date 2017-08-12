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
!>  @param file_name The null terminated name of the checkpoint file.

      interface
      subroutine open_checkpoint (file_name) bind(C)
      character, dimension(*), intent(in) :: file_name
      end subroutine open_checkpoint
      end interface


      interface
      subroutine get_persistent_static(label,num_elems,
     *                                  dims,vals) bind(C)
      use, intrinsic :: ISO_C_BINDING
      character, dimension(*), intent(in):: label
      integer (C_INT), intent(out)::num_elems
      TYPE(C_PTR), intent(out)::dims
      TYPE(C_PTR), intent(out)::vals
      end subroutine get_persistent_static
      end interface


      interface
      subroutine get_persistent_static_from_checkpoint(
     *             filename, label,num_elems,
     *                                  dims,vals) bind(C)
      use, intrinsic :: ISO_C_BINDING
      character, dimension(*), intent(in):: filename
      character, dimension(*), intent(in):: label
      integer (C_INT), intent(out)::num_elems
      TYPE(C_PTR), intent(out)::dims
      TYPE(C_PTR), intent(out)::vals
      end subroutine get_persistent_static_from_checkpoint
      end interface

!> Closes checkpoint file and release resources

      interface
      subroutine close_checkpoint() bind(C)
      end subroutine close_checkpoint
      end interface

