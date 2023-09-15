!fortran interface to c functions which will be called from fortran  
module fortran_c_interface
  use, intrinsic :: iso_c_binding, only : c_int, c_double, c_ptr 
  implicit none 
  interface 
    !interface to c function (returns double) array_sum 
    real (c_double) function array_sum(x, n) bind(c, name='array_sum')
      import :: c_int, c_ptr, c_double 
      implicit none 
      integer(c_int), value :: n
      type(c_ptr), value :: x
    end function array_sum 

    !interface to c function (void) sum_two_arrays 
    subroutine sum_two_arrays(N, a, b, c) bind(C, name="sum_two_arrays")
      import :: c_int, c_double, c_ptr 
      implicit none 
      integer(c_int), intent(in), value :: N 
      type(c_ptr), intent(in), value :: a, b
      real(c_double), intent(inout) :: c(*)
    end subroutine sum_two_arrays 
  end interface 
end module fortran_c_interface 

!program test_fortran_c_interoperability
!  use, intrinsic :: iso_c_binding 
!  use c_interface_func
!  implicit none 
!  
!  real (c_double), allocatable, target, dimension(:) :: a, b
!  real (c_double), pointer :: c(:)
!  real (c_double) :: sum  
!  integer (c_int) :: nx, i  
!
!  nx=5
!  allocate(a(0:nx-1))
!  allocate(b(0:nx-1))
!  allocate(c(0:nx-1))
!  
!  do i=0,nx-1
!    a(i)=i
!    b(i) = i+1
!  enddo
!
!  sum = array_sum(c_loc(a), nx)
!  print*, "Sum of array computed from C function (called from Fortran): ", sum 
!
!  call sum_two_arrays(nx, c_loc(a), c_loc(b), c)
!
!  print*, "Summed array is: ", c
!  
!  deallocate(a,b,c)
!
!end program test_fortran_c_interoperability 


