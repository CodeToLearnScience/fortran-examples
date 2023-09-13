!decalre the c interface function 
module c_interface_func
use, intrinsic :: iso_c_binding
interface c_cuda 
  subroutine array_sum_func(N, host_array, tot_sum) bind(C,name='array_sum')
    import c_int, c_ptr, c_double
    implicit none 
    integer(kind=c_int), value :: N
    type(c_ptr), value :: host_array 
    real(c_double) :: tot_sum 
  end subroutine array_sum_func 
end interface c_cuda 
end module c_interface_func

program test_fortran_c_interoperability
!#include <array_sum.h>
  use, intrinsic :: iso_c_binding 
  use c_interface_func
  implicit none 
  
  real (c_double), allocatable, target, dimension(:) :: a
  real (c_double) :: sum   
  integer (c_int) :: nx
  integer :: i  

  nx=100
  allocate(a(0:nx-1))
  
  do i=0,nx-1
    a(i)=i
  enddo

  call array_sum_func(nx, c_loc(a), sum)

  print*, "Sum of array computing from C function: ", sum 
end program test_fortran_c_interoperability 


