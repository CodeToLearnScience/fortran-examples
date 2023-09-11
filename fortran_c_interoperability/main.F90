!decalre the c interface function 
module c_interface_func
interface
  real (c_double) function array_sum(x, n) bind(c, name='array_sum')
    use, intrinsic :: iso_c_binding
    implicit none 
    integer(kind=c_int), value :: n
    type(c_ptr), value :: x
  end function array_sum 
end interface 
end module c_interface_func

program test_fortran_c_interoperability
  use, intrinsic :: iso_c_binding 
  use c_interface_func
  implicit none 
  
  real (c_double), allocatable, target, dimension(:) :: a
  real (c_double) :: sum  
  integer (c_int) :: nx, i  

  nx=100
  allocate(a(0:nx-1))
  
  do i=0,nx-1
    a(i)=i
  enddo

  sum = array_sum(c_loc(a), nx)

  print*, "Sum of array computing from C function: ", sum 
end program test_fortran_c_interoperability 


