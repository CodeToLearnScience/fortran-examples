program test_fortran_c_interoperability
  use, intrinsic :: iso_c_binding, only : c_double, c_int, c_loc  
  use fortran_c_interface 
  implicit none 
  
  real (c_double), allocatable, target, dimension(:) :: a, b
  real (c_double), pointer :: c(:)
  real (c_double) :: sum  
  integer (c_int) :: nx, i  

  nx=5
  allocate(a(0:nx-1))
  allocate(b(0:nx-1))
  allocate(c(0:nx-1))
  
  do i=0,nx-1
    a(i)=i
    b(i) = i+1
  enddo

  sum = array_sum(c_loc(a), nx)
  print*, "Sum of array computed from C function (called from Fortran): ", sum 

  call sum_two_arrays(nx, c_loc(a), c_loc(b), c)

  print*, "Summed array is: ", c
  
  deallocate(a,b,c)

end program test_fortran_c_interoperability 


