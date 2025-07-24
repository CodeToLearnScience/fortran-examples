! main.f90
program main
  use iso_c_binding
  implicit none

  ! Interface to C functions
  interface
  function create_array(size) bind(C, name="create_array") result(arr)
    import :: c_int, c_double, c_ptr
    integer(c_int), value :: size
    type(c_ptr) :: arr
  end function create_array

  subroutine free_array(arr) bind(C, name="free_array")
    import :: c_ptr
    type(c_ptr), value :: arr
  end subroutine free_array
  end interface

  ! Variables
  integer, parameter :: n = 5
  type(c_ptr) :: c_arr_ptr
  real(c_double), pointer :: f_arr(:) => null()
  integer :: i

  ! Call C function to create array
  c_arr_ptr = create_array(n)

  ! Convert C pointer to Fortran pointer
  call c_f_pointer(c_arr_ptr, f_arr, [n])

  ! Now we can use the array in Fortran
  write(*,*) "Array values:"
  do i = 1, n
    write(*,*) "Element", i, "=", f_arr(i)
  end do

  ! Free memory allocated in C
  call free_array(c_arr_ptr)

end program main
