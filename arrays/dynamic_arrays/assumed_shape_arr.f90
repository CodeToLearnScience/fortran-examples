module test
  implicit none

  integer, allocatable, dimension(:) :: a

  contains 
    subroutine alloc_a
      implicit none 
      integer :: local_size 
      local_size = 4
      allocate(a(0:local_size+1))
    end subroutine alloc_a 

    subroutine use_a
      implicit none 
      call alloc_a
      print*, size(a)
    end subroutine
end module

program main  
  use test

  call use_a 
end program main 
