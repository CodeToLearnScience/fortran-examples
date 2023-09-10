
program test_int_to_str
  implicit none 
  
  !character(:), allocatable :: str 
  
  !str=itoa(1000000)

  print*, itoa(0), ", ", itoa(1), ", ", itoa(99), ", ", itoa(1000000)

contains 
  !character(len=*) function itoa(i) result(res)
function itoa(i) result(res)
  character(:),allocatable :: res
  integer,intent(in) :: i
  character(range(i)+2) :: tmp
  write(tmp,'(i0)') i
  res = trim(tmp)
end function


end program test_int_to_str
