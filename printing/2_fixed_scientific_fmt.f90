program example
  implicit none
  integer :: i
  real(kind=8) :: x
  x = 86787687100010456475686780d0
  !x = 3424d0
  i=1897989089
  
  ! Output x in scientific notation with 6 significant digits
  write(*, '(ES12.4)') x
  write(*, '(i15)') i
end program example

