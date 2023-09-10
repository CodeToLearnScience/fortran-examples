program example
  implicit none
  real(kind=8) :: x
  !x = 1.23456789012345d308
  x = -143250985349058094

  ! Output x in general format
  write(*, '(G)') x
end program example

