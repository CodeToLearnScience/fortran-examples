program where_usage
  integer(kind=4), dimension(:), allocatable :: a, b

  allocate(a(5), b(5))

  a=[0,1,0,3,4]
  b=0

  where(a.ne.0) b=1

  print*, b
    
end program where_usage
