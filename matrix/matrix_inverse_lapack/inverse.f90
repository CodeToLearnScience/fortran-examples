!To compute matrix inverse
program inverse_matrix
  implicit none
  
  integer :: n, status, i, j
  real(kind=8), allocatable :: a(:,:), work(:)
  integer, allocatable :: ipiv(:)
  
  ! Set the size of the matrix
  n = 3
  
  ! Allocate memory for the matrix and pivot array
  allocate(a(n,n), ipiv(n), work(n*n))
  
  ! Set the matrix values
  a = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 10.0], [n,n])
  
  ! Compute the LU factorization of the matrix
  call dgetrf(n, n, a, n, ipiv, status)
  
  if (status /= 0) then
    print *, "Error: LU factorization failed."
    stop
  end if
  
  ! Compute the inverse of the matrix
  call dgetri(n, a, n, ipiv, work, n*n, status)
  
  if (status /= 0) then
    print *, "Error: Matrix inversion failed."
    stop
  end if
  
  ! Print the inverse matrix
  print *, "Inverse matrix:"
  do i = 1, n
    print "(3F10.5)", (a(i,j), j=1,n)
  end do
  
  ! Deallocate memory
  deallocate(a, ipiv, work)

end program inverse_matrix

