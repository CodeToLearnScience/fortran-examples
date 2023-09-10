program transpose_multidim_array
  implicit none
  
  real, allocatable, dimension(:,:,:,:) :: f, c
  integer :: nx, ny, nz, r
  integer :: i, j, k, l

  nx=5
  ny=4
  nz=3
  r=2

  allocate(f(nx,ny,nz,r))
  allocate(c(r,nz,ny,nx))

  call random_number(f)

  forall(i=1:nx, j=1:ny, k=1:nz, l=1:r) c(l,k,j,i) = f(i,j,k,l)

  print*, f(1,:,:,:)  
  !print*, "fortran: ", f 

  !print*, '------------------------'

  !print*, "c: ", c

end program transpose_multidim_array
