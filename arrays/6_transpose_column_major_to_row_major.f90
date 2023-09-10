!subroutine transpose(a, dims)
!    implicit none
!    integer, intent(in) :: dims(:)
!    real, intent(inout) :: a(dims(:))
!    real :: temp(dims(:))
!    integer :: i, j
!    integer, dimension(size(dims)) :: perm
!    perm = [size(dims):1]
!    temp = reshape(transpose(reshape(a, [dims(perm)])), dims)
!    a = temp
!end subroutine transpose

subroutine reshape_array(array, reshaped_array)
    implicit none

    integer, dimension(5,4,3,2) :: array
    integer, dimension(5*4*3*2) :: reshaped_array
    integer :: i, j, k, l

    ! Copy the elements of the original array into the reshaped array
    do i = 1, 5
        do j = 1, 4
            do k = 1, 3
                do l = 1, 2
                    reshaped_array((((l-1)*3 + k-1)*4 + j-1)*5 + i) = array(i,j,k,l)
                end do
            end do
        end do
    end do

end subroutine reshape_array


program transpose_mdarray
implicit none

real :: a1(5,3)
real :: a1_c(3,5)
real :: a1_flat(5*3)
integer :: i, j
integer :: dim(2)

dim(1) = 4
dim(2) = 3

print*, "dims: ", dim

do i=1,5
	do j=1,3
		a1(i,j)=i*j
	end do
end do

a1_flat = pack(a1,.true.)

print*, a1_flat

print*, '----------------row major-----------'

a1_c = reshape(a1, shape(a1_c), order=[2,1])
		
a1_flat = pack(a1_c,.true.)

print*, a1_flat

end	program transpose_mdarray

