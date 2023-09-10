program dynamic_array
  implicit none
  
  integer, allocatable :: arr(:), tmp_arr(:) ! Declare an allocatable integer array
  integer :: n, i, capacity, new_capacity

  capacity = 5 ! Starting capacity of the array
  n = 0 ! Current number of elements in the array
  allocate(arr(capacity)) ! Allocate space for capacity elements in the array

  do i = 1, 1000
    if (n == capacity) then ! If the array is full, increase its size
      new_capacity = capacity * 2 ! Double the capacity
      allocate(tmp_arr(new_capacity)) ! Allocate a temporary array with the new capacity
      tmp_arr(1:n) = arr(1:n) ! Copy the existing elements to the temporary array
      call move_alloc(tmp_arr, arr) ! Move the allocation of the temporary array to the original array
      capacity = new_capacity ! Update the capacity
    end if

    n = n + 1 ! Increase the current number of elements
    arr(n) = n ! Append the new element
  end do

  ! Deallocate the array
  deallocate(arr)
end program dynamic_array

