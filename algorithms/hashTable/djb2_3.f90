! fortran_int_dict.f90
! An optimized dictionary implementation for integer values in Fortran

module fortran_int_dict
  use, intrinsic :: iso_fortran_env, only: int32, int64
  implicit none
  private

  ! Define a key-value pair type specifically for integer values
  type :: key_value_pair
    character(len=:), allocatable :: key
    integer :: value              ! Specific integer type for optimization
    logical :: occupied = .false. ! Flag to indicate if slot is in use
    logical :: deleted = .false.  ! Flag for tombstone in open addressing
  end type key_value_pair

  ! Define an integer-optimized dictionary type
  type, public :: int_dict
    private
    type(key_value_pair), allocatable :: items(:)
    integer :: size = 0
    integer :: capacity = 0
    integer :: tombstones = 0     ! Count of deleted entries
    real :: load_factor = 0.70    ! Decreased for better performance
    real :: tombstone_factor = 0.15 ! Threshold for cleanup
    contains
    procedure :: init => initialize_dict
    procedure :: get => get_value
    procedure :: set => set_value
    procedure :: has_key => contains_key
    procedure :: delete => delete_key
    procedure :: get_keys => get_all_keys
    procedure :: get_values => get_all_values
    procedure :: get_items => get_all_items
    procedure :: clear => clear_dict
    procedure :: length => dict_length
    procedure :: print => print_dict
    procedure, private :: resize => resize_dict
    procedure, private :: cleanup => cleanup_tombstones
    procedure, private :: hash => hash_function
    procedure, private :: find_index => find_key_index
    procedure, private :: probing_distance => compute_probing_distance
  end type int_dict

  ! Public constructor
  public :: create_int_dict

  contains

  ! Constructor function with optional initial capacity
  function create_int_dict(initial_capacity) result(new_dict)
    integer, intent(in), optional :: initial_capacity
    type(int_dict) :: new_dict
    integer :: init_cap

    if (present(initial_capacity)) then
      init_cap = initial_capacity
    else
      init_cap = 16  ! Increased default initial capacity
    end if

    ! Make initial capacity a power of 2 for more efficient modulo operations
    init_cap = ceiling_power_of_two(init_cap)

    call new_dict%init(init_cap)
  end function create_int_dict

  ! Find the next power of 2 >= n
  function ceiling_power_of_two(n) result(power_of_two)
    integer, intent(in) :: n
    integer :: power_of_two
    integer :: temp

    temp = 1
    do while (temp < n)
      temp = temp * 2
    end do

    power_of_two = temp
  end function ceiling_power_of_two

  ! Initialize the dictionary
  subroutine initialize_dict(this, capacity)
    class(int_dict), intent(inout) :: this
    integer, intent(in) :: capacity
    integer :: i

    if (allocated(this%items)) deallocate(this%items)
    allocate(this%items(capacity))

    ! Initialize all items as unoccupied
    do i = 1, capacity
      this%items(i)%occupied = .false.
      this%items(i)%deleted = .false.
    end do

    this%size = 0
    this%capacity = capacity
    this%tombstones = 0
  end subroutine initialize_dict

  ! Optimized hash function for string keys
  ! Using a combination of FNV-1a and multiplicative hash for better distribution
  function hash_function(this, key) result(hash_val)
    class(int_dict), intent(in) :: this
    character(len=*), intent(in) :: key
    integer :: hash_val
    integer :: i
    integer(int32) :: h ! Use explicit int32 for the hash calculation

    ! Start with FNV offset basis that fits in 32 bit
    h = 2166136261_int32

    ! Optimized loop for hash calculation
    do i = 1, len(key)
      h = ieor(h, ichar(key(i:i)))
      h = h * 16777619_int32 ! FNV prime for 32-bit
    end do

    ! Make sure hash_val is positive
    hash_val = abs(int(h, kind=kind(hash_val)))

    ! Fast modulo for power-of-2 sizes using bitwise AND
    ! This works because capacity is always a power of 2
    hash_val = iand(hash_val, this%capacity - 1) + 1
  end function hash_function

  ! Calculate probing distance for quadratic probing
  ! Better distribution than linear probing for clustering
  function compute_probing_distance(this, attempt) result(distance)
    class(int_dict), intent(in) :: this
    integer, intent(in) :: attempt
    integer :: distance

    ! Quadratic probing: i^2 + i / 2
    distance = (attempt * attempt + attempt) / 2
  end function compute_probing_distance

  ! Find the index of a key using quadratic probing
  function find_key_index(this, key, for_insertion) result(idx)
    class(int_dict), intent(in) :: this
    character(len=*), intent(in) :: key
    logical, intent(in) :: for_insertion
    integer :: idx
    integer :: i, probe_idx, first_tombstone
    integer :: start_idx
    logical :: found_tombstone

    if (this%capacity == 0) then
      idx = -1
      return
    end if

    start_idx = this%hash(key)
    found_tombstone = .false.
    first_tombstone = -1
    idx = -1

    ! Use quadratic probing for better distribution
    do i = 0, this%capacity - 1
      probe_idx = modulo(start_idx - 1 + this%probing_distance(i), this%capacity) + 1

      ! Check if slot is empty
      if (.not. this%items(probe_idx)%occupied) then
        if (.not. this%items(probe_idx)%deleted) then
          ! True empty slot
          if (for_insertion) then
            ! For insertion, prefer tombstone if we found one, otherwise use empty slot
            if (found_tombstone) then
              idx = first_tombstone
            else
              idx = probe_idx
            end if
          end if
          ! If searching, this means the key doesn't exist
          return
        else if (for_insertion .and. .not. found_tombstone) then
          ! This is a tombstone - mark it as a candidate for insertion
          first_tombstone = probe_idx
          found_tombstone = .true.
        end if
      else
        ! Check if key matches
        if (allocated(this%items(probe_idx)%key)) then
          if (this%items(probe_idx)%key == key) then
            idx = probe_idx
            return
          end if
        end if
      end if
    end do

    ! If we get here during insertion, it means the table is full (shouldn't happen with resize)
    if (for_insertion .and. found_tombstone) then
      idx = first_tombstone
    end if
  end function find_key_index

  ! Clean up tombstones by rebuilding the dictionary
  subroutine cleanup_tombstones(this)
    class(int_dict), intent(inout) :: this
    type(key_value_pair), allocatable :: old_items(:)
    integer :: old_capacity, i

    if (allocated(this%items)) then
      old_capacity = this%capacity
      allocate(old_items(old_capacity))

      ! Copy items to temporary storage
      do i = 1, old_capacity
        if (this%items(i)%occupied .and. .not. this%items(i)%deleted) then
          old_items(i)%occupied = .true.
          if (allocated(this%items(i)%key)) then
            old_items(i)%key = this%items(i)%key
          end if
          old_items(i)%value = this%items(i)%value
        end if
      end do

      ! Reinitialize with same capacity (tombstones get cleared)
      call this%init(this%capacity)

      ! Reinsert all items
      do i = 1, old_capacity
        if (old_items(i)%occupied .and. allocated(old_items(i)%key)) then
          call this%set(old_items(i)%key, old_items(i)%value)
        end if
      end do

      deallocate(old_items)
    end if
  end subroutine cleanup_tombstones

  ! Resize and rehash the dictionary
  subroutine resize_dict(this, new_capacity)
    class(int_dict), intent(inout) :: this
    integer, intent(in) :: new_capacity
    type(key_value_pair), allocatable :: old_items(:)
    integer :: old_capacity, i

    ! Make sure new_capacity is a power of 2
    integer :: adjusted_capacity
    adjusted_capacity = ceiling_power_of_two(new_capacity)

    if (allocated(this%items)) then
      old_capacity = this%capacity
      allocate(old_items(old_capacity))

      ! Copy items to temporary storage
      do i = 1, old_capacity
        if (this%items(i)%occupied .and. .not. this%items(i)%deleted) then
          old_items(i)%occupied = .true.
          if (allocated(this%items(i)%key)) then
            old_items(i)%key = this%items(i)%key
          end if
          old_items(i)%value = this%items(i)%value
        end if
      end do

      ! Reinitialize with new capacity
      call this%init(adjusted_capacity)

      ! Reinsert all items
      do i = 1, old_capacity
        if (old_items(i)%occupied .and. allocated(old_items(i)%key)) then
          call this%set(old_items(i)%key, old_items(i)%value)
        end if
      end do

      deallocate(old_items)
    else
      call this%init(adjusted_capacity)
    end if
  end subroutine resize_dict

  ! Get a value by key
  subroutine get_value(this, key, value, found)
    class(int_dict), intent(in) :: this
    character(len=*), intent(in) :: key
    integer, intent(out) :: value
    logical, intent(out) :: found
    integer :: idx

    found = .false.
    idx = this%find_index(key, .false.)

    if (idx > 0 .and. this%items(idx)%occupied .and. .not. this%items(idx)%deleted) then
      if (allocated(this%items(idx)%key)) then
        if (this%items(idx)%key == key) then
          value = this%items(idx)%value
          found = .true.
        end if
      end if
    end if
  end subroutine get_value

  ! Set a key-value pair
  subroutine set_value(this, key, value)
    class(int_dict), intent(inout) :: this
    character(len=*), intent(in) :: key
    integer, intent(in) :: value
    integer :: idx

    ! Resize when reaching load factor or too many tombstones
    if (real(this%size + this%tombstones + 1) / real(this%capacity) > this%load_factor) then
      call this%resize(this%capacity * 2)
    else if (real(this%tombstones) / real(this%capacity) > this%tombstone_factor) then
      call this%cleanup_tombstones()
    end if

    idx = this%find_index(key, .true.)

    if (idx > 0) then
      ! If key exists, update the value
      if (this%items(idx)%occupied .and. .not. this%items(idx)%deleted) then
        if (allocated(this%items(idx)%key) .and. this%items(idx)%key == key) then
          this%items(idx)%value = value
          return
        end if
      end if

      ! New key (or reusing a tombstone)
      if (this%items(idx)%deleted) then
        this%tombstones = this%tombstones - 1
      end if

      this%items(idx)%occupied = .true.
      this%items(idx)%deleted = .false.

      ! Allocate or reallocate the key
      if (allocated(this%items(idx)%key)) then
        deallocate(this%items(idx)%key)
      end if
      this%items(idx)%key = key

      ! Set the value
      this%items(idx)%value = value
      this%size = this%size + 1
    end if
  end subroutine set_value

  ! Check if a key exists
  function contains_key(this, key) result(exists)
    class(int_dict), intent(in) :: this
    character(len=*), intent(in) :: key
    logical :: exists
    integer :: idx

    idx = this%find_index(key, .false.)
    exists = .false.

    if (idx > 0) then
      if (this%items(idx)%occupied .and. .not. this%items(idx)%deleted) then
        if (allocated(this%items(idx)%key)) then
          if (this%items(idx)%key == key) then
            exists = .true.
          end if
        end if
      end if
    end if
  end function contains_key

  ! Delete a key (using tombstone approach for open addressing)
  subroutine delete_key(this, key, success)
    class(int_dict), intent(inout) :: this
    character(len=*), intent(in) :: key
    logical, intent(out) :: success
    integer :: idx

    success = .false.
    idx = this%find_index(key, .false.)

    if (idx > 0) then
      if (this%items(idx)%occupied .and. .not. this%items(idx)%deleted) then
        if (allocated(this%items(idx)%key) .and. this%items(idx)%key == key) then
          ! Mark as deleted (tombstone) instead of deallocating
          ! This preserves the probing chain
          this%items(idx)%deleted = .true.
          this%size = this%size - 1
          this%tombstones = this%tombstones + 1
          success = .true.

          ! Clean up if too many tombstones
          if (real(this%tombstones) / real(this%capacity) > this%tombstone_factor) then
            call this%cleanup_tombstones()
          end if
        end if
      end if
    end if
  end subroutine delete_key

  ! Get all keys
  function get_all_keys(this) result(keys)
    class(int_dict), intent(in) :: this
    character(len=:), allocatable :: keys(:)
    integer :: i, count, max_len

    ! Determine maximum key length
    max_len = 1
    do i = 1, this%capacity
      if (this%items(i)%occupied .and. .not. this%items(i)%deleted) then
        if (allocated(this%items(i)%key)) then
          max_len = max(max_len, len(this%items(i)%key))
        end if
      end if
    end do

    allocate(character(len=max_len) :: keys(this%size))
    count = 0

    do i = 1, this%capacity
      if (this%items(i)%occupied .and. .not. this%items(i)%deleted) then
        if (allocated(this%items(i)%key)) then
          count = count + 1
          keys(count) = this%items(i)%key
        end if
      end if
    end do
  end function get_all_keys

  ! Get all values
  function get_all_values(this) result(values)
    class(int_dict), intent(in) :: this
    integer, allocatable :: values(:)
    integer :: i, count

    allocate(values(this%size))
    count = 0

    do i = 1, this%capacity
      if (this%items(i)%occupied .and. .not. this%items(i)%deleted) then
        if (allocated(this%items(i)%key)) then
          count = count + 1
          values(count) = this%items(i)%value
        end if
      end if
    end do
  end function get_all_values

  ! Get all key-value pairs
  subroutine get_all_items(this, keys, values, count)
    class(int_dict), intent(in) :: this
    character(len=:), allocatable, intent(out) :: keys(:)
    integer, allocatable, intent(out) :: values(:)
    integer, intent(out) :: count
    integer :: i, max_len

    ! Determine maximum key length
    max_len = 1
    do i = 1, this%capacity
      if (this%items(i)%occupied .and. .not. this%items(i)%deleted) then
        if (allocated(this%items(i)%key)) then
          max_len = max(max_len, len(this%items(i)%key))
        end if
      end if
    end do

    allocate(character(len=max_len) :: keys(this%size))
    allocate(values(this%size))
    count = 0

    do i = 1, this%capacity
      if (this%items(i)%occupied .and. .not. this%items(i)%deleted) then
        if (allocated(this%items(i)%key)) then
          count = count + 1
          keys(count) = this%items(i)%key
          values(count) = this%items(i)%value
        end if
      end if
    end do
  end subroutine get_all_items

  ! Clear the dictionary
  subroutine clear_dict(this)
    class(int_dict), intent(inout) :: this
    integer :: i

    if (allocated(this%items)) then
      do i = 1, this%capacity
        if (this%items(i)%occupied) then
          if (allocated(this%items(i)%key)) deallocate(this%items(i)%key)
          this%items(i)%occupied = .false.
          this%items(i)%deleted = .false.
        end if
      end do
    end if

    this%size = 0
    this%tombstones = 0
  end subroutine clear_dict

  ! Get the number of key-value pairs
  function dict_length(this) result(len)
    class(int_dict), intent(in) :: this
    integer :: len

    len = this%size
  end function dict_length

  ! Print the dictionary (for debugging)
  subroutine print_dict(this)
    class(int_dict), intent(in) :: this
    integer :: i

    print *, "Dictionary (size=", this%size, ", capacity=", this%capacity, &
    ", tombstones=", this%tombstones, "):"
    do i = 1, this%capacity
      if (this%items(i)%occupied .and. .not. this%items(i)%deleted) then
        if (allocated(this%items(i)%key)) then
          print *, "  Key: ", this%items(i)%key, ", Value: ", this%items(i)%value
        end if
      end if
    end do
  end subroutine print_dict

end module fortran_int_dict

! Example program demonstrating the optimized integer dictionary
program int_dict_demo
use fortran_int_dict
implicit none

type(int_dict) :: my_dict
logical :: found, success
integer :: value, i
character(len=:), allocatable :: keys(:)
integer, allocatable :: values(:)
integer :: count

! For performance testing
integer :: num_items = 1000
character(len=20) :: key_buffer
real :: start_time, end_time

! Create a dictionary with power-of-2 size for faster modulo operations
my_dict = create_int_dict(16)

! Set some values
call my_dict%set("one", 1)
call my_dict%set("two", 2)
call my_dict%set("three", 3)

! Get and print values
call my_dict%get("one", value, found)
if (found) then
  print *, "Value for 'one':", value
end if

call my_dict%get("two", value, found)
if (found) then
  print *, "Value for 'two':", value
end if

! Test has_key functionality
if (my_dict%has_key("three")) then
  print *, "Dictionary contains key 'three'"
end if

! Test delete functionality
call my_dict%delete("two", success)
if (success) then
  print *, "Deleted key 'two'"
end if

! Check if deleted key exists
if (.not. my_dict%has_key("two")) then
  print *, "Key 'two' no longer exists"
end if

! Get all keys and values
call my_dict%get_all_items(keys, values, count)
print *, "All items in dictionary:"
do i = 1, count
print *, "  ", keys(i), " => ", values(i)
end if

! Performance test
print *, ""
print *, "Performance test with", num_items, "items:"

! Start timer
call cpu_time(start_time)

! Insert items
do i = 1, num_items
  write(key_buffer, '(A,I0)') "key", i
  call my_dict%set(trim(key_buffer), i * 10)
end do

call cpu_time(end_time)
print *, "  Insert time:", end_time - start_time, "seconds"

! Lookup test
call cpu_time(start_time)
do i = 1, num_items
  write(key_buffer, '(A,I0)') "key", i
  call my_dict%get(trim(key_buffer), value, found)
  if (.not. found) then
    print *, "Error: key not found:", trim(key_buffer)
  end if
end do

call cpu_time(end_time)
print *, "  Lookup time:", end_time - start_time, "seconds"

! Random access test for keys that exist and don't exist
call cpu_time(start_time)
do i = 1, num_items
  ! Every other lookup will be for a non-existent key
  if (mod(i, 2) == 0) then
    write(key_buffer, '(A,I0)') "key", i
  else
    write(key_buffer, '(A,I0)') "nonexistent", i
  end if

  if (my_dict%has_key(trim(key_buffer))) then
    call my_dict%get(trim(key_buffer), value, found)
  end if
end do

call cpu_time(end_time)
print *, "  Mixed lookup time:", end_time - start_time, "seconds"

! Clear the dictionary
call my_dict%clear()
print *, "Dictionary size after clear:", my_dict%length()

end program int_dict_demo
