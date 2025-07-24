! fortran_dict.f90
! A sophisticated Python-like dictionary implementation in Fortran

module fortran_dict
  implicit none
  private

  ! Define a generic key-value pair type that can hold different data types
  type :: key_value_pair
    character(len=:), allocatable :: key
    class(*), allocatable :: value
  end type key_value_pair

  ! Define a dictionary type
  type, public :: dict
    private
    type(key_value_pair), allocatable :: items(:)
    integer :: size = 0
    integer :: capacity = 0
    real :: load_factor = 0.75
    contains
    procedure :: init => initialize_dict
    procedure :: get => get_value
    procedure :: set => set_value
    procedure :: has_key => contains_key
    procedure :: delete => delete_key
    procedure :: get_keys => get_all_keys
    procedure :: clear => clear_dict
    procedure :: length => dict_length
    procedure :: print => print_dict
    procedure, private :: resize => resize_dict
    procedure, private :: hash => hash_function
    procedure, private :: find_index => find_key_index
  end type dict

  ! Public constructor
  public :: create_dict

  contains

  ! Constructor function
  function create_dict(initial_capacity) result(new_dict)
    integer, intent(in), optional :: initial_capacity
    type(dict) :: new_dict
    integer :: init_cap

    if (present(initial_capacity)) then
      init_cap = initial_capacity
    else
      init_cap = 8  ! Default initial capacity
    end if

    call new_dict%init(init_cap)
  end function create_dict

  ! Initialize the dictionary
  subroutine initialize_dict(this, capacity)
    class(dict), intent(inout) :: this
    integer, intent(in) :: capacity

    if (allocated(this%items)) deallocate(this%items)
    allocate(this%items(capacity))
    this%size = 0
    this%capacity = capacity
  end subroutine initialize_dict

  ! Hash function using FNV-1a hash algorithm
  function hash_function(this, key) result(hash_val)
    class(dict), intent(in) :: this
    character(len=*), intent(in) :: key
    integer :: hash_val
    integer :: i
    integer, parameter :: FNV_PRIME = 16777619
    integer, parameter :: FNV_OFFSET_BASIS = 2166136261

    hash_val = FNV_OFFSET_BASIS
    do i = 1, len(key)
      hash_val = ieor(hash_val, ichar(key(i:i)))
      hash_val = modulo(hash_val * FNV_PRIME, huge(hash_val))
    end do

    ! Map to the array size
    hash_val = modulo(hash_val, this%capacity) + 1
  end function hash_function

  ! Find the index of a key using linear probing
  function find_key_index(this, key, for_insertion) result(idx)
    class(dict), intent(in) :: this
    character(len=*), intent(in) :: key
    logical, intent(in) :: for_insertion
    integer :: idx
    integer :: i, start_idx
    logical :: found_empty

    if (this%capacity == 0) then
      idx = -1
      return
    end if

    start_idx = this%hash(key)
    found_empty = .false.
    idx = -1

    do i = 0, this%capacity - 1
      integer :: probe_idx
      probe_idx = modulo(start_idx + i - 1, this%capacity) + 1

      if (.not. allocated(this%items(probe_idx)%key)) then
        if (for_insertion .and. .not. found_empty) then
          idx = probe_idx
          found_empty = .true.
        end if
        cycle
      end if

      if (this%items(probe_idx)%key == key) then
        idx = probe_idx
        return
      end if
    end do
  end function find_key_index

  ! Resize and rehash the dictionary
  subroutine resize_dict(this, new_capacity)
    class(dict), intent(inout) :: this
    integer, intent(in) :: new_capacity
    type(key_value_pair), allocatable :: old_items(:)
    integer :: old_capacity, i

    if (allocated(this%items)) then
      old_capacity = this%capacity
      allocate(old_items(old_capacity))

      ! Copy items to temporary storage
      do i = 1, old_capacity
        if (allocated(this%items(i)%key)) then
          old_items(i)%key = this%items(i)%key
          allocate(old_items(i)%value, source=this%items(i)%value)
        end if
      end do

      ! Reinitialize with new capacity
      call this%init(new_capacity)

      ! Reinsert all items
      do i = 1, old_capacity
        if (allocated(old_items(i)%key)) then
          call this%set(old_items(i)%key, old_items(i)%value)
        end if
      end do

      ! Clean up
      deallocate(old_items)
    else
      call this%init(new_capacity)
    end if
  end subroutine resize_dict

  ! Get a value by key
  subroutine get_value(this, key, value, found)
    class(dict), intent(in) :: this
    character(len=*), intent(in) :: key
    class(*), allocatable, intent(out) :: value
    logical, intent(out) :: found
    integer :: idx

    found = .false.
    idx = this%find_index(key, .false.)

    if (idx > 0 .and. allocated(this%items(idx)%key)) then
      if (this%items(idx)%key == key) then
        allocate(value, source=this%items(idx)%value)
        found = .true.
      end if
    end if
  end subroutine get_value

  ! Set a key-value pair
  subroutine set_value(this, key, value)
    class(dict), intent(inout) :: this
    character(len=*), intent(in) :: key
    class(*), intent(in) :: value
    integer :: idx

    ! Check if we need to resize
    if (real(this%size + 1) / real(this%capacity) > this%load_factor) then
      call this%resize(this%capacity * 2)
    end if

    idx = this%find_index(key, .true.)

    if (idx > 0) then
      ! If key exists, update the value
      if (allocated(this%items(idx)%key) .and. this%items(idx)%key == key) then
        if (allocated(this%items(idx)%value)) deallocate(this%items(idx)%value)
      else
        ! New key
        if (.not. allocated(this%items(idx)%key)) then
          this%size = this%size + 1
        end if
        this%items(idx)%key = key
      end if

      ! Set the value
      allocate(this%items(idx)%value, source=value)
    end if
  end subroutine set_value

  ! Check if a key exists
  function contains_key(this, key) result(exists)
    class(dict), intent(in) :: this
    character(len=*), intent(in) :: key
    logical :: exists
    integer :: idx

    idx = this%find_index(key, .false.)
    exists = (idx > 0 .and. allocated(this%items(idx)%key) .and. this%items(idx)%key == key)
  end function contains_key

  ! Delete a key
  subroutine delete_key(this, key, success)
    class(dict), intent(inout) :: this
    character(len=*), intent(in) :: key
    logical, intent(out) :: success
    integer :: idx

    success = .false.
    idx = this%find_index(key, .false.)

    if (idx > 0 .and. allocated(this%items(idx)%key)) then
      if (this%items(idx)%key == key) then
        deallocate(this%items(idx)%key)
        if (allocated(this%items(idx)%value)) deallocate(this%items(idx)%value)
        this%size = this%size - 1
        success = .true.
      end if
    end if
  end subroutine delete_key

  ! Get all keys
  function get_all_keys(this) result(keys)
    class(dict), intent(in) :: this
    character(len=:), allocatable :: keys(:)
    integer :: i, count

    allocate(character(len=50) :: keys(this%size))
    count = 0

    do i = 1, this%capacity
      if (allocated(this%items(i)%key)) then
        count = count + 1
        keys(count) = this%items(i)%key
      end if
    end do
  end function get_all_keys

  ! Clear the dictionary
  subroutine clear_dict(this)
    class(dict), intent(inout) :: this
    integer :: i

    if (allocated(this%items)) then
      do i = 1, this%capacity
        if (allocated(this%items(i)%key)) then
          deallocate(this%items(i)%key)
          if (allocated(this%items(i)%value)) deallocate(this%items(i)%value)
        end if
      end do
    end if

    this%size = 0
  end subroutine clear_dict

  ! Get the number of key-value pairs
  function dict_length(this) result(len)
    class(dict), intent(in) :: this
    integer :: len

    len = this%size
  end function dict_length

  ! Print the dictionary (for debugging)
  subroutine print_dict(this)
    class(dict), intent(in) :: this
    integer :: i

    print *, "Dictionary (size=", this%size, ", capacity=", this%capacity, "):"
    do i = 1, this%capacity
      if (allocated(this%items(i)%key)) then
        print *, "  Key: ", this%items(i)%key
      end if
    end do
  end subroutine print_dict

end module fortran_dict

! Example program demonstrating the dictionary
program dict_demo
  use fortran_dict
  implicit none

  type(dict) :: my_dict
  logical :: found, success
  integer :: int_val
  real :: real_val
  character(len=:), allocatable :: str_val
  class(*), allocatable :: val

  ! Create a dictionary
  my_dict = create_dict(16)

  ! Set some values of different types
  call my_dict%set("int_key", 42)
  call my_dict%set("real_key", 3.14159)
  call my_dict%set("string_key", "Hello, Fortran Dictionary!")

  ! Get and print values
  call my_dict%get("int_key", val, found)
  if (found) then
    select type(val)
    type is (integer)
    int_val = val
    print *, "int_key value:", int_val
    end select
  end if

  call my_dict%get("real_key", val, found)
  if (found) then
    select type(val)
    type is (real)
    real_val = val
    print *, "real_key value:", real_val
    end select
  end if

  call my_dict%get("string_key", val, found)
  if (found) then
    select type(val)
    type is (character(len=*))
    str_val = val
    print *, "string_key value:", str_val
    end select
  end if

  ! Test has_key functionality
  if (my_dict%has_key("int_key")) then
    print *, "Dictionary contains int_key"
  end if

  ! Test delete functionality
  call my_dict%delete("real_key", success)
  if (success) then
    print *, "Deleted real_key"
  end if

  ! Check dictionary size
  print *, "Dictionary size:", my_dict%length()

  ! Print all keys
  print *, "All keys in dictionary:", my_dict%get_keys()

  ! Clear the dictionary
  call my_dict%clear()
  print *, "Dictionary size after clear:", my_dict%length()

end program dict_demo
