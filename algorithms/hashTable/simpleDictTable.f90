module DictionaryModule
  implicit none
  private
  public :: Dictionary, init_dict, insert, get, contains, remove

  type :: Dictionary
    integer, allocatable :: keys(:)   ! Array to store keys
    integer, allocatable :: values(:) ! Array to store values
    logical, allocatable :: occupied(:) ! Array to track occupied slots
    logical, allocatable :: deleted(:)  ! Array to track deleted slots
    integer :: size                   ! Current size of the dictionary
    integer :: capacity               ! Maximum capacity of the dictionary
  end type Dictionary

  contains

  subroutine init_dict(dict, capacity)
    type(Dictionary), intent(inout) :: dict
    integer, intent(in) :: capacity

    allocate(dict%keys(capacity))
    allocate(dict%values(capacity))
    allocate(dict%occupied(capacity))
    allocate(dict%deleted(capacity))
    dict%keys = 0
    dict%values = 0
    dict%occupied = .false.
    dict%deleted = .false.
    dict%size = 0
    dict%capacity = capacity
  end subroutine init_dict

  subroutine insert(dict, key, value)
    type(Dictionary), intent(inout) :: dict
    integer, intent(in) :: key, value
    integer :: index, original_index, i

    if (dict%size >= dict%capacity * 0.75) then
      call resize_dict(dict)
    end if

    index = hash_function(key, dict%capacity)
    original_index = index

    do
      if (.not. dict%occupied(index) .or. dict%deleted(index) .or. dict%keys(index) == key) then
        dict%keys(index) = key
        dict%values(index) = value
        if (.not. dict%occupied(index)) then
          dict%occupied(index) = .true.
          dict%deleted(index) = .false.
          dict%size = dict%size + 1
        end if
        return
      end if
      index = mod(index + 1, dict%capacity)
      if (index == original_index) exit ! Prevent infinite loop
    end do
  end subroutine insert

  function get(dict, key) result(value)
    type(Dictionary), intent(in) :: dict
    integer, intent(in) :: key
    integer :: value
    integer :: index, original_index

    index = hash_function(key, dict%capacity)
    original_index = index

    do
      if (dict%occupied(index) .and. dict%keys(index) == key) then
        value = dict%values(index)
        return
      end if
      if (.not. dict%occupied(index) .and. .not. dict%deleted(index)) exit
      index = mod(index + 1, dict%capacity)
      if (index == original_index) exit ! Prevent infinite loop
    end do

    value = -1 ! Return -1 if key is not found
  end function get

  function contains(dict, key) result(found)
    type(Dictionary), intent(in) :: dict
    integer, intent(in) :: key
    logical :: found
    integer :: index, original_index

    index = hash_function(key, dict%capacity)
    original_index = index

    do
      if (dict%occupied(index) .and. dict%keys(index) == key) then
        found = .true.
        return
      end if
      if (.not. dict%occupied(index) .and. .not. dict%deleted(index)) exit
      index = mod(index + 1, dict%capacity)
      if (index == original_index) exit ! Prevent infinite loop
    end do

    found = .false.
  end function contains

  subroutine remove(dict, key)
    type(Dictionary), intent(inout) :: dict
    integer, intent(in) :: key
    integer :: index, original_index

    index = hash_function(key, dict%capacity)
    original_index = index

    do
      if (dict%occupied(index) .and. dict%keys(index) == key) then
        dict%occupied(index) = .false.
        dict%deleted(index) = .true.
        dict%size = dict%size - 1
        return
      end if
      if (.not. dict%occupied(index) .and. .not. dict%deleted(index)) exit
      index = mod(index + 1, dict%capacity)
      if (index == original_index) exit ! Prevent infinite loop
    end do
  end subroutine remove

  subroutine resize_dict(dict)
    type(Dictionary), intent(inout) :: dict
    integer :: new_capacity, i
    type(Dictionary) :: new_dict

    new_capacity = dict%capacity * 2
    call init_dict(new_dict, new_capacity)

    do i = 1, dict%capacity
      if (dict%occupied(i) .and. .not. dict%deleted(i)) then
        call insert(new_dict, dict%keys(i), dict%values(i))
      end if
    end do

    call move_alloc(new_dict%keys, dict%keys)
    call move_alloc(new_dict%values, dict%values)
    call move_alloc(new_dict%occupied, dict%occupied)
    call move_alloc(new_dict%deleted, dict%deleted)
    dict%capacity = new_capacity
  end subroutine resize_dict

  function hash_function(key, capacity) result(index)
    integer, intent(in) :: key, capacity
    integer :: index
    index = mod(abs(key), capacity)
    if (index < 0) index = index + capacity
  end function hash_function

end module DictionaryModule
