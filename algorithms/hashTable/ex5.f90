! Simplified Fortran 2008 dictionary module for integer keys
module dict_mod
  implicit none
  private
  public :: DictIntInt, new_DictIntInt, dict_put_int, dict_get_int, dict_remove_int, dict_contains_int

  integer, parameter :: EMPTY=0, OCCUPIED=1, DELETED=2
  real,    parameter :: MAX_LOAD=0.7

  ! Dictionary type with integer keys and integer values
  type :: DictIntInt
    integer :: capacity = 0
    integer :: size     = 0
    integer, allocatable :: keys(:)
    integer, allocatable :: vals(:)
    integer, allocatable :: state(:)
  end type DictIntInt

contains

  ! Constructor: create dictionary with initial capacity
  function new_DictIntInt(init_cap) result(d)
    integer, intent(in), optional :: init_cap
    type(DictIntInt) :: d
    integer :: c
    c = merge(init_cap, 16, present(init_cap))
    d%capacity = max(1, c)
    d%size = 0
    allocate(d%keys(d%capacity), d%vals(d%capacity), d%state(d%capacity))
    d%state = EMPTY
  end function new_DictIntInt

  ! Hash function
  pure function hash(key, cap) result(h)
    integer, intent(in) :: key, cap
    integer :: h
    h = mod(key, cap) + 1
  end function hash

  ! Insert or update key->value
  subroutine dict_put_int(d, key, value)
    type(DictIntInt), intent(inout) :: d
    integer, intent(in) :: key, value
    integer :: idx, start
    integer :: oldc, i, k, v
    integer, allocatable :: ok(:), os(:), ov(:)
    ! grow if necessary
    if (real(d%size + 1) / d%capacity > MAX_LOAD) then
      oldc = d%capacity
      ok = d%keys; ov = d%vals; os = d%state
      d%capacity = oldc * 2; d%size = 0
      deallocate(d%keys, d%vals, d%state)
      allocate(d%keys(d%capacity), d%vals(d%capacity), d%state(d%capacity))
      d%state = EMPTY
      do i = 1, oldc
        if (os(i) == OCCUPIED) then
          call dict_put_int(d, ok(i), ov(i))
        end if
      end do
    end if
    start = hash(key, d%capacity); idx = start
    do
      if (d%state(idx) /= OCCUPIED) then
        d%keys(idx) = key; d%vals(idx) = value; d%state(idx) = OCCUPIED; d%size = d%size + 1; return
      elseif (d%keys(idx) == key) then
        d%vals(idx) = value; return
      end if
      idx = idx + 1; if (idx > d%capacity) idx = 1
      if (idx == start) exit
    end do
  end subroutine dict_put_int

  ! Retrieve value; found=.true. if present
  subroutine dict_get_int(d, key, value, found)
    type(DictIntInt), intent(in) :: d
    integer, intent(in) :: key
    integer, intent(out) :: value
    logical, intent(out) :: found
    integer :: idx, start
    if (d%capacity == 0) then found = .false.; return; end if
    start = hash(key, d%capacity); idx = start
    do
      if (d%state(idx) == EMPTY) then found = .false.; return; end if
      if (d%state(idx) == OCCUPIED .and. d%keys(idx) == key) then
        value = d%vals(idx); found = .true.; return
      end if
      idx = idx + 1; if (idx > d%capacity) idx = 1
      if (idx == start) then found = .false.; return; end if
    end do
  end subroutine dict_get_int

  ! Remove a key
  subroutine dict_remove_int(d, key)
    type(DictIntInt), intent(inout) :: d
    integer, intent(in) :: key
    integer :: idx, start
    if (d%capacity == 0) return
    start = hash(key, d%capacity); idx = start
    do
      if (d%state(idx) == OCCUPIED .and. d%keys(idx) == key) then
        d%state(idx) = DELETED; d%size = d%size - 1; return
      end if
      if (d%state(idx) == EMPTY) return
      idx = idx + 1; if (idx > d%capacity) idx = 1
      if (idx == start) return
    end do
  end subroutine dict_remove_int

  ! Check membership
  function dict_contains_int(d, key) result(res)
    type(DictIntInt), intent(in) :: d
    integer, intent(in) :: key
    logical :: res
    integer :: tmp; logical :: f
    call dict_get_int(d, key, tmp, f)
    res = f
  end function dict_contains_int

end module dict_mod

! Example usage
program test_dict
  use dict_mod
  implicit none
  type(DictIntInt) :: di
  integer :: v; logical :: found

  di = new_DictIntInt(8)
  call dict_put_int(di, 10, 42)
  call dict_get_int(di, 10, v, found)
  if (found) print *, '10->', v

  call dict_put_int(di, 10, 84)
  call dict_get_int(di, 10, v, found)
  if (found) print *, '10->', v

  print *, 'Contains 5?', dict_contains_int(di, 5)
  call dict_remove_int(di, 10)
  print *, 'Contains 10 after remove?', dict_contains_int(di, 10)
end program test_dict

