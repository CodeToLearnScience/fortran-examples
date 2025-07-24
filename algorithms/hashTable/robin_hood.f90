! Module: Integer Hash Table with Optimized Robin Hood Open Addressing
! Uses 64‑bit arithmetic for hashing to avoid overflow of the multiplicative constant.

module IntegerHashTable
  use iso_fortran_env, only: int64
  implicit none
  private
  public :: HashTable, ht_init, ht_insert, ht_find, ht_remove, ht_free

  integer(int64), parameter :: EMPTY64   = -huge(0_int64)       ! sentinel for empty slot (64‑bit)
  integer(int64), parameter :: DELETED64 = EMPTY64 - 1_int64     ! sentinel for deleted slot
  integer(int64), parameter :: MULT64    = 2654435761_int64      ! Knuth multiplicative constant

  type :: HashTable
    integer(int64), allocatable :: keys(:)
    integer :: capacity = 0       ! always power-of-two
    integer :: mask = 0           ! capacity - 1 for bitmask
    integer :: size = 0
  end type HashTable

  contains

  ! Initialize hash table with given initial capacity (rounded up to power-of-two)
  subroutine ht_init(ht, init_cap)
    type(HashTable), intent(out) :: ht
    integer, intent(in) :: init_cap
    integer :: p

    p = 1
    do while (p < init_cap)
      p = p * 2
    end do
    ht%capacity = p
    ht%mask = p - 1
    allocate(ht%keys(0:ht%mask))
    ht%keys = EMPTY64
    ht%size = 0
  end subroutine ht_init

  ! Fast hash: 64‑bit multiplicative hashing + bitmask
  pure function hash(key, mask) result(h)
    integer(int64), intent(in) :: key
    integer, intent(in) :: mask
    integer :: h
    integer(int64) :: prod

    prod = key * MULT64
    h = int( iand(prod, int(mask, int64)), kind=h )
  end function hash

  ! Insert key into table using Robin Hood policy
  subroutine ht_insert(ht, key)
    type(HashTable), intent(inout) :: ht
    integer, intent(in) :: key
    integer :: idx, dist
    integer(int64) :: k64, cur64, prod
    integer :: cur_hash, cur_dist

    if (ht%size * 2 > ht%capacity) then
      call resize(ht, ht%capacity * 2)
    end if

    k64 = int(key, int64)
    idx = hash(k64, ht%mask)
    dist = 0

    do
      cur64 = ht%keys(idx)
      if (cur64 == EMPTY64 .or. cur64 == DELETED64) then
        ht%keys(idx) = k64
        ht%size = ht%size + 1
        return
      end if

      prod = cur64 * MULT64
      cur_hash = int( iand(prod, int(ht%mask, int64)), kind=cur_hash )
      cur_dist = iand(idx - cur_hash, ht%mask)
      if (cur_dist < dist) then
        ht%keys(idx) = k64
        k64 = cur64
        dist = cur_dist
      end if
      idx = iand(idx + 1, ht%mask)
      dist = dist + 1
    end do
  end subroutine ht_insert

  ! Find key: returns .true. if found; uses early exit via Robin Hood invariant
  pure function ht_find(ht, key) result(found)
    type(HashTable), intent(in) :: ht
    integer, intent(in) :: key
    logical :: found
    integer :: idx, dist
    integer(int64) :: k64, cur64, prod
    integer :: cur_hash, cur_dist

    k64 = int(key, int64)
    idx = hash(k64, ht%mask)
    dist = 0

    do
      cur64 = ht%keys(idx)
      if (cur64 == EMPTY64) then
        found = .false.
        return
      else if (cur64 == k64) then
        found = .true.
        return
      end if
      prod = cur64 * MULT64
      cur_hash = int( iand(prod, int(ht%mask, int64)), kind=cur_hash )
      cur_dist = iand(idx - cur_hash, ht%mask)
      if (cur_dist < dist) then
        found = .false.
        return
      end if
      idx = iand(idx + 1, ht%mask)
      dist = dist + 1
      if (dist > ht%capacity) exit
    end do
    found = .false.
  end function ht_find

  ! Remove key: marks slot as DELETED
  subroutine ht_remove(ht, key)
    type(HashTable), intent(inout) :: ht
    integer, intent(in) :: key
    integer :: idx, dist
    integer(int64) :: k64, cur64, prod
    integer :: cur_hash, cur_dist

    k64 = int(key, int64)
    idx = hash(k64, ht%mask)
    dist = 0

    do
      cur64 = ht%keys(idx)
      if (cur64 == EMPTY64) return
      if (cur64 == k64) then
        ht%keys(idx) = DELETED64
        ht%size = ht%size - 1
        return
      end if
      prod = cur64 * MULT64
      cur_hash = int( iand(prod, int(ht%mask, int64)), kind=cur_hash )
      cur_dist = iand(idx - cur_hash, ht%mask)
      if (cur_dist < dist) return
      idx = iand(idx + 1, ht%mask)
      dist = dist + 1
      if (dist > ht%capacity) exit
    end do
  end subroutine ht_remove

  ! Free resources
  subroutine ht_free(ht)
    type(HashTable), intent(inout) :: ht
    if (allocated(ht%keys)) deallocate(ht%keys)
    ht%capacity = 0
    ht%mask = 0
    ht%size = 0
  end subroutine ht_free

  ! Resize table to new power-of-two capacity
  subroutine resize(ht, new_cap)
    type(HashTable), intent(inout) :: ht
    integer, intent(in) :: new_cap
    type(HashTable) :: tmp
    integer :: i

    call ht_init(tmp, new_cap)
    do i = 0, ht%capacity-1
      if (ht%keys(i) /= EMPTY64 .and. ht%keys(i) /= DELETED64) then
        call ht_insert(tmp, int(ht%keys(i), kind=kind(tmp%keys)))
      end if
    end do
    call ht_free(ht)
    ht = tmp
  end subroutine resize

end module IntegerHashTable

