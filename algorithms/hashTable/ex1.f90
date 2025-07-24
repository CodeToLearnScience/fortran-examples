module dict_mod
  ! Fortran module implementing Python-like dictionary
  implicit none
  private
  public :: DictIntInt, DictIntLog, &
  new_DictIntInt, new_DictIntLog

  integer, parameter :: EMPTY = 0, OCCUPIED = 1, DELETED = 2
  real,    parameter :: MAX_LOAD = 0.7

  ! Integer -> Integer dictionary
  type :: DictIntInt
    private
    integer :: capacity = 0
    integer :: size = 0
    integer, allocatable :: keys(:)
    integer, allocatable :: values(:)
    integer, allocatable :: state(:)
    contains
    procedure :: put => put_int_int
    procedure :: get => get_int_int
    procedure :: remove => remove_int_int
    procedure :: contains_key => contains_key_int
    procedure :: clear => clear_int_int
    procedure :: keys_list => keys_int_int
    procedure :: values_list => values_int_int
    procedure :: rehash => rehash_int_int
  end type DictIntInt

  ! Integer -> Logical dictionary
  type :: DictIntLog
    private
    integer :: capacity = 0
    integer :: size = 0
    integer, allocatable :: keys(:)
    logical, allocatable :: values(:)
    integer, allocatable :: state(:)
    contains
    procedure :: put => put_int_log
    procedure :: get => get_int_log
    procedure :: remove => remove_int_log
    procedure :: contains_key => contains_key_int
    procedure :: clear => clear_int_log
    procedure :: keys_list => keys_int_log
    procedure :: values_list => values_int_log
    procedure :: rehash => rehash_int_log
  end type DictIntLog

  contains

  ! Create new DictIntInt with given initial capacity
  function new_DictIntInt(init_cap) result(dict)
    integer, intent(in), optional :: init_cap
    type(DictIntInt) :: dict
    integer :: c
    c = max(1, merge(init_cap, 16, present(init_cap)))
    dict%capacity = c
    allocate(dict%keys(c))
    allocate(dict%values(c))
    allocate(dict%state(c))
    dict%state = EMPTY
    dict%size = 0
  end function new_DictIntInt

  ! Create new DictIntLog with given initial capacity
  function new_DictIntLog(init_cap) result(dict)
    integer, intent(in), optional :: init_cap
    type(DictIntLog) :: dict
    integer :: c
    c = max(1, merge(init_cap, 16, present(init_cap)))
    dict%capacity = c
    allocate(dict%keys(c))
    allocate(dict%values(c))
    allocate(dict%state(c))
    dict%state = EMPTY
    dict%size = 0
  end function new_DictIntLog

  ! Internal: hash function
  pure function hash_int(key, cap) result(h)
    integer, intent(in) :: key, cap
    integer :: h
    h = mod(key, cap) + 1
  end function hash_int

  ! Rehash when load factor exceeded (integer-int)
  subroutine rehash_int_int(this)
    class(DictIntInt), intent(inout) :: this
    integer :: old_cap, i, k, idx
    integer, allocatable :: old_keys(:), old_vals(:), old_state(:)
    old_cap = this%capacity
    old_keys = this%keys
    old_vals = this%values
    old_state = this%state

    call clear_int_int(this, newsize=old_cap*2)

    do i = 1, old_cap
      if (old_state(i) == OCCUPIED) then
        call this%put(old_keys(i), old_vals(i))
      end if
    end do
  end subroutine rehash_int_int

  ! Rehash when load factor exceeded (integer-logical)
  subroutine rehash_int_log(this)
    class(DictIntLog), intent(inout) :: this
    integer :: old_cap, i, k, idx
    integer, allocatable :: old_keys(:), old_state(:)
    logical, allocatable :: old_vals(:)
    old_cap = this%capacity
    old_keys = this%keys
    old_vals = this%values
    old_state = this%state

    call clear_int_log(this, newsize=old_cap*2)

    do i = 1, old_cap
      if (old_state(i) == OCCUPIED) then
        call this%put(old_keys(i), old_vals(i))
      end if
    end do
  end subroutine rehash_int_log

  ! Put key,value (integer-int)
  subroutine put_int_int(this, key, val)
    class(DictIntInt), intent(inout) :: this
    integer, intent(in) :: key, val
    integer :: idx, start

    if (real(this%size+1)/this%capacity > MAX_LOAD) call this%rehash()
    start = hash_int(key, this%capacity)
    idx = start
    do
      select case (this%state(idx))
      case (EMPTY, DELETED)
      this%keys(idx) = key
      this%values(idx) = val
      this%state(idx) = OCCUPIED
      this%size = this%size + 1
      return
      case (OCCUPIED)
      if (this%keys(idx) == key) then
        this%values(idx) = val
        return
      end if
      end select
      idx = idx + 1
      if (idx > this%capacity) idx = 1
      if (idx == start) exit
    end do
  end subroutine put_int_int

  ! Put key,value (integer-logical)
  subroutine put_int_log(this, key, val)
    class(DictIntLog), intent(inout) :: this
    integer, intent(in) :: key
    logical, intent(in) :: val
    integer :: idx, start

    if (real(this%size+1)/this%capacity > MAX_LOAD) call this%rehash()
    start = hash_int(key, this%capacity)
    idx = start
    do
      select case (this%state(idx))
      case (EMPTY, DELETED)
      this%keys(idx) = key
      this%values(idx) = val
      this%state(idx) = OCCUPIED
      this%size = this%size + 1
      return
      case (OCCUPIED)
      if (this%keys(idx) == key) then
        this%values(idx) = val
        return
      end if
      end select
      idx = idx + 1
      if (idx > this%capacity) idx = 1
      if (idx == start) exit
    end do
  end subroutine put_int_log

  ! Get value (integer-int)
  subroutine get_int_int(this, key, val, found)
    class(DictIntInt), intent(in) :: this
    integer, intent(in) :: key
    integer, intent(out) :: val
    logical, intent(out) :: found
    integer :: idx, start

    start = hash_int(key, this%capacity)
    idx = start
    do
      select case (this%state(idx))
      case (EMPTY)
      found = .false.
      return
      case (OCCUPIED)
      if (this%keys(idx) == key) then
        val = this%values(idx)
        found = .true.
        return
      end if
      case (DELETED)
      ! skip
      end select
      idx = idx + 1
      if (idx > this%capacity) idx = 1
      if (idx == start) then
        found = .false.
        return
      end if
    end do
  end subroutine get_int_int

  ! Get value (integer-logical)
  subroutine get_int_log(this, key, val, found)
    class(DictIntLog), intent(in) :: this
    integer, intent(in) :: key
    logical, intent(out) :: val
    logical, intent(out) :: found
    integer :: idx, start

    start = hash_int(key, this%capacity)
    idx = start
    do
      select case (this%state(idx))
      case (EMPTY)
      found = .false.
      return
      case (OCCUPIED)
      if (this%keys(idx) == key) then
        val = this%values(idx)
        found = .true.
        return
      end if
      case (DELETED)
      end select
      idx = idx + 1
      if (idx > this%capacity) idx = 1
      if (idx == start) then
        found = .false.
        return
      end if
    end do
  end subroutine get_int_log

  ! Remove key
  subroutine remove_int_int(this, key)
    class(DictIntInt), intent(inout) :: this
    integer, intent(in) :: key
    integer :: idx, start

    start = hash_int(key, this%capacity)
    idx = start
    do
      select case (this%state(idx))
      case (EMPTY)
      return
      case (OCCUPIED)
      if (this%keys(idx) == key) then
        this%state(idx) = DELETED
        this%size = this%size - 1
        return
      end if
      case (DELETED)
      end select
      idx = idx + 1
      if (idx > this%capacity) idx = 1
      if (idx == start) return
    end do
  end subroutine remove_int_int

  ! Remove key (int-log)
  subroutine remove_int_log(this, key)
    class(DictIntLog), intent(inout) :: this
    integer, intent(in) :: key
    integer :: idx, start
    start = hash_int(key, this%capacity)
    idx = start
    do
      select case (this%state(idx))
      case (EMPTY)
      return
      case (OCCUPIED)
      if (this%keys(idx) == key) then
        this%state(idx) = DELETED
        this%size = this%size - 1
        return
      end if
      case (DELETED)
      end select
      idx = idx + 1
      if (idx > this%capacity) idx = 1
      if (idx == start) return
    end do
  end subroutine remove_int_log

  ! Contains key
  pure function contains_key_int(this, key) result(res)
    class(*), intent(in) :: this
    integer, intent(in) :: key
    logical :: res
    if (associated(this%state)) then
      call get_int_int(this, key, res, res)
    else
      call get_int_log(this, key, res, res)
    end if
  end function contains_key_int

  ! Clear and optionally set new capacity (int-int)
  subroutine clear_int_int(this, newsize)
    class(DictIntInt), intent(inout) :: this
    integer, intent(in), optional :: newsize
    integer :: c
    if (present(newsize)) then
      c = newsize
    else
      c = this%capacity
    end if
    this%capacity = c
    this%size = 0
    if (allocated(this%keys)) deallocate(this%keys)
    if (allocated(this%values)) deallocate(this%values)
    if (allocated(this%state)) deallocate(this%state)
    allocate(this%keys(c))
    allocate(this%values(c))
    allocate(this%state(c))
    this%state = EMPTY
  end subroutine clear_int_int

  ! Clear and optionally set new capacity (int-log)
  subroutine clear_int_log(this, newsize)
    class(DictIntLog), intent(inout) :: this
    integer, intent(in), optional :: newsize
    integer :: c
    if (present(newsize)) then
      c = newsize
    else
      c = this%capacity
    end if
    this%capacity = c
    this%size = 0
    if (allocated(this%keys)) deallocate(this%keys)
    if (allocated(this%values)) deallocate(this%values)
    if (allocated(this%state)) deallocate(this%state)
    allocate(this%keys(c))
    allocate(this%values(c))
    allocate(this%state(c))
    this%state = EMPTY
  end subroutine clear_int_log

  ! Return list of keys (int-int)
  subroutine keys_int_int(this, klist, n)
    class(DictIntInt), intent(in) :: this
    integer, allocatable, intent(out) :: klist(:)
    integer, intent(out) :: n
    integer :: i, cnt
    cnt = this%size
    allocate(klist(cnt))
    n = 0
    do i = 1, this%capacity
      if (this%state(i) == OCCUPIED) then
        n = n + 1
        klist(n) = this%keys(i)
      end if
    end do
  end subroutine keys_int_int

  ! Return list of values (int-int)
  subroutine values_int_int(this, vlist, n)
    class(DictIntInt), intent(in) :: this
    integer, allocatable, intent(out) :: vlist(:)
    integer, intent(out) :: n
    integer :: i
    allocate(vlist(this%size))
    n = 0
    do i = 1, this%capacity
      if (this%state(i) == OCCUPIED) then
        n = n + 1
        vlist(n) = this%values(i)
      end if
    end do
  end subroutine values_int_int

  ! Return list of keys (int-log)
  subroutine keys_int_log(this, klist, n)
    class(DictIntLog), intent(in) :: this
    integer, allocatable, intent(out) :: klist(:)
    integer, intent(out) :: n
    integer :: i
    allocate(klist(this%size))
    n = 0
    do i = 1, this%capacity
      if (this%state(i) == OCCUPIED) then
        n = n + 1
        klist(n) = this%keys(i)
      end if
    end do
  end subroutine keys_int_log

  ! Return list of values (int-log)
  subroutine values_int_log(this, vlist, n)
    class(DictIntLog), intent(in) :: this
    logical, allocatable, intent(out) :: vlist(:)
    integer, intent(out) :: n
    integer :: i
    allocate(vlist(this%size))
    n = 0
    do i = 1, this%capacity
      if (this%state(i) == OCCUPIED) then
        n = n + 1
        vlist(n) = this%values(i)
      end if
    end do
  end subroutine values_int_log

end module dict_mod

