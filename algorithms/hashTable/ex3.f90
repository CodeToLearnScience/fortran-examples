module dict_mod
  ! Fortran 2003/2008 module implementing Python-like dictionaries
  implicit none
  private
  public :: DictIntInt, DictIntLog, GenericDict, new_GenericDict

  ! State flags and load factor
  integer, parameter :: EMPTY=0, OCCUPIED=1, DELETED=2
  real,    parameter :: MAX_LOAD=0.7

  ! Integer->Integer dictionary type
  type, public :: DictIntInt
    integer :: capacity=0, size=0
    integer, allocatable, contiguous :: keys(:), state(:), values(:)
  contains
    procedure :: put => put_int_int
    procedure :: get => get_int_int
    procedure :: remove => remove_int_int
    procedure :: contains_key => contains_key_int
    procedure :: clear => clear_int_int
    procedure :: rehash => rehash_int_int
    final    :: finalize_int_int
  end type DictIntInt

  ! Integer->Logical dictionary type
  type, public :: DictIntLog
    integer :: capacity=0, size=0
    integer, allocatable, contiguous :: keys(:), state(:)
    logical, allocatable, contiguous :: values(:)
  contains
    procedure :: put => put_int_log
    procedure :: get => get_int_log
    procedure :: remove => remove_int_log
    procedure :: contains_key => contains_key_int
    procedure :: clear => clear_int_log
    procedure :: rehash => rehash_int_log
    final    :: finalize_int_log
  end type DictIntLog

  ! Generic dictionary type with unlimited polymorphic values
  type, public :: GenericDict
    integer :: capacity=0, size=0
    integer, allocatable, contiguous :: keys(:), state(:)
    class(*), allocatable, contiguous :: values(:)
  contains
    procedure :: put => put_generic
    procedure :: get => get_generic
    procedure :: remove => remove_generic
    procedure :: contains_key => contains_key_generic
    procedure :: clear => clear_generic
    procedure :: rehash => rehash_generic
    final    :: finalize_generic
  end type GenericDict

  interface new_GenericDict
    module procedure new_GenericDict_impl
  end interface

  ! Fortran 2003/2008 module implementing Python-like dictionaries
  implicit none
  private
  public :: DictIntInt, DictIntLog, GenericDict, new_GenericDict

  integer, parameter :: EMPTY=0, OCCUPIED=1, DELETED=2
  real,    parameter :: MAX_LOAD=0.7

  interface new_GenericDict
    module procedure new_GenericDict_impl
  end interface

contains  ! Primary module contains both interfaces and implementations

  !> Generic constructor
  function new_GenericDict_impl(init_cap) result(d)
    integer, intent(in), optional :: init_cap
    type(GenericDict) :: d
    integer :: c
    c = max(1, merge(init_cap,16,present(init_cap)))
    d%capacity = c; d%size = 0
    allocate(d%keys(c), d%state(c), d%values(c))
    d%state = EMPTY
  end function new_GenericDict_impl

  !> Finalizers
  subroutine finalize_int_int(this)
    type(DictIntInt), intent(inout) :: this
    if (allocated(this%keys))    deallocate(this%keys,this%values,this%state)
  end subroutine finalize_int_int

  subroutine finalize_int_log(this)
    type(DictIntLog), intent(inout) :: this
    if (allocated(this%keys))    deallocate(this%keys,this%values,this%state)
  end subroutine finalize_int_log

  subroutine finalize_generic(this)
    type(GenericDict), intent(inout) :: this
    if (allocated(this%keys))    deallocate(this%keys)
    if (allocated(this%state))   deallocate(this%state)
    if (allocated(this%values))  deallocate(this%values)
  end subroutine finalize_generic

  !> Generic methods (put/get/remove/contains/clear/rehash)
  subroutine put_generic(this, key, val)
    class(GenericDict), intent(inout) :: this
    integer, intent(in) :: key
    class(*), intent(in) :: val
    integer :: idx, start
    if (real(this%size+1)/this%capacity > MAX_LOAD) call rehash_generic(this)
    start = mod(key,this%capacity)+1; idx = start
    do
      select case (this%state(idx))
      case (EMPTY,DELETED)
        this%keys(idx)=key; this%values(idx)=val; this%state(idx)=OCCUPIED; this%size=this%size+1; return
      case (OCCUPIED)
        if (this%keys(idx)==key) then this%values(idx)=val; return; end if
      end select
      idx = idx + 1; if (idx>this%capacity) idx=1
      if (idx==start) exit
    end do
  end subroutine put_generic

  subroutine get_generic(this, key, val, found)
    class(GenericDict), intent(in) :: this
    integer, intent(in) :: key
    class(*), allocatable, intent(out) :: val
    logical, intent(out) :: found
    integer :: idx, start
    start = mod(key,this%capacity)+1; idx = start
    do
      select case (this%state(idx))
      case (EMPTY)
        found = .false.; return
      case (OCCUPIED)
        if (this%keys(idx)==key) then val = this%values(idx); found = .true.; return; end if
      end select
      idx = idx + 1; if (idx>this%capacity) idx=1
      if (idx==start) then found = .false.; return; end if
    end do
  end subroutine get_generic

  subroutine remove_generic(this, key)
    class(GenericDict), intent(inout) :: this
    integer, intent(in) :: key
    integer :: idx, start
    start = mod(key,this%capacity)+1; idx = start
    do
      if (this%state(idx)==OCCUPIED .and. this%keys(idx)==key) then this%state(idx)=DELETED; this%size=this%size-1; return; end if
      if (this%state(idx)==EMPTY) return
      idx = idx + 1; if (idx>this%capacity) idx=1
      if (idx==start) return
    end do
  end subroutine remove_generic

  pure function contains_key_generic(this, key) result(res)
    class(GenericDict), intent(in) :: this; integer, intent(in) :: key; logical :: res
    call get_generic(this, key, res, res)
  end function contains_key_generic

  subroutine clear_generic(this, newsize)
    class(GenericDict), intent(inout) :: this; integer, intent(in), optional :: newsize
    integer :: c
    c = merge(newsize, this%capacity, present(newsize))
    this%capacity = c; this%size = 0
    if (allocated(this%keys))   deallocate(this%keys)
    if (allocated(this%state))  deallocate(this%state)
    if (allocated(this%values)) deallocate(this%values)
    allocate(this%keys(c), this%state(c), this%values(c))
    this%state = EMPTY
  end subroutine clear_generic

  subroutine rehash_generic(this)
    class(GenericDict), intent(inout) :: this
    integer :: old_cap, i
    integer, allocatable :: ok(:), os(:)
    class(*), allocatable :: ov(:)
    old_cap = this%capacity; ok = this%keys; os = this%state; ov = this%values
    call clear_generic(this, old_cap*2)
    do concurrent (i=1:old_cap)
      if (os(i)==OCCUPIED) call put_generic(this, ok(i), ov(i))
    end do
  end subroutine rehash_generic

  !> Integer-specific and logical-specific dicts (omitted unchanged)

end module dict_mod

!> Example usage
program test_generic_dict
  use dict_mod
  implicit none
  type(GenericDict) :: gd
  class(*), allocatable :: v
  logical :: found

  gd = new_GenericDict(8)
  call gd%put(1, 100)
  call gd%put(2, 3.14)
  call gd%put(3, 'Fortran')
  call gd%put(4, .true.)

  call gd%get(1, v, found)
  if (found) print *, '1->', v
  call gd%get(2, v, found)
  if (found) print *, '2->', v
  call gd%get(3, v, found)
  if (found) print *, '3->', trim(v)
  call gd%get(4, v, found)
  if (found) print *, '4->', v
end program test_generic_dict

