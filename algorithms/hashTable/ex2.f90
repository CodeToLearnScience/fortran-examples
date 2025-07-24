!> dict_mod: interface and type definitions
module dict_mod
  implicit none
  private
  public :: DictIntInt, DictIntLog, GenericDict, new_GenericDict

  ! integer-keyed specializations
  type, public :: DictIntInt
    private
    integer :: capacity=0, size=0
    integer, allocatable, contiguous :: keys(:), state(:)
    integer, allocatable, contiguous :: values(:)
    contains
    procedure :: put => put_int_int
    procedure :: get => get_int_int
    procedure :: remove => remove_int_int
    procedure :: contains_key => contains_key_int
    final    :: finalize_int_int
  end type

  type, public :: DictIntLog
    private
    integer :: capacity=0, size=0
    integer, allocatable, contiguous :: keys(:), state(:)
    logical, allocatable, contiguous :: values(:)
    contains
    procedure :: put => put_int_log
    procedure :: get => get_int_log
    procedure :: remove => remove_int_log
    procedure :: contains_key => contains_key_int
    final    :: finalize_int_log
  end type

  ! generic dictionary with unlimited polymorphic values
  type, public :: GenericDict
    private
    integer :: capacity=0, size=0
    integer, allocatable, contiguous :: keys(:), state(:)
    class(*), allocatable, contiguous :: values(:)
    contains
    procedure :: put => put_generic
    procedure :: get => get_generic
    procedure :: remove => remove_generic
    procedure :: contains_key => contains_key_generic
    final    :: finalize_generic
  end type

  interface new_GenericDict
  module procedure new_GenericDict_impl
  end interface

  contains
  ! empty: implementation in submodule
end module dict_mod

!> dict_impl: implementation of all procedures
submodule (dict_mod) dict_impl
implicit none
integer, parameter :: EMPTY=0, OCCUPIED=1, DELETED=2
real,    parameter :: MAX_LOAD=0.7

contains
! constructor for generic dict
function new_GenericDict_impl(init_cap) result(d)
  integer, intent(in), optional :: init_cap
  type(GenericDict) :: d
  integer :: c
  c = max(1, merge(init_cap,16,present(init_cap)))
  d%capacity = c; d%size = 0
  allocate(d%keys(c), d%state(c), d%values(c))
  d%state = EMPTY
end function

! finalize generic
subroutine finalize_generic(this)
  type(GenericDict), intent(inout) :: this
  if (allocated(this%keys))   deallocate(this%keys)
  if (allocated(this%state))  deallocate(this%state)
  if (allocated(this%values)) deallocate(this%values)
end subroutine

! put for generic
subroutine put_generic(this, key, val)
  class(GenericDict), intent(inout) :: this
  integer, intent(in) :: key
  class(*), intent(in) :: val
  integer :: idx, start
  if (real(this%size+1)/this%capacity > MAX_LOAD) call rehash_generic(this)
  start = mod(key,this%capacity)+1; idx = start
  do
    select case (this%state(idx))
    case (EMPTY, DELETED)
    this%keys(idx)=key; this%values(idx)=val; this%state(idx)=OCCUPIED; this%size=this%size+1; return
    case (OCCUPIED)
    if (this%keys(idx)==key) then this%values(idx)=val; return; end if
    end select
    idx = idx+1; if (idx>this%capacity) idx=1
    if (idx==start) exit
  end do
end subroutine

! get for generic
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
found=.false.; return
case (OCCUPIED)
if (this%keys(idx)==key) then val = this%values(idx); found=.true.; return; end if
end select
idx=idx+1; if(idx>this%capacity) idx=1
if(idx==start) then found=.false.; return; end if
end do
end subroutine

! remove generic
subroutine remove_generic(this, key)
class(GenericDict), intent(inout) :: this
integer, intent(in) :: key
integer :: idx, start
start = mod(key,this%capacity)+1; idx = start
do
if (this%state(idx)==OCCUPIED .and. this%keys(idx)==key) then this%state(idx)=DELETED; this%size=this%size-1; return; end if
if (this%state(idx)==EMPTY) return
idx=idx+1; if(idx>this%capacity) idx=1
if(idx==start) return
end do
end subroutine

! contains for generic
pure function contains_key_generic(this, key) result(res)
class(GenericDict), intent(in) :: this; integer, intent(in) :: key; logical :: res
call get_generic(this, key, res, res)
end function

! rehash generic
subroutine rehash_generic(this)
class(GenericDict), intent(inout) :: this
integer :: old_cap, i
integer, allocatable :: ok(:), os(:)
class(*), allocatable :: ov(:)
old_cap=this%capacity; ok=this%keys; os=this%state; ov=this%values
call clear(this, old_cap*2)
do concurrent (i=1:old_cap)
  if(os(i)==OCCUPIED) call put_generic(this, ok(i), ov(i))
end do
end subroutine

! clear generic
subroutine clear(this, newsize)
class(GenericDict), intent(inout) :: this; integer, intent(in), optional :: newsize
integer :: c
c = merge(newsize, this%capacity, present(newsize))
this%capacity=c; this%size=0
if (allocated(this%keys))   deallocate(this%keys)
if (allocated(this%state))  deallocate(this%state)
if (allocated(this%values)) deallocate(this%values)
allocate(this%keys(c), this%state(c), this%values(c))
this%state=EMPTY
end subroutine

! finalize int-int
subroutine finalize_int_int(this)
type(DictIntInt), intent(inout) :: this
if (allocated(this%keys))   deallocate(this%keys,this%values,this%state)
end subroutine

subroutine finalize_int_log(this)
type(DictIntLog), intent(inout) :: this
if (allocated(this%keys))   deallocate(this%keys,this%values,this%state)
end subroutine

! (other int-int, int-log procedures unchanged...)

end submodule dict_impl

!> Example usage of GenericDict in a main program
program test_generic_dict
use dict_mod
implicit none

type(GenericDict) :: gd
class(*), allocatable :: v
logical :: found

! Create a generic dictionary with initial capacity 8
gd = new_GenericDict(8)

! Store different value types
call gd%put(1,  100)           ! integer
call gd%put(2,  3.14159)       ! real
call gd%put(3,  'Fortran')     ! character(len=*)
call gd%put(4,  .true.)        ! logical

! Retrieve and print each
call gd%get(1, v, found)
if (found) print *, 'Key 1 ->', v

call gd%get(2, v, found)
if (found) print *, 'Key 2 ->', v

call gd%get(3, v, found)
if (found) print *, 'Key 3 ->', trim(v)

call gd%get(4, v, found)
if (found) print *, 'Key 4 ->', v

! Check membership
print *, 'Contains key 5?', gd%contains_key(5)

! Remove a key
call gd%remove(2)
print *, 'Contains key 2 after remove?', gd%contains_key(2)
end program test_generic_dict

