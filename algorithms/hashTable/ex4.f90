module dict_mod
  ! Fortran 2003/2008 module implementing Python-like integer-keyed dictionaries
  implicit none
  private
  public :: DictIntInt, DictIntLog, new_DictIntInt, new_DictIntLog

  integer, parameter :: EMPTY=0, OCCUPIED=1, DELETED=2
  real,    parameter :: MAX_LOAD=0.7

  type, public :: DictIntInt
    integer :: capacity=0, size=0
    integer, allocatable :: keys(:), state(:), values(:)
  contains
    procedure :: put => put_int_int
    procedure :: get => get_int_int
    procedure :: remove => remove_int_int
    procedure :: contains_key => contains_key_int
    procedure :: clear => clear_int_int
    procedure :: rehash => rehash_int_int
    final    :: finalize_int_int
  end type DictIntInt

  type, public :: DictIntLog
    integer :: capacity=0, size=0
    integer, allocatable :: keys(:), state(:)
    logical, allocatable :: values(:)
  contains
    procedure :: put => put_int_log
    procedure :: get => get_int_log
    procedure :: remove => remove_int_log
    procedure :: contains_key => contains_key_int
    procedure :: clear => clear_int_log
    procedure :: rehash => rehash_int_log
    final    :: finalize_int_log
  end type DictIntLog

contains

  function new_DictIntInt(init_cap) result(d)
    integer, intent(in), optional :: init_cap
    type(DictIntInt) :: d
    integer :: c
    c = max(1, merge(init_cap,16,present(init_cap)))
    d%capacity = c; d%size = 0
    allocate(d%keys(c), d%state(c), d%values(c))
    d%state = EMPTY
  end function new_DictIntInt

  function new_DictIntLog(init_cap) result(d)
    integer, intent(in), optional :: init_cap
    type(DictIntLog) :: d
    integer :: c
    c = max(1, merge(init_cap,16,present(init_cap)))
    d%capacity = c; d%size = 0
    allocate(d%keys(c), d%state(c), d%values(c))
    d%state = EMPTY
  end function new_DictIntLog

  ! finalizers
  subroutine finalize_int_int(this)
    type(DictIntInt), intent(inout) :: this
    if (allocated(this%keys))    deallocate(this%keys,this%values,this%state)
  end subroutine finalize_int_int

  subroutine finalize_int_log(this)
    type(DictIntLog), intent(inout) :: this
    if (allocated(this%keys))    deallocate(this%keys,this%values,this%state)
  end subroutine finalize_int_log

  ! hash
  pure function hash_int(key,cap) result(h)
    integer, intent(in) :: key,cap
    integer :: h
    h = mod(key,cap)+1
  end function hash_int

  ! specialized routines for IntInt
  subroutine put_int_int(this,key,val)
    class(DictIntInt), intent(inout) :: this
    integer, intent(in) :: key,val
    integer :: idx,start
    if (real(this%size+1)/this%capacity > MAX_LOAD) call this%rehash()
    start = hash_int(key,this%capacity); idx = start
    do
  if (this%state(idx)/=OCCUPIED) then
  this%keys(idx)=key; this%values(idx)=val; this%state(idx)=OCCUPIED; this%size=this%size+1; return
      elseif (this%keys(idx)==key) then
              this%values(idx)=val; return
      end if
      idx = idx+1; if (idx>this%capacity) idx=1
      if (idx==start) exit
    end do
  end subroutine put_int_int

  subroutine get_int_int(this,key,val,found)
    class(DictIntInt), intent(in) :: this
    integer, intent(in) :: key
    integer, intent(out) :: val
    logical, intent(out) :: found
    integer :: idx,start
    start = hash_int(key,this%capacity); idx = start
    do
  if (this%state(idx)==EMPTY) then found=.false.; return end if
  if (this%state(idx)==OCCUPIED .and. this%keys(idx)==key) then val=this%values(idx); found=.true.; return end if
  idx = idx+1; if(idx>this%capacity) idx=1
  if(idx==start) then found=.false.; return end if
    end do
  end subroutine get_int_int

  subroutine remove_int_int(this,key)
    class(DictIntInt), intent(inout) :: this
    integer, intent(in) :: key
    integer :: idx,start
    start=hash_int(key,this%capacity); idx=start
    do
  if (this%state(idx)==OCCUPIED .and. this%keys(idx)==key) then this%state(idx)=DELETED; this%size=this%size-1; return end if
  if (this%state(idx)==EMPTY) return
  idx=idx+1; if(idx>this%capacity) idx=1
  if(idx==start) return
    end do
  end subroutine remove_int_int

  function contains_key_int(this,key) result(res)
    class(DictIntInt), intent(in) :: this
    integer, intent(in) :: key
    logical :: res
    integer :: tmp; logical :: f
    call get_int_int(this,key,tmp,f)
    res = f
  end function contains_key_int

  subroutine clear_int_int(this)
    class(DictIntInt), intent(inout) :: this
    integer :: c
    c = this%capacity; this%size=0
    if (allocated(this%keys)) deallocate(this%keys,this%values,this%state)
    allocate(this%keys(c), this%state(c), this%values(c))
    this%state = EMPTY
  end subroutine clear_int_int

  subroutine rehash_int_int(this)
    class(DictIntInt), intent(inout) :: this
    integer :: old_cap, i, k, v, st
    integer, allocatable :: ok(:), ov(:), os(:)
    old_cap=this%capacity; ok=this%keys; ov=this%values; os=this%state
    call clear_int_int(this)
    this%capacity = old_cap*2
    call clear_int_int(this)
    do i=1,old_cap
  if (os(i)==OCCUPIED) then k=ok(i); v=ov(i); call put_int_int(this,k,v) end if
    end do
  end subroutine rehash_int_int

  ! specialized routines for IntLog
  subroutine put_int_log(this,key,val)
    class(DictIntLog), intent(inout) :: this
    integer, intent(in) :: key
    logical, intent(in) :: val
    integer :: idx,start
    if (real(this%size+1)/this%capacity > MAX_LOAD) call this%rehash()
    start = hash_int(key,this%capacity); idx = start
    do
  if (this%state(idx)/=OCCUPIED) then
  this%keys(idx)=key; this%values(idx)=val; this%state(idx)=OCCUPIED; this%size=this%size+1; return
      elseif (this%keys(idx)==key) then
              this%values(idx)=val; return
      end if
      idx = idx+1; if (idx>this%capacity) idx=1
      if (idx==start) exit
    end do
  end subroutine put_int_log

  subroutine get_int_log(this,key,val,found)
    class(DictIntLog), intent(in) :: this
    integer, intent(in) :: key
    logical, intent(out) :: val, found
    integer :: idx,start
    start = hash_int(key,this%capacity); idx = start
    do
  if (this%state(idx)==EMPTY) then found=.false.; return end if
  if (this%state(idx)==OCCUPIED .and. this%keys(idx)==key) then val=this%values(idx); found=.true.; return end if
  idx = idx+1; if(idx>this%capacity) idx=1
  if(idx==start) then found=.false.; return end if
    end do
  end subroutine get_int_log

  subroutine remove_int_log(this,key)
    class(DictIntLog), intent(inout) :: this
    integer, intent(in) :: key
    integer :: idx,start
    start=hash_int(key,this%capacity); idx=start
    do
  if (this%state(idx)==OCCUPIED .and. this%keys(idx)==key) then this%state(idx)=DELETED; this%size=this%size-1; return end if
  if (this%state(idx)==EMPTY) return
  idx=idx+1; if(idx>this%capacity) idx=1
  if(idx==start) return
    end do
  end subroutine remove_int_log

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

