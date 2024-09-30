!user-defined derived data-type input/output example
module uddtio
  use, intrinsic :: iso_fortran_env, only : int32, real64
  type :: uddt 
    integer :: n
    real(kind=real64) :: a, b, c 
    contains 
    procedure, pass :: write_uddt
    procedure, pass :: read_uddt
    generic, public :: write(formatted)=> write_uddt
    generic, public :: read(formatted) => read_uddt
  end type uddt 

  contains 
  subroutine write_uddt(dtv, unit, iotype, v_list, iostat, iomsg)
    class(uddt), intent(in) :: dtv 
    integer, intent(in) :: unit 
    character(*), intent(in) :: iotype 
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat 
    character(*), intent(inout) :: iomsg
    
    write(unit, '("++++++++++++++++++++++++++++++++++++++++++++++++++++")')
    write(unit, *, iostat=iostat, iomsg=iomsg) new_line("")
    write(unit, *, iostat=iostat, iomsg=iomsg) dtv%n
    write(unit, *, iostat=iostat, iomsg=iomsg) new_line("")
    write(unit, '("++++++++++++++++++++++++++++++++++++++++++++++++++++")')
    write(unit, *, iostat=iostat, iomsg=iomsg) dtv%a, dtv%b, dtv%c  
    write(unit, *, iostat=iostat, iomsg=iomsg) new_line("")
    write(unit, '("++++++++++++++++++++++++++++++++++++++++++++++++++++")')
  end subroutine write_uddt 

  subroutine read_uddt(dtv, unit, iotype, v_list, iostat, iomsg)
    class(uddt), intent(inout) :: dtv 
    integer, intent(in) :: unit 
    character(*), intent(in) :: iotype 
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat 
    character(*), intent(inout) :: iomsg

    character(256) :: dummy

    read(unit, *, iostat=iostat, iomsg=iomsg) dummy 
    read(unit, *, iostat=iostat, iomsg=iomsg) dtv%n 
    read(unit, *, iostat=iostat, iomsg=iomsg) dummy
    read(unit, *, iostat=iostat, iomsg=iomsg) dtv%a, dtv%b, dtv%c 
  end subroutine read_uddt 
end module uddtio 

program main 
  use uddtio 
  type(uddt) :: dt 
  open(unit, file='input.dat', form='formatted')
  read(unit, fmt='(dt)', advance='no') dt 
  close(unit)

  open(11, file='output.dat', form='formatted', status="replace") 
  write(11, fmt='(dt)', advance='no') dt 
  close(11)
  print*, dt%n, dt%a, dt%b, dt%c 
end program main 
