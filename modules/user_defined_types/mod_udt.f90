!example to demonstrate the use of fortran submodule
!This defines interface for subroutine which will be implemented in separate
!submodule use_mod_udt.f90 fortran file. That inherits this main module.
!Later the function defined in that submodule is used in use_mod_submod module
!thereby avoiding circular dependency.

module mod_udt
  ! use mod_use_mod_udt
  implicit none
  type udt
    real :: length, breadth, area
  end type udt

  interface
    module subroutine reactangle_area(obj)
      type(udt), intent(inout) :: obj
    end subroutine reactangle_area
  end interface
end module mod_udt

!module to call the subroutine implemented in submodule
module use_mod_submod
  use mod_udt
  implicit none
  contains
  subroutine call_rectangle_area_func
    type(udt) :: obj
    call reactangle_area(obj)
  end subroutine call_rectangle_area_func
end module use_mod_submod

