!submodule which inherits mod_udt (notice we don't have to use mod_udt)
!the interface defined for procedure rectangle_area is defined in mod_udt module
submodule(mod_udt) submod_udt
  implicit none
  type(udt) :: udt_obj

  contains
  module procedure reactangle_area
    implicit none
    udt_obj%length = 2.0
    udt_obj%breadth = 3.0
    udt_obj%area = udt_obj%length*udt_obj%breadth
    print*, "Area of rectangle is: ", udt_obj%area
  end procedure reactangle_area
end submodule submod_udt

