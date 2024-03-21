!example to avoid circular dependency in fortran modules
program  udt_ex
  use use_mod_submod
  implicit none

  call call_rectangle_area_func

end program udt_ex
