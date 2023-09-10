

program shape_mod

  use shape
  implicit none
  include "mpif.h"
  

  call set_shape(10.5, 20.2)
  call get_area()

end program shape_mod
