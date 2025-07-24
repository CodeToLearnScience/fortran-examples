! Example program demonstrating dictionary usage
program test_dictionary
  use dictionary_module
  implicit none
  
  type(dictionary_type) :: dict
  type(value_type) :: val
  character(len=:), allocatable :: str_val
  integer(kind=4) :: int32_val
  integer(kind=8) :: int64_val
  real(kind=4) :: real32_val
  real(kind=8) :: real64_val
  logical :: bool_val
  character(len=:), allocatable :: keys(:)
  integer :: i, type_code
  
  ! Add various types to dictionary
  call val%set_int32(42)
  call dict%set("int32_key", val)
  
  call val%set_int64(9223372036854775807_8) ! Max int64 value
  call dict%set("int64_key", val)
  
  call val%set_real32(3.14159_4)
  call dict%set("float_key", val)
  
  call val%set_real64(3.141592653589793238_8)
  call dict%set("double_key", val)
  
  call val%set_string("Hello, Fortran Dictionary!")
  call dict%set("greeting", val)
  
  call val%set_logical(.true.)
  call dict%set("is_working", val)
  
  ! Retrieve and display values with explicit type checks
  val = dict%get("int32_key")
  type_code = val%get_type_code()
  print *, "int32_key type_code:", type_code
  int32_val = val%get_int32()
  print *, "int32_key: ", int32_val
  
  val = dict%get("int64_key")
  type_code = val%get_type_code()
  print *, "int64_key type_code:", type_code
  int64_val = val%get_int64()
  print *, "int64_key: ", int64_val
  
  val = dict%get("float_key")
  type_code = val%get_type_code()
  print *, "float_key type_code:", type_code
  real32_val = val%get_real32()
  print *, "float_key: ", real32_val
  
  val = dict%get("double_key")
  type_code = val%get_type_code()
  print *, "double_key type_code:", type_code
  real64_val = val%get_real64()
  print *, "double_key: ", real64_val
  
  val = dict%get("greeting")
  type_code = val%get_type_code()
  print *, "greeting type_code:", type_code
  str_val = val%get_string()
  print *, "greeting: ", str_val
  
  val = dict%get("is_working")
  type_code = val%get_type_code()
  print *, "is_working type_code:", type_code
  bool_val = val%get_logical()
  print *, "is_working: ", bool_val
  
  ! Check if key exists
  print *, "has 'double_key': ", dict%has_key("double_key")
  print *, "has 'xyz': ", dict%has_key("xyz")
  
  ! Get size
  print *, "Dictionary size: ", dict%get_size()
  
  ! Get all keys
  keys = dict%get_keys()
  print *, "Keys:"
  do i = 1, size(keys)
    print *, "  - ", keys(i)
  end do
  
  ! Remove a key
  call dict%remove("double_key")
  print *, "After removing 'double_key', size: ", dict%get_size()
  print *, "has 'double_key': ", dict%has_key("double_key")
  
  ! Clean up
  call dict%cleanup()
  
end program test_dictionary
