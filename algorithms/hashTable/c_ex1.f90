! Example program demonstrating dictionary usage
program test_dictionary
  use dictionary_module
  implicit none
  
  type(dictionary_type) :: dict
  type(value_type) :: val
  character(len=:), allocatable :: str_val
  integer :: int_val
  real :: real_val
  logical :: bool_val
  character(len=:), allocatable :: keys(:)
  integer :: i
  
  ! Add various types to dictionary
  call val%set(42)
  call dict%set("int_key", val)
  
  call val%set(3.14159)
  call dict%set("pi", val)
  
  call val%set("Hello, Fortran Dictionary!")
  call dict%set("greeting", val)
  
  call val%set(.true.)
  call dict%set("is_working", val)
  
  ! Retrieve and display values
  val = dict%get("int_key")
  int_val = val%get_int32()
  print *, "int_key: ", int_val
  
  val = dict%get("pi")
  real_val = val%get_real64()
  print *, "pi: ", real_val
  
  val = dict%get("greeting")
  str_val = val%get_string()
  print *, "greeting: ", str_val
  
  val = dict%get("is_working")
  bool_val = val%get_logical()
  print *, "is_working: ", bool_val
  
  ! Check if key exists
  print *, "has 'pi': ", dict%has_key("pi")
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
  call dict%remove("pi")
  print *, "After removing 'pi', size: ", dict%get_size()
  print *, "has 'pi': ", dict%has_key("pi")
  
  ! Clean up
  call dict%cleanup()
  
end program test_dictionary
