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
  integer :: num_entries = 1000
  character(len=20) :: temp_key

  ! Add various basic types to dictionary
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

  ! Add a large number of entries to test hashing and resizing
  do i = 1, num_entries
    write(temp_key, '(a,i0)') "key_", i
    call val%set_int32(i)
    call dict%set(trim(temp_key), val)
  end do

  print *, "Dictionary size after adding ", num_entries, " items: ", dict%get_size()

  ! Retrieve and display some values
  val = dict%get("int32_key")
  type_code = val%get_type_code()
  print *, "int32_key type_code:", type_code
  int32_val = val%get_int32()
  print *, "int32_key: ", int32_val

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

  ! Check some random entries from the bulk insertion
  do i = 1, 5
    write(temp_key, '(a,i0)') "key_", i * 100
    print *, "Checking for ", trim(temp_key), ": ", dict%has_key(trim(temp_key))

    if (dict%has_key(trim(temp_key))) then
      val = dict%get(trim(temp_key))
      int32_val = val%get_int32()
      print *, trim(temp_key), " value: ", int32_val
    end if
  end do

  ! Remove some entries
  call dict%remove("double_key")
  print *, "After removing 'double_key', exists?: ", dict%has_key("double_key")

  ! Try to get a key that doesn't exist
  if (.not. dict%has_key("nonexistent_key")) then
    print *, "As expected, 'nonexistent_key' not found"
  end if

  ! Get number of entries
  print *, "Final dictionary size: ", dict%get_size()

  ! Clean up
  call dict%cleanup()

  print *, "Dictionary size after cleanup: ", dict%get_size()

end program test_dictionary
