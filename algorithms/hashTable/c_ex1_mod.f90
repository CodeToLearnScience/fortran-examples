module dictionary_module
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64
  implicit none
  private

  ! The type that can store any data type
  type, public :: value_type
    private
    integer :: type_code = 0  ! 0=not set, 1=int32, 2=int64, 3=real32, 4=real64, 5=logical, 6=string, 7=array
    integer(int32) :: int32_value = 0
    integer(int64) :: int64_value = 0
    real(real32) :: real32_value = 0.0
    real(real64) :: real64_value = 0.0
    logical :: logical_value = .false.
    character(len=:), allocatable :: string_value
    type(c_ptr) :: array_ptr = c_null_ptr
    integer :: array_size = 0
    integer :: array_type = 0  ! Same coding as type_code
  contains
    ! Individual accessor methods
    procedure :: get_int32 => value_get_int32
    procedure :: get_int64 => value_get_int64
    procedure :: get_real32 => value_get_real32
    procedure :: get_real64 => value_get_real64
    procedure :: get_logical => value_get_logical
    procedure :: get_string => value_get_string
    procedure :: get_type_code => value_get_type_code
    ! Individual setter methods
    procedure :: set_int32 => value_set_int32
    procedure :: set_int64 => value_set_int64
    procedure :: set_real32 => value_set_real32
    procedure :: set_real64 => value_set_real64
    procedure :: set_logical => value_set_logical
    procedure :: set_string => value_set_string
    ! Generic setter interface
    generic :: set => set_int32, set_int64, set_real32, set_real64, set_logical, set_string
    procedure :: cleanup => value_cleanup
    final :: value_finalizer
  end type value_type

  ! Key-value pair for the dictionary
  type :: dict_pair
    character(len=:), allocatable :: key
    type(value_type) :: value
    type(dict_pair), pointer :: next => null()
  end type dict_pair

  ! The dictionary type
  type, public :: dictionary_type
    private
    type(dict_pair), pointer :: first => null()
    integer :: size = 0
  contains
    procedure :: get => dict_get
    procedure :: set => dict_set
    procedure :: has_key => dict_has_key
    procedure :: remove => dict_remove
    procedure :: get_size => dict_size
    procedure :: get_keys => dict_keys
    procedure :: cleanup => dict_cleanup
    final :: dict_finalizer
  end type dictionary_type

contains

  ! Value type methods
  function value_get_type_code(this) result(code)
    class(value_type), intent(in) :: this
    integer :: code
    code = this%type_code
  end function value_get_type_code
  
  function value_get_int32(this) result(val)
    class(value_type), intent(in) :: this
    integer(int32) :: val
    val = this%int32_value
  end function value_get_int32
  
  function value_get_int64(this) result(val)
    class(value_type), intent(in) :: this
    integer(int64) :: val
    val = this%int64_value
  end function value_get_int64
  
  function value_get_real32(this) result(val)
    class(value_type), intent(in) :: this
    real(real32) :: val
    val = this%real32_value
  end function value_get_real32
  
  function value_get_real64(this) result(val)
    class(value_type), intent(in) :: this
    real(real64) :: val
    val = this%real64_value
  end function value_get_real64
  
  function value_get_logical(this) result(val)
    class(value_type), intent(in) :: this
    logical :: val
    val = this%logical_value
  end function value_get_logical
  
  function value_get_string(this) result(val)
    class(value_type), intent(in) :: this
    character(len=:), allocatable :: val
    if (allocated(this%string_value)) then
      val = this%string_value
    else
      val = ""
    end if
  end function value_get_string
  
  subroutine value_set_int32(this, val)
    class(value_type), intent(inout) :: this
    integer(int32), intent(in) :: val
    call this%cleanup()
    this%type_code = 1
    this%int32_value = val
  end subroutine value_set_int32
  
  subroutine value_set_int64(this, val)
    class(value_type), intent(inout) :: this
    integer(int64), intent(in) :: val
    call this%cleanup()
    this%type_code = 2
    this%int64_value = val
  end subroutine value_set_int64
  
  subroutine value_set_real32(this, val)
    class(value_type), intent(inout) :: this
    real(real32), intent(in) :: val
    call this%cleanup()
    this%type_code = 3
    this%real32_value = val
  end subroutine value_set_real32
  
  subroutine value_set_real64(this, val)
    class(value_type), intent(inout) :: this
    real(real64), intent(in) :: val
    call this%cleanup()
    this%type_code = 4
    this%real64_value = val
  end subroutine value_set_real64
  
  subroutine value_set_logical(this, val)
    class(value_type), intent(inout) :: this
    logical, intent(in) :: val
    call this%cleanup()
    this%type_code = 5
    this%logical_value = val
  end subroutine value_set_logical
  
  subroutine value_set_string(this, val)
    class(value_type), intent(inout) :: this
    character(len=*), intent(in) :: val
    call this%cleanup()
    this%type_code = 6
    this%string_value = val
  end subroutine value_set_string
  
  subroutine value_cleanup(this)
    class(value_type), intent(inout) :: this
    if (this%type_code == 6 .and. allocated(this%string_value)) then
      deallocate(this%string_value)
    end if
    if (this%type_code == 7) then
      ! Instead of comparing c_ptr directly, check if it points to something
      if (c_associated(this%array_ptr)) then
        ! Here we would deallocate array based on type and size
        ! This is simplified for brevity
        this%array_ptr = c_null_ptr
      end if
    end if
    this%type_code = 0
  end subroutine value_cleanup
  
  subroutine value_finalizer(this)
    type(value_type), intent(inout) :: this
    call this%cleanup()
  end subroutine value_finalizer

  ! Dictionary methods
  function dict_has_key(this, key) result(exists)
    class(dictionary_type), intent(in) :: this
    character(len=*), intent(in) :: key
    logical :: exists
    type(dict_pair), pointer :: current
    
    exists = .false.
    current => this%first
    
    do while (associated(current))
      if (current%key == key) then
        exists = .true.
        return
      end if
      current => current%next
    end do
  end function dict_has_key
  
  function dict_get(this, key, default) result(value)
    class(dictionary_type), intent(in) :: this
    character(len=*), intent(in) :: key
    type(value_type), intent(in), optional :: default
    type(value_type) :: value
    type(dict_pair), pointer :: current
    
    current => this%first
    
    do while (associated(current))
      if (current%key == key) then
        value = current%value
        return
      end if
      current => current%next
    end do
    
    ! Key not found, return default if provided
    if (present(default)) then
      value = default
    end if
  end function dict_get
  
  subroutine dict_set(this, key, value)
    class(dictionary_type), intent(inout) :: this
    character(len=*), intent(in) :: key
    type(value_type), intent(in) :: value
    type(dict_pair), pointer :: current, new_pair
    
    ! Check if key already exists
    current => this%first
    do while (associated(current))
      if (current%key == key) then
        current%value = value
        return
      end if
      current => current%next
    end do
    
    ! Key doesn't exist, add new pair
    allocate(new_pair)
    new_pair%key = key
    new_pair%value = value
    new_pair%next => this%first
    this%first => new_pair
    this%size = this%size + 1
  end subroutine dict_set
  
  subroutine dict_remove(this, key)
    class(dictionary_type), intent(inout) :: this
    character(len=*), intent(in) :: key
    type(dict_pair), pointer :: current, previous
    
    current => this%first
    previous => null()
    
    do while (associated(current))
      if (current%key == key) then
        if (associated(previous)) then
          previous%next => current%next
        else
          this%first => current%next
        end if
        deallocate(current)
        this%size = this%size - 1
        return
      end if
      previous => current
      current => current%next
    end do
  end subroutine dict_remove
  
  function dict_size(this) result(size)
    class(dictionary_type), intent(in) :: this
    integer :: size
    size = this%size
  end function dict_size
  
  function dict_keys(this) result(keys)
    class(dictionary_type), intent(in) :: this
    character(len=:), allocatable :: keys(:)
    type(dict_pair), pointer :: current
    integer :: i
    
    allocate(character(len=0) :: keys(this%size))
    
    current => this%first
    i = 1
    do while (associated(current))
      keys(i) = current%key
      current => current%next
      i = i + 1
    end do
  end function dict_keys
  
  subroutine dict_cleanup(this)
    class(dictionary_type), intent(inout) :: this
    type(dict_pair), pointer :: current, next
    
    current => this%first
    do while (associated(current))
      next => current%next
      call current%value%cleanup()
      deallocate(current)
      current => next
    end do
    
    this%first => null()
    this%size = 0
  end subroutine dict_cleanup
  
  subroutine dict_finalizer(this)
    type(dictionary_type), intent(inout) :: this
    call this%cleanup()
  end subroutine dict_finalizer
  
end module dictionary_module
