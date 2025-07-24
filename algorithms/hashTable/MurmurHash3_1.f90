module dictionary_module
  use iso_fortran_env, only: int64, real64, int32
  implicit none

  private ! Default private for module entities

  !--------------------------------------------------------------------
  ! Derived Type Definitions (must come before CONTAINS)
  !--------------------------------------------------------------------

  ! --- Linked List Node ---
  type :: dict_node_t
     integer(int64) :: key
     integer(int64) :: value
     type(dict_node_t), pointer :: next => null()
  end type dict_node_t

  ! --- Dictionary Main Type ---
  type, public :: dictionary_t  ! Make the type itself public
     private                   ! Components are private by default unless specified
     type(dict_node_t), pointer :: table(:) => null() ! Array of chain heads
     integer :: table_size = 0                        ! Must be power of 2
     integer :: num_elements = 0
     real(real64) :: max_load_factor = 0.75_real64     ! Resize threshold
   contains
     ! Procedure bindings (the interface of the dictionary object)
     procedure, public :: init => dict_init           ! Initialize
     procedure, public :: destroy => dict_destroy       ! Clean up memory
     procedure, public :: set => dict_set             ! Add/update key-value
     procedure, public :: get => dict_get             ! Retrieve value by key
     procedure, public :: delete => dict_delete         ! Remove key-value
     procedure, public :: has_key => dict_has_key       ! Check if key exists
     procedure, public :: get_num_elements => dict_get_num_elements ! Get current count
     procedure, public :: get_table_size => dict_get_table_size     ! Get internal table size
     procedure, private :: resize => dict_resize        ! Grow table (internal helper)
     procedure, private :: get_index => dict_get_index  ! Map hash to table index (internal helper)
     ! Removed binding for free_chain as it's not a type-bound procedure
  end type dictionary_t

  ! Module-level constants (can go here)
  integer(kind=int64), parameter, private :: MH3_PRIME1 = -49064778989728563_int64  ! 0xff51afd7ed558ccd
  integer(kind=int64), parameter, private :: MH3_PRIME2 = -4265267296055464797_int64 ! 0xc4ceb9fe1a85ec53

!#####################################################################
! CONTAINS section for IMPLEMENTATION of module procedures
!#####################################################################
contains

  !--------------------------------------------------------------------
  ! Hash function for 64-bit integer (fmix64 from MurmurHash3) - PURE
  !--------------------------------------------------------------------
  pure function hash_integer64(key) result(hash_val)
    integer(kind=int64), intent(in) :: key
    integer(kind=int64)             :: hash_val
    integer(kind=int64)             :: k

    k = key
    k = ieor(k, ishft(k, -33))   ! k ^= k >> 33
    k = k * MH3_PRIME1
    k = ieor(k, ishft(k, -33))
    k = k * MH3_PRIME2
    k = ieor(k, ishft(k, -33))
    hash_val = k
  end function hash_integer64

  !--------------------------------------------------------------------
  ! Helper: Deallocate all nodes in a linked list chain (recursive)
  ! This is now a private MODULE subroutine, not bound to dictionary_t
  !--------------------------------------------------------------------
  recursive subroutine dict_free_chain(node_ptr)
    type(dict_node_t), pointer, intent(inout) :: node_ptr
    type(dict_node_t), pointer :: next_node

    if (.not. associated(node_ptr)) return

    next_node => node_ptr%next   ! Store pointer to the next node
    deallocate(node_ptr)         ! Deallocate the current node
    nullify(node_ptr)            ! Nullify the pointer variable passed in
    call dict_free_chain(next_node) ! Recurse on the stored next pointer

  end subroutine dict_free_chain

  !--------------------------------------------------------------------
  ! Initialize the dictionary
  !--------------------------------------------------------------------
  subroutine dict_init(self, initial_capacity)
    class(dictionary_t), intent(inout) :: self
    integer, intent(in), optional     :: initial_capacity
    integer :: actual_size
    integer :: stat

    actual_size = 16 ! Default initial size
    if (present(initial_capacity)) then
        if (initial_capacity > 0) then
           ! Compute the next power of 2 >= initial_capacity
           actual_size = 2**(ceiling(log(real(max(initial_capacity,1)))/log(2.0_real64)))
        endif
    endif
    if (actual_size <= 0) actual_size = 16 ! Fallback

    ! If table already exists, destroy it first
    ! Use ASSOCIATED for pointers
    if (associated(self%table)) call self%destroy()

    allocate(self%table(0:actual_size-1), stat=stat)
    if (stat /= 0) then
        write(*, '(A)') "Dictionary Error: Failed to allocate table in init"
        error stop 1
    endif

    self%table => null() ! Initialize all bucket pointers to null
    self%table_size = actual_size
    self%num_elements = 0
  end subroutine dict_init

  !--------------------------------------------------------------------
  ! Deallocate all nodes and the table itself
  !--------------------------------------------------------------------
  subroutine dict_destroy(self)
    class(dictionary_t), intent(inout) :: self
    integer :: i

    ! Use ASSOCIATED to check if the table pointer points to anything
    if (.not. associated(self%table)) return

    ! Now we know self%table points to an array, iterate through its elements
    do i = lbound(self%table, 1), ubound(self%table, 1)
      ! Check if the pointer element self%table(i) is associated
      if (associated(self%table(i))) then
         ! Call the module subroutine directly
         call dict_free_chain(self%table(i))
         ! self%table(i) should be nullified by dict_free_chain
      end if
    end do

    deallocate(self%table) ! Deallocate the array the table pointer points to
    nullify(self%table)    ! Nullify the table pointer itself
    self%table_size = 0
    self%num_elements = 0
  end subroutine dict_destroy

  !--------------------------------------------------------------------
  ! Calculate the table index for a given key (PURE)
  !--------------------------------------------------------------------
  pure function dict_get_index(self, key) result(idx)
    class(dictionary_t), intent(in) :: self
    integer(int64), intent(in)      :: key
    integer                         :: idx
    integer(int64)                  :: h

    h = hash_integer64(key)
    idx = int(iand(h, int(self%table_size - 1, kind=int64)))
  end function dict_get_index

  !--------------------------------------------------------------------
  ! Add or update a key-value pair
  !--------------------------------------------------------------------
  subroutine dict_set(self, key, value)
    class(dictionary_t), intent(inout) :: self
    integer(int64), intent(in)      :: key
    integer(int64), intent(in)      :: value
    integer                         :: idx
    type(dict_node_t), pointer :: current_node, new_node
    integer :: stat
    real(real64) :: load_factor

    ! Auto-initialize if table not associated or size is zero
    ! Use ASSOCIATED for pointers
    if (self%table_size <= 0 .or. .not. associated(self%table)) then
        call self%init()
    endif
    ! Check again after potential init
    if (self%table_size <= 0) then
        write(*, '(A)') "Dictionary Error: Table size zero after init in set"
        error stop 1
    endif

    idx = self%get_index(key)
    current_node => self%table(idx)

    do while (associated(current_node))
      if (current_node%key == key) then
         current_node%value = value
         return
      endif
      current_node => current_node%next
    end do

    allocate(new_node, stat=stat)
    if (stat /= 0) then
        write(*, '(A)') "Dictionary Error: Failed to allocate new node in set"
        error stop 1
    endif
    new_node%key = key
    new_node%value = value
    new_node%next => self%table(idx)
    self%table(idx) => new_node      ! Pointer assignment should be okay here
    self%num_elements = self%num_elements + 1

    load_factor = real(self%num_elements, real64) / real(self%table_size, real64)
    if (load_factor > self%max_load_factor) then
        call self%resize()
    endif
  end subroutine dict_set

  !--------------------------------------------------------------------
  ! Get the value associated with a key
  !--------------------------------------------------------------------
  function dict_get(self, key, found) result(value)
    class(dictionary_t), intent(in) :: self
    integer(int64), intent(in)      :: key
    logical, intent(out)            :: found
    integer(int64)                  :: value
    integer                         :: idx
    type(dict_node_t), pointer :: current_node

    value = 0_int64
    found = .false.

    ! Use ASSOCIATED for pointers
    if (self%table_size <= 0 .or. .not. associated(self%table)) return

    idx = self%get_index(key)
    current_node => self%table(idx)

    do while (associated(current_node))
      if (current_node%key == key) then
         value = current_node%value
         found = .true.
         return
      endif
      current_node => current_node%next
    end do
  end function dict_get

  !--------------------------------------------------------------------
  ! Check if a key exists in the dictionary (NOT PURE)
  !--------------------------------------------------------------------
   function dict_has_key(self, key) result(exists)
    ! Removed PURE due to pointer assignment current_node => ...
    class(dictionary_t), intent(in) :: self
    integer(int64), intent(in)      :: key
    logical                         :: exists
    integer                         :: idx
    type(dict_node_t), pointer :: current_node

    exists = .false.

    ! Use ASSOCIATED for pointers
    if (self%table_size <= 0 .or. .not. associated(self%table)) return

    idx = self%get_index(key)
    current_node => self%table(idx) ! Pointer assignment

    do while (associated(current_node))
      if (current_node%key == key) then
         exists = .true.
         return
      endif
      current_node => current_node%next
    end do
  end function dict_has_key

  !--------------------------------------------------------------------
  ! Delete a key-value pair
  !--------------------------------------------------------------------
  subroutine dict_delete(self, key, deleted)
    class(dictionary_t), intent(inout) :: self
    integer(int64), intent(in)      :: key
    logical, intent(out)            :: deleted
    integer                         :: idx
    type(dict_node_t), pointer :: current_node, prev_node

    deleted = .false.

    ! Use ASSOCIATED for pointers
    if (self%table_size <= 0 .or. .not. associated(self%table)) return

    idx = self%get_index(key)
    current_node => self%table(idx)
    nullify(prev_node)

    do while (associated(current_node))
      if (current_node%key == key) then
         if (associated(prev_node)) then
            prev_node%next => current_node%next
         else
            self%table(idx) => current_node%next ! Pointer assignment should be okay
         endif
         deallocate(current_node)
         self%num_elements = self%num_elements - 1
         deleted = .true.
         return
      endif
      prev_node => current_node
      current_node => current_node%next
    end do
  end subroutine dict_delete

  !--------------------------------------------------------------------
  ! Resize the hash table (doubling it)
  !--------------------------------------------------------------------
  subroutine dict_resize(self)
    class(dictionary_t), intent(inout) :: self
    integer :: old_size, new_size, i, new_idx, stat
    type(dict_node_t), pointer :: old_table_head(:)
    type(dict_node_t), pointer :: new_table_head(:)
    type(dict_node_t), pointer :: current_node, next_node

    if (.not. associated(self%table)) then
        call self%init() ! Initialize if called on unassociated table
        return
    endif
    old_size = self%table_size
    if (old_size <= 0) then
       call self%init() ! Re-initialize if size is somehow zero
       return
    endif
    new_size = old_size * 2
    ! write(*,'(A,I0,A,I0)') "Dictionary Info: Resizing from ", old_size, " to ", new_size ! Debug

    ! Allocate new table array
    allocate(new_table_head(0:new_size-1), stat=stat)
    if (stat /= 0) then
        write(*, '(A)') "Dictionary Error: Failed to allocate new table in resize"
        error stop 1
    endif
    new_table_head => null()

    ! Allocate temporary holder for old table pointers
    allocate(old_table_head(0:old_size-1))
    old_table_head = self%table ! Copy pointers

    ! Deallocate the dictionary's old table *array*
    deallocate(self%table)

    ! Point dictionary's table to the new empty table array
    self%table => new_table_head
    self%table_size = new_size
    self%num_elements = 0

    ! Rehash all elements from old table into new table
    do i = 0, old_size - 1
      current_node => old_table_head(i)
      do while (associated(current_node))
         next_node => current_node%next
         new_idx = self%get_index(current_node%key) ! Uses new table_size

         ! Prepend node to the new chain
         current_node%next => self%table(new_idx)
         self%table(new_idx) => current_node ! Pointer assignment should be okay
         self%num_elements = self%num_elements + 1
         current_node => next_node
      end do
    end do

    deallocate(old_table_head) ! Deallocate the temporary array
  end subroutine dict_resize

  !--------------------------------------------------------------------
  ! Get the current number of elements stored (PURE)
  !--------------------------------------------------------------------
  pure function dict_get_num_elements(self) result(count)
    class(dictionary_t), intent(in) :: self
    integer :: count
    count = self%num_elements
  end function dict_get_num_elements

  !--------------------------------------------------------------------
  ! Get the current internal table size (capacity) (PURE)
  !--------------------------------------------------------------------
  pure function dict_get_table_size(self) result(size)
      class(dictionary_t), intent(in) :: self
      integer :: size
      size = self%table_size
  end function dict_get_table_size

end module dictionary_module
