module dictionary_module
  use iso_fortran_env, only: int64, real64, int32
  implicit none

  private ! Default private, expose only necessary types and functions

  !--------------------------------------------------------------------
  ! Hash function for 64-bit integer (fmix64 from MurmurHash3)
  !--------------------------------------------------------------------
  interface 
  module procedure hash_integer64
  ! Constants from MurmurHash3 fmix64
  integer(kind=int64), parameter :: MH3_PRIME1 = -49064778989728563_int64  ! 0xff51afd7ed558ccd
  integer(kind=int64), parameter :: MH3_PRIME2 = -4265267296055464797_int64 ! 0xc4ceb9fe1a85ec53
  integer(kind=int64)             :: k
  integer(kind=int64), intent(in) :: key
  integer(kind=int64)             :: hash_val

  k = key
  k = ieor(k, ishft(k, -33))   ! k ^= k >> 33
  k = k * MH3_PRIME1
  k = ieor(k, ishft(k, -33))
  k = k * MH3_PRIME2
  k = ieor(k, ishft(k, -33))
  hash_val = k
  end module procedure hash_integer64

  !--------------------------------------------------------------------
  ! Linked List Node for storing key-value pairs in a chain
  !--------------------------------------------------------------------
  type :: dict_node_t
    integer(int64) :: key
    integer(int64) :: value
    type(dict_node_t), pointer :: next => null()
  end type dict_node_t

  !--------------------------------------------------------------------
  ! Dictionary Main Type
  !--------------------------------------------------------------------
  type, public :: dictionary_t
    private
    type(dict_node_t), pointer :: table(:) => null() ! Array of chain heads
    integer :: table_size = 0                        ! Must be power of 2
    integer :: num_elements = 0
    real(real64) :: max_load_factor = 0.75_real64     ! Resize threshold
    contains
    procedure, public :: init => dict_init           ! Initialize
    procedure, public :: destroy => dict_destroy       ! Clean up memory
    procedure, public :: set => dict_set             ! Add/update key-value
    procedure, public :: get => dict_get             ! Retrieve value by key
    procedure, public :: delete => dict_delete         ! Remove key-value
    procedure, public :: has_key => dict_has_key       ! Check if key exists
    procedure, public :: get_num_elements => dict_get_num_elements ! Get current count
    procedure, public :: get_table_size => dict_get_table_size     ! Get internal table size
    procedure, private :: resize => dict_resize        ! Grow table
    procedure, private :: get_index => dict_get_index  ! Map hash to table index
    procedure, private :: free_chain => dict_free_chain ! Deallocate a list
  end type dictionary_t

  contains

  !--------------------------------------------------------------------
  ! Initialize the dictionary with a suggested initial size
  !--------------------------------------------------------------------
  subroutine dict_init(self, initial_capacity)
    class(dictionary_t), intent(inout) :: self
    integer, intent(in), optional     :: initial_capacity
    integer :: actual_size
    integer :: stat

    ! Choose a power-of-2 size >= suggested capacity (default 16)
    actual_size = 16
    if (present(initial_capacity)) then
      if (initial_capacity > actual_size) then
        actual_size = 2**ceiling(log(real(initial_capacity))/log(2.0))
      endif
    endif

    if (allocated(self%table)) call self%destroy() ! Destroy if already initialized

    allocate(self%table(0:actual_size-1), stat=stat)
    if (stat /= 0) then
      print *, "Dictionary Error: Failed to allocate table in init"
      error stop 1
    endif

    self%table => null() ! Initialize all bucket pointers to null
    self%table_size = actual_size
    self%num_elements = 0
    ! print *, "Dictionary Info: Initialized with table size", self%table_size ! Debug
  end subroutine dict_init

  !--------------------------------------------------------------------
  ! Deallocate all nodes and the table itself
  !--------------------------------------------------------------------
  subroutine dict_destroy(self)
    class(dictionary_t), intent(inout) :: self
    integer :: i

    if (.not. allocated(self%table)) return ! Nothing to destroy

    do i = 0, self%table_size - 1
      if (associated(self%table(i))) then
        call self%free_chain(self%table(i))
        nullify(self%table(i)) ! Not strictly needed after free_chain, but good practice
      end if
    end do

    deallocate(self%table)
    self%table_size = 0
    self%num_elements = 0
    ! print *, "Dictionary Info: Destroyed" ! Debug
  end subroutine dict_destroy

  !--------------------------------------------------------------------
  ! Helper: Deallocate all nodes in a linked list chain
  !--------------------------------------------------------------------
  recursive subroutine dict_free_chain(node_ptr)
    type(dict_node_t), pointer, intent(inout) :: node_ptr
    type(dict_node_t), pointer :: next_node

    if (.not. associated(node_ptr)) return

    next_node => node_ptr%next
    deallocate(node_ptr)
    call dict_free_chain(next_node)
    nullify(node_ptr) ! Nullify the pointer passed in after deallocation

  end subroutine dict_free_chain

  !--------------------------------------------------------------------
  ! Calculate the table index for a given key
  !--------------------------------------------------------------------
  pure function dict_get_index(self, key) result(idx)
    class(dictionary_t), intent(in) :: self
    integer(int64), intent(in)      :: key
    integer                         :: idx
    integer(int64)                  :: h

    h = hash_integer64(key)
    ! Use bitwise AND for index calculation (requires table_size to be power of 2)
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
    type(dict_node_t), pointer :: current, new_node
    integer :: stat
    real(real64) :: load_factor

    if (self%table_size == 0) call self%init() ! Auto-initialize if needed

    idx = self%get_index(key)
    current => self%table(idx)

    ! Traverse chain to check if key exists
    do while (associated(current))
      if (current%key == key) then
        current%value = value ! Update existing key
        ! print *, "Dictionary Info: Updated key", key ! Debug
        return
      endif
      current => current%next
    end do

    ! Key not found, create and prepend new node
    allocate(new_node, stat=stat)
    if (stat /= 0) then
      print *, "Dictionary Error: Failed to allocate new node in set"
      error stop 1
    endif
    new_node%key = key
    new_node%value = value
    new_node%next => self%table(idx) ! Point new node's next to current head
    self%table(idx) => new_node      ! Point table head to new node
    self%num_elements = self%num_elements + 1
    ! print *, "Dictionary Info: Inserted key", key ! Debug

    ! Check load factor and resize if necessary
    load_factor = real(self%num_elements, real64) / real(self%table_size, real64)
    if (load_factor > self%max_load_factor .and. self%table_size > 0) then
      ! print *, "Dictionary Info: Resizing needed (load factor=", load_factor, ")" ! Debug
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
    integer(int64)                  :: value  ! Return value
    integer                         :: idx
    type(dict_node_t), pointer :: current

    value = 0_int64 ! Default return if not found
    found = .false.
    if (self%table_size == 0) return ! Dictionary not initialized

    idx = self%get_index(key)
    current => self%table(idx)

    ! Traverse chain to find key
    do while (associated(current))
      if (current%key == key) then
        value = current%value
        found = .true.
        ! print *, "Dictionary Info: Found key", key ! Debug
        return
      endif
      current => current%next
    end do
    ! print *, "Dictionary Info: Key not found", key ! Debug
  end function dict_get

  !--------------------------------------------------------------------
  ! Check if a key exists in the dictionary
  !--------------------------------------------------------------------
  function dict_has_key(self, key) result(exists)
    class(dictionary_t), intent(in) :: self
    integer(int64), intent(in)      :: key
    logical                         :: exists
    integer                         :: idx
    type(dict_node_t), pointer :: current

    exists = .false.
    if (self%table_size == 0) return

    idx = self%get_index(key)
    current => self%table(idx)

    do while (associated(current))
      if (current%key == key) then
        exists = .true.
        return
      endif
      current => current%next
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
    type(dict_node_t), pointer :: current, prev

    deleted = .false.
    if (self%table_size == 0) return

    idx = self%get_index(key)
    current => self%table(idx)
    nullify(prev)

    ! Traverse chain to find key, keeping track of previous node
    do while (associated(current))
      if (current%key == key) then
        ! Key found, now unlink and deallocate
        if (associated(prev)) then
          ! Node is not the head of the chain
          prev%next => current%next
        else
          ! Node is the head of the chain
          self%table(idx) => current%next
        endif

        deallocate(current) ! Free memory of the node being deleted
        self%num_elements = self%num_elements - 1
        deleted = .true.
        ! print *, "Dictionary Info: Deleted key", key ! Debug
        return
      endif
      prev => current
      current => current%next
    end do
    ! print *, "Dictionary Info: Key not found for deletion", key ! Debug
  end subroutine dict_delete

  !--------------------------------------------------------------------
  ! Resize the hash table (usually doubling it)
  !--------------------------------------------------------------------
  subroutine dict_resize(self)
    class(dictionary_t), intent(inout) :: self
    integer :: old_size, new_size, i, new_idx, stat
    type(dict_node_t), pointer :: old_table(:), current, next_node, new_node

    old_size = self%table_size
    if (old_size == 0) then
      call self%init() ! Initialize if called on empty/uninitialized
      return
    endif
    new_size = old_size * 2
    ! print *, "Dictionary Info: Resizing from", old_size, "to", new_size ! Debug

    ! Allocate new table
    allocate(new_node%table(0:new_size-1), stat=stat) ! Use new_node as temp type holder
    if (stat /= 0) then
      print *, "Dictionary Error: Failed to allocate new table in resize"
      ! Maybe try smaller size or just stop? Stopping for now.
      error stop 1
    endif
    new_node%table => null() ! Initialize new table pointers

    ! Swap old and new table info temporarily within self to use get_index etc.
    ! but keep old table pointer safe for iteration
    old_table => self%table
    nullify(self%table) ! Break association
    self%table => new_node%table
    nullify(new_node%table) ! Detach from temporary pointer
    self%table_size = new_size
    self%num_elements = 0 ! Will be recounted during re-insertion

    ! Rehash all elements from old table into new table
    do i = 0, old_size - 1
      current => old_table(i)
      do while (associated(current))
        next_node => current%next ! Save next pointer before modifying list

        ! Calculate index in the *new* table
        new_idx = self%get_index(current%key)

        ! Prepend current node to the new chain (avoids new allocation)
        current%next => self%table(new_idx)
        self%table(new_idx) => current

        self%num_elements = self%num_elements + 1 ! Recount elements

        current => next_node ! Move to the next node in the old chain
      end do
    end do

    ! Deallocate the old table *array* (nodes are now in new table)
    deallocate(old_table)
    ! print *, "Dictionary Info: Resize complete. Num elements:", self%num_elements ! Debug

  end subroutine dict_resize

  !--------------------------------------------------------------------
  ! Get the current number of elements stored
  !--------------------------------------------------------------------
  pure function dict_get_num_elements(self) result(count)
    class(dictionary_t), intent(in) :: self
    integer :: count
    count = self%num_elements
  end function dict_get_num_elements

  !--------------------------------------------------------------------
  ! Get the current internal table size (capacity)
  !--------------------------------------------------------------------
  pure function dict_get_table_size(self) result(size)
    class(dictionary_t), intent(in) :: self
    integer :: size
    size = self%table_size
  end function dict_get_table_size
end interface

end module dictionary_module


! --- Example Usage ---
program test_dictionary
use dictionary_module
use iso_fortran_env, only: int64
implicit none

type(dictionary_t) :: my_dict
integer(int64) :: key, value, retrieved_value
integer :: i
logical :: found, deleted

! Initialize with a small capacity (will trigger resize)
call my_dict%init(initial_capacity=4)
print *, "Initial table size:", my_dict%get_table_size()

  ! Set some values
  print *, "--- Setting Values ---"
  call my_dict%set(10_int64, 100_int64)
  call my_dict%set(20_int64, 200_int64)
  call my_dict%set(10_int64, 101_int64) ! Update key 10
  call my_dict%set(33_int64, 300_int64) ! Key that might collide with 1 depending on hash/size
  call my_dict%set(47_int64, 400_int64) ! Trigger resize maybe?
  call my_dict%set(5_int64, 500_int64)  ! Trigger resize maybe?

  print *, "Current number of elements:", my_dict%get_num_elements()
  print *, "Current table size:", my_dict%get_table_size()

  ! Get some values
  print *, "--- Getting Values ---"
  value = my_dict%get(20_int64, found)
  if (found) then
    print *, "Get key 20:", value
  else
    print *, "Get key 20: Not found"
  endif

  value = my_dict%get(10_int64, found)
  if (found) then
    print *, "Get key 10:", value
  else
    print *, "Get key 10: Not found"
  endif

  value = my_dict%get(99_int64, found) ! Key not present
  if (found) then
    print *, "Get key 99:", value
  else
    print *, "Get key 99: Not found"
  endif

  ! Check for keys
  print *, "--- Checking Keys ---"
  print *, "Has key 33?", my_dict%has_key(33_int64)
  print *, "Has key 99?", my_dict%has_key(99_int64)

  ! Delete a key
  print *, "--- Deleting Keys ---"
  call my_dict%delete(20_int64, deleted)
  print *, "Deleted key 20?", deleted
  print *, "Has key 20 after delete?", my_dict%has_key(20_int64)
  print *, "Current number of elements:", my_dict%get_num_elements()

  call my_dict%delete(99_int64, deleted) ! Key not present
  print *, "Deleted key 99?", deleted

  ! Clean up
  call my_dict%destroy()
  print *, "Destroyed dictionary."
  print *, "Current number of elements:", my_dict%get_num_elements()

end program test_dictionaryG
