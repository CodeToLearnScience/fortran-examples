
program test_move_alloc
  
  implicit none
  
  type :: my_type
    character(len=256) :: char 
    logical :: logic
    real(kind=8) :: r1, r2
  end type my_type

  type(my_type), allocatable, dimension(:) :: obj1, obj2

  integer :: num_funcs=1, count=0
  ! Allocate obj1 and initialize its components
  allocate(obj1(num_funcs))
  
!  obj1(3)%r1 = 3.0
!  obj1(4)%r1 = 4.0
!  obj1(5)%r1 = 5.0

!  obj1(3)%r2 = 13.0
!  obj1(4)%r2 = 14.0
!  obj1(5)%r2 = 15.0
  
!  obj1(3)%logic = .false.
!  obj1(4)%logic = .true.
!  obj1(5)%logic = .true.

do i=1, 5
   count = count+1
   
   obj1(count)%char = 'Thimma'//'_'//iota(i)
   obj1(count)%r1 = 1.0+i
   obj1(count)%r2 = 11.0+i
   if(mod(i,2)==0) then
    obj1(count)%logic = .true.
   else
    obj1(count)%logic = .false.
   endif
   
   if(count==size(obj1)) then
    num_funcs=2*num_funcs
    allocate(obj2(num_funcs))
    obj2(1:count)=obj1 
		call move_alloc(obj2, obj1)
   endif
enddo

!   obj1(count)%char = 'Soma'
!   obj1(count)%r1 = 2.0
!   obj1(count)%r2 = 12.0
!   obj1(count)%logic = .true.
!   
!   obj1(3)%char = 'Bheema'
!   obj1(4)%char = 'Koma'
!   obj1(5)%char = 'Poma'



  ! Print the contents of obj2
  !write(*,*) "-----------------------------"
  !write(*,*) "---------OBJ1-------------"
  !write(*,*) "-----------------------------"
  !write(*,*) obj1%logic
  !write(*,*) obj1%char
  !write(*,*) obj1%r1
  !write(*,*) obj1%r2
 
 
  
  !obj2(1:size(obj1))=obj1


  ! Move the allocation of obj1%arr to obj2%arr and copy the values in order
  !call move_alloc(obj2, obj1)


  write(*,*) "-----------------------------"
  write(*,*) "---------OBJ2-------------"
  write(*,*) "-----------------------------"
  write(*,*) obj1%logic
  write(*,*) obj1%char
  write(*,*) obj1%r1
  write(*,*) obj1%r2

!contains
    !Convert int to str
    function itoa(i) result(res)
        character(:),allocatable :: res
        integer,intent(in) :: i
        character(range(i)+2) :: tmp
        write(tmp,'(i0)') i
        res = trim(tmp)
    end function

program test_move_alloc
