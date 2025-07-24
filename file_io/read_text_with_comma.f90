program main
  implicit none 
  integer :: unit, n_words, i, ierr, index
  character(256) :: line, str1, str2
  character(256), allocatable :: words(:), test(:)
  open(unit, file='data.txt', status='old', action='read')

  !allocate(test(12), stat=ierr)
  read(unit, '(A)') line
  !read(unit, *, advance="no")  test
    
  !print*, line 

str1 = adjustl(line)
do i=1,6
!  print*, str1
  index = scan(str1, ' ')
 ! print*, "index: ", index
  str2 = adjustl(str1(index+1:))
  str1=adjustl(str2 )
  !print*, "i= ", i, str2 
enddo 
    print*, str2, len(str2)
  !n_words = count([(line(i:i)==',', i=1,len(trim(line)))])+1
  !print*, n_words
  
!    str1 = test(7)//test(8)//test(9)//test(10)//test(11)//test(12)
!  print*, str1
  !index = scan(line, ',')
  !str1 = line(1:index-1)
  !str2 = line(index+1:)

  !allocate(words(n_words), stat=ierr)

  !print*, trim(str1), trim(str2)

 ! read(line, *, delimiter=',') (words(i), i=1, n_words)
 ! print*, words
end program main 
