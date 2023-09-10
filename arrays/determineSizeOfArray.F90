program test_size
        Integer:: i(9)
        Real(kind=8), dimension(:), allocatable :: row
        write(*,*) size((/1,2/))
        write(*,*) size(i)

        allocate(row(69))
        write(*,*) size(row)
end program
