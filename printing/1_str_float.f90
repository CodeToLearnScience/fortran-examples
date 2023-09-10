PROGRAM example
  CHARACTER(LEN=20) :: name = 'John Doe'
  REAL :: x = 3.14159
    WRITE(*, '(A20, F10.5)') name, x
END PROGRAM

