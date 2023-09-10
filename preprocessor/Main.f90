program test_ifdef
  implicit none 
  
  print*, "Testing preprocessor macro"

#if IS_WINDOWS
    print*, "USE in windows only"
#endif

#ifdef _WIN32
    print*, "To use in windows"
#elif __linux__
    print*, "To use in linux"
#endif

endprogram test_ifdef


