cmake_minimum_required (VERSION 3.22)
project ("Test")
enable_language(Fortran)

add_executable(TestPreProcess Main.f90)

if(UNIX)
  target_compile_definitions(TestPreProcess PRIVATE LIS_WINDOWS=0 __linux__)
elseif(WIN32)
  target_compile_definitions(TestPreProcess PRIVATE LIS_WINDOWS=1 _WIN32)
endif()



