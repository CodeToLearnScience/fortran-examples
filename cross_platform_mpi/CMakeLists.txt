cmake_minimum_required(VERSION 3.16)

project(CrossPlatformFromVS VERSION 0.1)

enable_language(Fortran)
find_package(MPI REQUIRED)
include_directories(${MPI_Fortran_INCLUDE_PATH})

if(WIN32)
	set(CMAKE_Fortran_COMPILER "%install_path%/Intel/oneAPI/compiler/2022.1.0/windows/bin/intel64/ifort.exe")
endif(WIN32)

get_filename_component(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

add_compile_options(-Wall -Werror=array-bounds -Wextra -Wpedantic)

add_executable(app cal_area.f90 module_shape.f90 ${MPI_Fortran_LIBRARIES})
