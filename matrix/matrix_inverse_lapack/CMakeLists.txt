cmake_minimum_required(VERSION 3.1)

project(inverse_matrix)
enable_language(Fortran)

# Find Intel MKL libraries
find_package(MKL REQUIRED)

# Set compiler options
set(CMAKE_Fortran_COMPILER "ifort")
set(CMAKE_Fortran_FLAGS "-qmkl")

# Add executable
add_executable(inverse_matrix inverse.f90)

# Link with Intel MKL libraries
target_link_libraries(inverse_matrix PRIVATE ${MKL_LIBRARIES})

# Set executable output directory
set_target_properties(inverse_matrix PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# Add a custom target to run the program
add_custom_target(run COMMAND ${CMAKE_BINARY_DIR}/bin/inverse_matrix DEPENDS inverse_matrix)

# Set the working directory for the run target
set_target_properties(run PROPERTIES WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

