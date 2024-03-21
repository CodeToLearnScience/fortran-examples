cmake_minimum_required(VERSION 3.16)

project(FortranSubmodule VERSION 0.1)

enable_language(Fortran)

get_filename_component(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

add_compile_options(-Wall -Werror=array-bounds -Wextra -Wpedantic)

add_executable(submod_ex main.f90 mod_udt.f90 use_mod_udt.f90)
