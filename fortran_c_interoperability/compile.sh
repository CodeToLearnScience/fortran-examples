#!/bin/bash 

printf "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"
printf "Script to compile and link a c function with fortran.\n \
Usage: \n a. ./compile.sh intel to compile with intel compilers\n \
b. ./compile.sh gcc to compile with gcc compilers.\n \
By default gcc compiler is used\n"
printf "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"

arg1="$1"
cc=gcc
cf=gfortran

if [ $arg1 == "intel" ]; then 
  printf "Compiling with Intel compilers\n"
  cc=icc 
  cf=ifort
else 
  printf "Compiling with gcc compilers\n"
  cc=gcc 
  cf=gfortran 
fi 

printf "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"
printf "This example shows how to call c function from fortran. We use shared \
library for linking...\n"
printf "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"

$cc array_sum.c -c -fPIC
$cc array_sum.o -shared -o libarraysum.so
export LD_LIBRARY_PATH=$PWD:$LD_LIBRARY_PATH 
$cc main.c -L. -larraysum -o test_c.out
$cf main.F90 -L. -larraysum -o test_c_from_fortran.out
printf "Running program from pure c code\n"
./test_c.out
printf "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"
printf "Running program from fortran code which calls c function\n"
./test_c_from_fortran.out
printf "Cleaning up....\n"
rm *.out *.so *.o *.mod

printf "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"
printf "The following lines show static linking of c and fortran code. Here \
we call c function from fortran...\n"

$cc -c array_sum.c -o array_sum.o
$cf -c main.F90 -o main_f.o
#-lc for linking c-lib and -lstdc++ for c++
$cf main_f.o array_sum.o -o test_c_from_fortran_stat_link.out -lc 
printf "Running program from fortran code which calls c function \
(static linking)\n"
./test_c_from_fortran_stat_link.out
rm *.out *.o *.mod
