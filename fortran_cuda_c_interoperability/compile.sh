#!/bin/bash 

printf "\n"
printf "Script to compile and link a cuda function with fortran.\n\

Usage: \n
a. ./compile.sh intel to compile with ifort compiler\n\
b. ./compile.sh gfort to compile with gfortran compiler.\n\

By default gfortran compiler is used\n\n"

arg1="$1"
cc=nvcc
cf=gfortran

if [[ $arg1 == "intel" ]]; then 
  printf "Compiling with Intel compilers\n\n"
  cc=nvcc 
  cf=ifort
else 
  printf "Compiling with gcc compilers\n\n"
  cc=nvcc 
  cf=gfortran 
fi 

printf "1. This example shows how to call cuda function from fortran. We use shared \
library for linking...\n\n"

$cc array_sum_kernel.cu array_sum.cu -c -Xcompiler -fPIC
$cc array_sum_kernel.o array_sum.o -shared -o libarraysum.so
export LD_LIBRARY_PATH=$PWD:$LD_LIBRARY_PATH 
$cc main.c -L. -larraysum -o test_c.out
$cf main.F90 -L. -larraysum -o test_c_from_fortran.out
printf "1a. Running program from c code which calls cuda code\n\n"
./test_c.out
printf "\n1b. Running program from fortran code which calls cuda function\n\n"
./test_c_from_fortran.out
printf "Cleaning up....\n\n"
rm *.out *.so *.o *.mod

printf "2. The following lines show static linking of cuda and fortran code. Here \
we call c function from fortran...\n\n"

$cc -c array_sum.cu array_sum_kernel.cu 
$cf -c main.F90 -o main_f.o
#-lc for linking c-lib and -lstdc++ for c++
$cf main_f.o array_sum.o array_sum_kernel.o -o test_c_from_fortran_stat_link.out -lgcc -L/usr/local/cuda-12.2/lib64/ -lcudart -lstdc++
printf "\n2a. Running program from fortran code which calls cuda function \
(static linking)\n\n"
./test_c_from_fortran_stat_link.out
rm *.out *.o *.mod
