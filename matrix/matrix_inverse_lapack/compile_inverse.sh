#gfortran inverse.f90 -llapack -lblas
gfortran -c inverse.f90 LAPACK_Module.f90 -L -llapack -lblas 
gfortran inverse.o LAPACK_Module.o -o matrix_inverse.exe
