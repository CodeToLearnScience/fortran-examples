#include <stdio.h>
#include <stdlib.h> 

//extern "C" is required if compiling with c++ compiler 
#ifdef __cplusplus 
extern "C" double array_sum(double *a, int n);
extern "C" void sum_two_arrays(int N, double *a, double *b, double *c);
#else
double array_sum(double *a, int n);
void sum_two_arrays(int N, double *a, double *b, double *c);
#endif

