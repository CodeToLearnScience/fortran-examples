#include "array_sum.h"


void sum_two_arrays(int N, double *a, double *b, double *c){
  for(int i=0; i<N; i++){
    c[i] = a[i] + b[i];
  }
}
