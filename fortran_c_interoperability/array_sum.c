#include "array_sum.h"

double array_sum(double *a, int n){
  double temp=0.0;
  for(int i=0; i<n; i++){
    temp+=a[i];
  }
  return temp;
}

