#include "array_sum.h"

int main()
{
  int n;
  double *x, *y, *z, sum;

  n=5;
  x = (double *)malloc(n*sizeof(double));
  y = (double *)malloc(n*sizeof(double));
  z = (double *)malloc(n*sizeof(double));
  for(int i=0; i<n; i++){
    x[i]=i;
    y[i]=i+1;
  }

  sum = array_sum(x, n);
  printf("Sum of array is: %lf \n ", sum);

  sum_two_arrays(n, x, y, z);
  
  for(int i=0; i<n; i++){
    printf("Summed array values: %lf \n", z[i]);
  }

  free(x);
  return 0;
}
