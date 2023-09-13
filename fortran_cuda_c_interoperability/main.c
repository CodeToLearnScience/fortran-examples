#include "array_sum.h"

int main()
{
  int n;
  double *x, sum;

  n=100;
  x = (double *)malloc(n*sizeof(double));
  for(int i=0; i<n; i++){
    x[i]=i;
  }

  array_sum(n, x, &sum);

  printf("Sum of array is: %lf \n ", sum);

  free(x);
  return 0;
}
