#include "array_sum.h"

__global__ void array_sum_kernel(double *a, int n, double *sum){

  int tid = threadIdx.x + blockIdx.x * blockDim.x;
  double temp=0.0;

  //calculate partial sum 
  while (tid < n){
    temp += a[tid];
    tid += blockDim.x*gridDim.x;
  }

  //store the partial sum in shared memory 
  __shared__ double part_sum[256];
  part_sum[threadIdx.x] = temp;
  __syncthreads();

  //reduce partial sum 
  int i=blockDim.x/2;
  while(i != 0 ){
    if(threadIdx.x < i) {
      part_sum[threadIdx.x] += part_sum[threadIdx.x + i];
    }
    __syncthreads();
    i /= 2;
  }

  //Write final result to sum 
  if(threadIdx.x == 0){
    sum[blockIdx.x] = part_sum[0];
  }
}

