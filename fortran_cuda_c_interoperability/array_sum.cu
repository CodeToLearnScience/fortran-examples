#include "array_sum.h" 

void array_sum(int N, double *host_array, double *tot_sum){
  
  const int n_threads_blk=256; //number of threads per block 
  
  //calculate block size 
  int num_blks = (N + n_threads_blk - 1) / n_threads_blk;
  
  //allocate device array and copy host array data to device
  double *device_array;
  cudaMalloc((void**)&device_array, N*sizeof(double));
  cudaMemcpy(device_array, host_array, N*sizeof(double), 
      cudaMemcpyHostToDevice);

  //ptr for storing partial result on device 
  double *d_sum;
  cudaMalloc((void**)&d_sum, num_blks*sizeof(double));
  
  //host ptr for copying partial results from device 
  double *h_sum = (double*)malloc(num_blks*sizeof(double));
  
  //call cuda kernel to compute sum of an array
  array_sum_kernel<<<num_blks, n_threads_blk>>>(device_array, N, d_sum);

  //copy partial sum to gpu 
  cudaMemcpy(h_sum, d_sum, num_blks*sizeof(double), cudaMemcpyDeviceToHost);

  for(int i=0; i<num_blks; i++){
    *tot_sum+=h_sum[i];
  } 
}
