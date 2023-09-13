#pragma once
#include <stdio.h>
#include <stdlib.h>
#include <cuda.h>
#include <cuda_runtime.h>

__global__ void array_sum_kernel(double *a, int n, double *sum);

#ifdef __cplusplus 
extern "C" void array_sum(int N, double *host_array, double *total_sum);
#else
void array_sum(int N, double *host_array, double *total_sum);
#endif
