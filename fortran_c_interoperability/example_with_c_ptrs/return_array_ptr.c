#include <stdlib.h>

double* create_array(int size) {
    double* arr = (double*)malloc(size * sizeof(double));
    
    for (int i = 0; i < size; i++) {
        arr[i] = i * 1.5;
    }
    
    return arr;
}

void free_array(double* arr) {
    free(arr);
}
