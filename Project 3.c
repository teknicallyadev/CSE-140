//partner = Jaeha Hwang


//---------------------------
//Blocking/Tiling
//used this as a reference http://www.netlib.org/utk/papers/autoblock/node2.html
//learned about #define min through stackoverflow
#define min(a,b) ((a < b) ? a : b)
void dgemm(int m, int n, float *A, float *C){ // m is rows, n is columns
//i is columns, j is rows, k is for repeating
	for(int i = 0; i < n; i += 32){ // 32 is tile size
		int itemp = min(i + 32, n); //compares i + 32 and n then returns whichever is smaller
		for(int j = 0; j < m; j += 32){ // 32 is tile size
			int jtemp = min(j + 32, m); //compares j + 32 and n then returns whichever is smaller
			for(int k = 0; k < m; k += 32){
				int ktemp = min(k + 32, m); //compares k + 32 and n then returns whichever is smaller
				for(int iclone = i; iclone < itemp; iclone++){
					for(int jclone = j; jclone < jtemp; jclone++){
						for(int kclone = k; kclone < ktemp; kclone++){
							C[(jclone * m) + kclone] += (A[(iclone * m) + kclone] * A[(iclone * m) + jclone]);
						}
					}
				}					
			}
		}
	}	
}
//---------------------------

//---------------------------
//Padding
/*#include <stdlib.h>
void dgemm( int m, int n, float *A, float *C )
{
    if(m % 2 == 0 && n % 2 == 0) //if the array has an even number of rows and columns, do nothing
    {
        for( int i = 0; i < m; i++ )
            for( int k = 0; k < n; k++ ) 
                for( int j = 0; j < m; j++ ) 
	                C[i+j*m] += A[i+k*m] * A[j+k*m];
    }
    else{
        int m2 = m;
        int n2 = n;
        //make rows and columns even if necessary
        if(m % 2 == 1){
            m2 = m + 1;
        }
        if(n % 2 == 1){
            n2 = n + 1;
        }
        //create new array with increased size
        float *B = (float*) malloc( m2 * n2 * sizeof(float) );

        //copy the array, fill the extra row / column with zeroes
        for(int copy = 0; copy < (m * n); copy++)
        {
            B[copy] = A[copy];
        }
        for(int zero = (m * n); zero < (m2 * n2); zero++)
        {
            B[zero] = 0;
        }
        //multiply
        for( int i = 0; i < m; i++ )
            for( int k = 0; k < n; k++ ) 
                for( int j = 0; j < m; j++ ) 
	                C[i+j*m] += B[i+k*m] * B[j+k*m];
    }
}*/
//---------------------------
//Unrolling
/*
void dgemm( int m, int n, float *A, float *C ){
    for( int i = 0; i < m; i++){
        for( int k = 0; k < n; k++){
            for(int j = 2; j < m; j+=3){
                int rollcount = m - j;

	            C[i + j * m] += A[i + k * m] * A[j + k * m];
	            C[i + (j-1) * m] += A[i + k * m] * A[(j-1) + k * m];
	            C[i + (j-2) * m] += A[i + k * m] * A[(j-2) + k * m];

                if(rollcount == 3){
                    C[i + (j+1) * m] += A[i + k * m] * A[(j+1) + k * m];
                    C[i + (j+2) * m] += A[i + k * m] * A[(j+2) + k * m];
                }
                else if(rollcount == 2)
                {
                    C[i + (j+1) * m] += A[i + k * m] * A[(j+1) + k * m];
                }
                
            }
        }
    }
}
*/
