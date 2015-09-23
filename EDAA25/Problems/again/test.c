# include <stdio.h>
# include <string.h>
# include <stdlib.h>
# include <stdint.h>


# define SWAP(a, b)	do {int temp = a; a = b; b = temp;}while(0);
# define M	(4)
# define N	(4)


typedef struct matrix_t matrix_t;

struct matrix_t
{
	int m;
	int n;
	double* ptr;
};


matrix_t* new_matrix(int m, int n)
{
	matrix_t* mat;
	mat = malloc(sizeof(matrix_t));
	if (mat == NULL) {
		printf("out of memory\n");
		exit(1);
	}
	mat->m = m;
	mat->n = n;
	double* ptr;
	ptr = malloc(m*n*sizeof(double));
	if (ptr == NULL) {
		printf("out of memory\n");
		exit(1);
	}
	mat->ptr = ptr;
	int i, j;
	for (i = 0; i < mat->m; ++i)
	{
		for (j = 0; j < mat->n; ++j)
		{
			mat->ptr[i*mat->n + j] = 0.0;
		}
	}

	return mat;
}

void put(matrix_t* mat, int m, int n, double data)
{
	mat->ptr[m*mat->n + n] = data;
}

double get(matrix_t* mat, int m, int n)
{
	return mat->ptr[m*mat->n + n];
}

matrix_t* add(matrix_t* a, matrix_t* b)
{
	matrix_t* c = new_matrix(a->m, a->n);
	int i, j;
	for (i = 0; i < c->m; ++i)
	{
		for (j = 0; j < c->n; ++j)
		{
			c->ptr[i*c->n +j] = a->ptr[i*c->n +j] + b->ptr[i*c->n +j];
		}
	}
	return c;
}


void my_free(matrix_t** mat)
{
	free((*mat)->ptr);
	free(*mat);
}

int main () 
{

	matrix_t* mat;
	matrix_t* b;
	b = new_matrix(5,5);
	mat = new_matrix(5, 5);
	put(mat, 2, 2, 4.0);
	put(mat, 2, 3, 7.0);
	put(b, 2, 2, 4.0);
	put(b, 2, 3, 7.0);
	matrix_t* n_mat = add(mat, b);
	int i, j;
	for (i = 0; i < n_mat->m; ++i)
	{
		for (j = 0; j < n_mat->n; ++j)
		{
			printf("	%f 		", n_mat->ptr[i*n_mat->n + j]);
		}
		printf("\n");
	}
	my_free(&mat);
	my_free(&b);
	my_free(&n_mat);
	// double (*p)[4];

	// p = malloc(M*N*sizeof(double));

	// if (p == NULL) {
	// 	printf("out of memory\n");
	// }
	// int i, j;
	// for (i = 0; i < M; ++i) {
	// 	for (j = 0; j < N; ++j) {
	// 			p[i][j] = 10.0;
	// 	}	
	// }

}


void* malloc_al(size_t n, size_t a, void** pointer)
{

	uintptr_t arith_ptr;
	void* ptr;

	n += (a-1);

	*pointer = ptr = malloc(n);
	arith_ptr = (uintptr_t)ptr;
	arith_ptr += (a - 1);
	arith_ptr &= (a - 1);
	ptr = (void*)arith_ptr;

	return ptr;


}