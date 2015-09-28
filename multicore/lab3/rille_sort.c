#include <assert.h>
#include <limits.h>
#include <pthread.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/times.h>
#include <sys/time.h>
#include <unistd.h>
#include <string.h>

#define MAX_THREADS	(4)
static int cmp(const void* ap, const void* bp);

struct sort_struct {
	double* base; 
	size_t n; 
	size_t s; 
	int (*cmp)(const void*, const void*);
	int threads;
};

int started_threads;
pthread_t threads[4];

static double sec(void)
{
	struct timeval	tv;
	gettimeofday(&tv, NULL);
	
	return tv.tv_sec + 1e-6 * tv.tv_usec;
}

void* sort_handler(void* data) 
{
	struct sort_struct *a = data;

	int i;

	if (a->threads > 1 && a->n > 10) {
		pthread_t sort_thread;
		double* tmp_base = a->base;
		size_t n = a->n;

		double* sort_base = malloc(n*sizeof(double));
		if (sort_base == NULL) {
			printf("out of mem\n");
			exit(1);
		}

		double pivot = tmp_base[0];

		int n1 = 0;
		int n2 = n - 1;
		int i;
		for (i = 1; i < n; ++i) {
			if (pivot >= tmp_base[i]) {
				sort_base[n1] = tmp_base[i];
				++n1;
			} else {
				sort_base[n2] = tmp_base[i];
				--n2;
			}
		}
		sort_base[n1] = pivot;
		++n1;

		memcpy(tmp_base, sort_base, n * sizeof(double));
		struct sort_struct data1 = {tmp_base, n1, sizeof(double),a->cmp, a->threads/2};
		struct sort_struct data2 = {tmp_base + n1, n - n1, sizeof(double),a->cmp, a->threads/2};

		if (pthread_create(&sort_thread, NULL, sort_handler, &data1)) {
			printf("Could not create thread\n");
			exit(1);
		}

		sort_handler(&data2);
		if (pthread_join(sort_thread, NULL)) {
			printf("Failed to join\n");
			exit(1);
		}

	} else {
		qsort(a->base, a->n, sizeof(double), a->cmp);
		return NULL;
	}


}

void par_sort(
	double*		base,	// Array to sort.
	size_t		n,	// Number of elements in base.
	size_t		s,	// Size of each element.
	int		(*my_cmp)(const void*, const void*)) // Behaves like strcmp
{
	struct sort_struct data = {(double*)base, n, sizeof(base[0]), my_cmp, MAX_THREADS};
	sort_handler(&data);
}

static int cmp(const void* ap, const void* bp)
{	
	/* you need to modify this function to compare doubles. */
	double a = *(double*)ap;
	double b = *(double*)bp;

	double diff = a - b;

	if (diff > 0)
		return 1;
	else if (diff < 0) 
		return -1;
	return 0;
}

int main(int ac, char** av)
{
	int		n = 2000000;
	int		i;
	double*		a;
	double		start, end;

	if (ac > 1)
		sscanf(av[1], "%d", &n);

	srand(getpid());

	a = malloc(n * sizeof a[0]);
	for (i = 0; i < n; i++)
		a[i] = rand();

	start = sec();

#ifdef PARALLEL
	par_sort(a, n, sizeof a[0], cmp);
	//printf("sizeof double: %zu\n", sizeof a[0]);
#else
	qsort(a, n, sizeof(a[0]), cmp);
#endif

	end = sec();

	//printf("Done! printing result\n");
	// for (i = 0; i < n; i++) {
	// 	printf("%lf\n", a[i]);
	// }
	printf("%1.2f s\n", end - start);

	free(a);

	return 0;
}
