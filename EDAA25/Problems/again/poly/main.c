#include <stdbool.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "poly.h"

char*	progname;

static void poly_test(const char* a, const char* b)
{
	poly_t*		p;	/* Polynomial made from A. */
	poly_t*		q;	/* Polynomial made from B. */
	poly_t*		r;	/* Product of P and Q. */

	printf("Begin polynomial test of (%s) * (%s)\n", a, b);

	p = new_poly_from_string(a);
	q = new_poly_from_string(b);

	print_poly(p);
	print_poly(q);

	r = mul(p, q);

	print_poly(r);

	free_poly(p);
	free_poly(q);
	free_poly(r);

	printf("End polynomial test of (%s) * (%s)\n\n\n", a, b);
}

int main(int argc, char** argv)
{
	progname = argv[0];

	poly_test("x^2 - 70x + 1", "3x + 200");
	poly_test("x^10000000 + 2", "2x^2 + 3x + 100");

	return 0;
}
