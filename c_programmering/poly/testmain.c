#include <stdio.h>
#include "poly.h"

int main(int argc, char** argv){
	poly_t* p;
	poly_t* q;
	p = new_poly_from_string("x^2 - 7x");
	q = new_poly_from_string("x^3 + x");
	print_poly(p);
	print_poly(q);
	
}