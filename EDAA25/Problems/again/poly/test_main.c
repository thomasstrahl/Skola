# include "poly.h"


int main () 
{
	poly_t* p;
	poly_t* q;
	p = new_poly_from_string("x^2 - 70x + 1", "3x + 200");
	q = new_poly_from_string("x^10000000 + 2", "2x^2 + 3x + 100");
	return 0;
}