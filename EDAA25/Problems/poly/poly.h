#ifndef poly_h
#define poly_h

typedef struct poly_t poly_t;

poly_t* new_poly_from_string(const char*);
void free_poly(poly_t*);

poly_t* mul(poly_t*, poly_t*);

void print_poly(poly_t*);

#endif
