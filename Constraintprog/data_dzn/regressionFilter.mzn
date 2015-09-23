include "globals.mzn";

int: number_add;
int: number_mul;

int: del_add;
int: del_mul;

int: n;
set of int: last;
set of int: add;
set of int: mul;

set of int: dependencies;

array[0..n] of var 0..1000: additions;
array[0..n] of var 0..1000: multiplications;

array[number_add] of int: add_times;
array[number_mul] of int: mul_times;

