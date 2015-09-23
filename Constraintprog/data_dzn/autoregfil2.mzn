include "globals.mzn";

int: del_add;
int: del_mul;
int: number_add;
int: number_mul;
int: n;
set of int : last;
set of int: add;
set of int: mul;

int: adds = card(add);
int: muls = card(mul);

array[1..n] of set of int: dependencies;

array[1..adds] of var 0..10000: add_start;
array[1..muls] of var 0..10000: mul_start;

var 0..10000: end;

array[1..adds] of int: add_duration = array1d(1..adds, [del_add, del_add, del_add, del_add, del_add, del_add, del_add, del_add, del_add, del_add, del_add, del_add]);
array[1..muls] of int: mul_duration = array1d(1..muls, [del_mul, del_mul, del_mul, del_mul, del_mul, del_mul, del_mul, del_mul, del_mul, del_mul, del_mul, del_mul, del_mul, del_mul, del_mul, del_mul]);
array[1..adds] of int: add_resources = array1d(1..adds, [1,1,1,1,1,1,1,1,1,1,1,1]);
array[1..muls] of int: mul_resources = array1d(1..muls, [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]);

constraint cumulative(add_start, add_duration, add_resources, number_add);
constraint cumulative(mul_start, mul_duration, mul_resources, number_mul);

constraint
  forall(i in 1..muls)(
    forall(j in dependencies[i])(
    mul_start[i] + del_mul <= j
  )
);

constraint 
  forall(i in 1..adds)(
    forall(j in dependencies[i])
    (add_start[i] + del_add <= j 
  )    
);  

solve minimize max(max(a in add_start)(a+del_add), max(p in mul_start)(p + del_mul));

output[ show (end) ++ "\n" ++
        show(add_start) ++ "\n" ++
        show(mul_start)
];

