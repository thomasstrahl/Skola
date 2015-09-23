
solve satisfy;

array[1..9] of int:q = [1,2,3,4,5,6,7,8,9];
int: half = length(q) div 2;
array[1..9] of int:r = [ if i mod 2 == 1 then half + i div 2 + 1
                                    else half - i div 2 + 1 endif
                | i in 1..length(q) ];

output [show(half), "\n", show(r)];

