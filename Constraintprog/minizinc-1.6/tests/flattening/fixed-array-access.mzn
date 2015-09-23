% RUNS ON mzn2fzn
% RUNS ON mzn2fzn_cd
array[1..3] of int: a = [1, 2, 3];

var 1..a[2]: vi;

var int: x = a[3];

solve maximize vi;

output [
   "vi = ", show(vi), ";\n",
   "x = ", show(x), ";\n"
];
