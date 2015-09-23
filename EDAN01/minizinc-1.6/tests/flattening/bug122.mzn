% RUNS ON mzn2fzn

array[1..2] of var 1..2: a = [1, 2];
var 1..2: x;
constraint a[x] = 1;
solve satisfy;
