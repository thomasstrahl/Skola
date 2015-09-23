% RUNS ON mzn2fzn

var int: x;
int: u = lb(x);

solve satisfy;

output [show(u), "\n"];
