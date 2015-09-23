% RUNS ON mzn2fzn

var 2..2: o;
solve maximize o;
o = 2;
output ["o = ", show(o), "\n"];
