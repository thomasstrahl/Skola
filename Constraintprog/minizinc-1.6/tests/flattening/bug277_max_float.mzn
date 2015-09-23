% RUNS ON mzn2fzn

var 2.0..2.0: o;
solve maximize o;
o = 2.0;
output ["o = ", show(o), "\n"];
