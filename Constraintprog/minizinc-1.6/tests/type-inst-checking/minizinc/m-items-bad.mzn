% RUNS ON mzn_tc
var int:  vi = true;
var bool: vb;

vb = 3;

constraint vi;

solve maximize vb;

output vb;

predicate p(int: x) = vi;
