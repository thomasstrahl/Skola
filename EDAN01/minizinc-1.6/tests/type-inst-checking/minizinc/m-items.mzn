% RUNS ON mzn_tc
var int:  vi = 3;
var bool: vb;

vb = true;

constraint vb;

solve maximize 2*vi;

output [show(vi) ++ "foo", "\n"];

predicate p(int: x) = vb;
