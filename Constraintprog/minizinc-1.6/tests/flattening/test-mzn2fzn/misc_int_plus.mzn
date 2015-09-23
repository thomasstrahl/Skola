% Check bounds on var sums.
var 1..2: x1;
var 1..2: x2;
var 0..1: x3;
constraint x3 = x1 + x2;
solve satisfy;
