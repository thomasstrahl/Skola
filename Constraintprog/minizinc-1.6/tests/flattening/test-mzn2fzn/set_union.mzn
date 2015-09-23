var set of 1..3: x1;
var set of 1..3: x2;
var set of 1..3: x3;
constraint x3 = x1 union x2;
solve satisfy;
