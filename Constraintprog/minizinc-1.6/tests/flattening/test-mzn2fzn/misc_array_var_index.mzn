array [1..3] of var 1..3: x1;
var int: x2;
var int: x3;
constraint x3 = x1[x2];
solve satisfy;
