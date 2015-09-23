array [11..13] of var 1..3: x1;
var int: x2;
constraint x2 = x1[13];
solve satisfy;
