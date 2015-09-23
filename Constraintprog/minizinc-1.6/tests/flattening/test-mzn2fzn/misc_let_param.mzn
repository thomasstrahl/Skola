var 1..3: x1;
var 1..3: x2;
constraint let { var int: x3 = x1 + x1 } in x2 = x3;
solve satisfy;
