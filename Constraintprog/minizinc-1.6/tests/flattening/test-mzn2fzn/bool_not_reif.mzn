var bool: x1;
var bool: x2;
var bool: x3;
constraint x3 \/ (x1 = not x2);
solve satisfy;
