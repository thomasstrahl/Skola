var -5..5: x1;
var -5..5: x2;
var bool: x3;
constraint x3 = (x2 = -x1);
solve satisfy;
