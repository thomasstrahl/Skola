var -5..5: x1;
var -5..5: x2;
var -5..5: x3;
var bool: x4;
constraint x4 = (x3 = x1 + x2);
solve satisfy;
