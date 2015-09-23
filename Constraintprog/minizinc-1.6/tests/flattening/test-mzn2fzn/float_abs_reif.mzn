var -5.0..5.0: x1;
var -5.0..5.0: x2;
var bool: x3;
constraint x3 = (x1 = abs(x2));
solve satisfy;
