var -5.0..5.0: x1;
var -5.0..5.0: x2;
var -5.0..5.0: x3;
constraint x3 = max(x1, x2);
solve satisfy;
