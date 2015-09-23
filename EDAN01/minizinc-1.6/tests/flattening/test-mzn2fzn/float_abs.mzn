var -5.0..5.0: x1;
var -5.0..5.0: x2;
constraint x1 = abs(x2);
solve satisfy;
