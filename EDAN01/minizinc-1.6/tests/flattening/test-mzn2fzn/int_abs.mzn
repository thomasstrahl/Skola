var -5..5: x1;
var -5..5: x2;
constraint x1 = abs(x2);
solve satisfy;
