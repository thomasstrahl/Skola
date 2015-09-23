var -5..5: x1;
var -5..5: x2;
var -5..5: x3;
constraint x3 = x1 div x2;
solve satisfy;
