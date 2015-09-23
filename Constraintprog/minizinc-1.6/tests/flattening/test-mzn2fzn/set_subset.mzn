var set of 1..3: x1;
var set of 1..3: x2;
var bool: x3;
constraint x3 = x1 subset x2;
solve satisfy;
