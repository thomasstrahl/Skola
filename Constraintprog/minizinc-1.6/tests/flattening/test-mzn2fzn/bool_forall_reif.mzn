var bool: x1;
var bool: x2;
var bool: x3;
constraint x3 \/ (forall([x1, x2]));
solve satisfy;
