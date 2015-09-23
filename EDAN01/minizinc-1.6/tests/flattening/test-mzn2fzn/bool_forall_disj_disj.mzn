var bool: x1;
var bool: x2;
var bool: x3;
var bool: x4;
constraint forall([x1 \/ x2, x3 \/ x4]);
solve satisfy;
