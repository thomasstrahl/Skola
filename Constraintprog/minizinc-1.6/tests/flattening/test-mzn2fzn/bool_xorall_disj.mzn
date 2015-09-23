var bool: x1;
var bool: x2;
var bool: x3;
constraint xorall([x1 \/ x2, x3]);
solve satisfy;
