var 1..10: x1;
var bool: x2;
constraint x2 = (x1 in 3..3);
solve satisfy;
output [ show(x2), " <-> (x = ", show(x1), ")\n" ];
