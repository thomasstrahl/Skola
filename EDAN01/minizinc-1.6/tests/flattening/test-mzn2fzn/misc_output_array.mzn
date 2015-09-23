array [1..3, 4..6] of var 1..9: x1 =
	array2d(1..3, 4..6, [i | i in 1..9]);
solve satisfy;
output ["x1 = ", show(x1), ";\n"];
