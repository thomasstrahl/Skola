predicate int_lt_reif(var int: x, var int: y, var bool: b);
predicate int_le_reif(var int: x, var int: y, var bool: b) =
	int_lt_reif(x, y + 1, b);
var 1..3: x1;
var bool: x2;
constraint x2 = (x1 <= 2);
solve satisfy;
