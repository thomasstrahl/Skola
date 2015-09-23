array [1..3] of var 1..3: x1;
var int: x2 = lb_array(x1);
var int: x3 = ub_array(x1);
var int: x4 = lb(x1[1]);
var int: x5 = ub(x1[1]);
solve satisfy;
