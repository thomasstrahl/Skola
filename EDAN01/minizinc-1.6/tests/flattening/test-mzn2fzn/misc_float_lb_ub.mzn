array [1..3] of var 1.0..3.0: x1;
var float: x2 = lb_array(x1);
var float: x3 = ub_array(x1);
var float: x4 = lb(x1[1]);
var float: x5 = ub(x1[1]);
solve satisfy;
