array [1..2, 2..3, 3..4] of var bool: x1;
var set of 0..9: x2 = index_set_1of3(x1);
var set of 0..9: x3 = index_set_2of3(x1);
var set of 0..9: x4 = index_set_3of3(x1);
solve satisfy;
