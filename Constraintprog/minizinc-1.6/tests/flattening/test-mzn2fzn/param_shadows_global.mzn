predicate p(var int: x) = x in 1..3;
array [1..3] of var 1..10: x;
constraint p(x[1]);
solve satisfy;
