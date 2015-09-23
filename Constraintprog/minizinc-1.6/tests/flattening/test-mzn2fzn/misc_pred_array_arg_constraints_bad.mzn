predicate p1(var 2..4: p1x1);
array [1..1] of var 7..9: x1;
constraint p1(x1[1]);
solve satisfy;
