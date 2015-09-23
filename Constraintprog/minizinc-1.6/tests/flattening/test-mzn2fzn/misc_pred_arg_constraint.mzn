predicate p1(var 2..4: p1x1) = 3 <= p1x1;
var 1..3: x1;
constraint p1(x1);
solve satisfy;
