predicate p(int: y) = let { var 0..2: z } in x = y + z;
var 1..3: x;
constraint forall (i in 1..3) (p(i));
solve satisfy;
