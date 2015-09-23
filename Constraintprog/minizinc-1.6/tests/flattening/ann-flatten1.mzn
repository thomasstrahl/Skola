% RUNS ON mzn2fzn

% Test flattening of annotations.

predicate foo(int: a);
predicate foo_reif(int: a, bool: b);
annotation test_ann;

constraint foo(1) :: test_ann /\ foo(2) /\ foo(3);

constraint foo(4) :: test_ann /\ foo(5) :: test_ann /\  foo(6) :: test_ann;

constraint (foo(7) /\ foo(8)) :: test_ann;

constraint (foo(9) \/ foo(10)) :: test_ann;

constraint forall(i in 11..13) (foo(i)) :: test_ann;

constraint forall(i in 14..16) (foo(i) :: test_ann);

constraint ( if 3 = 3 then foo(17) else foo(18) endif) :: test_ann;

constraint ( if 3 = 4 then foo(19) else foo(20) endif) :: test_ann;

constraint exists(i in 21..23) (foo(i)) :: test_ann;

constraint exists(i in 24..26) (foo(i) :: test_ann);

solve satisfy;
