% RUNS ON mzn2fzn

% Test flattening of annotations.

array[1..3] of var -100..100: x;
array[1..3] of var -100..100: y;

annotation test_ann;

constraint forall(i in 1..3) (
	(x[i] = y[i] + i) :: test_ann
);

solve satisfy;
