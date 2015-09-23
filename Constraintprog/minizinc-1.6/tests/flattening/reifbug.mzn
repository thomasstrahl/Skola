% RUNS ON mzn2fzn

% Regression test for a problem in r14632 and before:
% When flattening the reified application of p below mzn2fzn wasn't using the
% name of the reified form of the constraint in the result.

predicate p(array[int] of var int: x);
predicate p_reif(array[int] of var int: x, var bool: b);
array[1..3] of var 1..3: x;
var bool: b;
constraint b <-> p(x);
solve satisfy;
