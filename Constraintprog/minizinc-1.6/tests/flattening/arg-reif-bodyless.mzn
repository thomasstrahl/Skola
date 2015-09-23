% RUNS ON mzn2fzn

% Test that constraints induced by predicate parameter types are flattened
% correctly in reified contexts where we have a bodyless specialised reified
% form of the predicate.

var 1..3: a;

predicate test_pred(1..10: x, var int: y);
predicate test_pred_reif(1..10: x, var int: y);

constraint test_pred(11, a) == false;

solve satisfy;

output ["a = ", show(a), ";\n"];
