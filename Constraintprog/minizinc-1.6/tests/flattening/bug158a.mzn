% RUNS ON mzn2fzn

% Regression test for bug #158 (part a).  mzn2fzn did not implement
% the dom_size operation for integers.

array[1..5] of int: f = [1,2,4,5,1];

constraint q(f);

predicate q(array[int] of var int: d) =
    if forall(i in index_set(d))(dom_size(d[i]) = 1) then true else true endif;

solve satisfy;
