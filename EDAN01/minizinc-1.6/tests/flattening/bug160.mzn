% RUNS ON mzn2fzn

% Regression test for bug #160.  An output_array/1 annotation was not being
% attached to the declaration for x in the generated FlatZinc.  The problem
% was that the code for collecting variables from the output expression
% neglected to process where clauses in comprehensions.

array[1..3,1..4,1..2] of var 0..1:x;

solve satisfy;

output [ show(r) ++ if k == 4 then "\n" else " " endif |
		 i in 1..3, k in 1 ..4, r in 1..2 where fix(x[i,k,r]) = 1 ]
