% RUNS ON mzn2fzn

% Regression test for bug #158 (part b).  When flattening the the application
% of q/1 to x the condition of the ite in its body was not being flattened in
% a reifying context.  This resulted in model inconsistency being erroneously
% reported.

array[1..5] of var 1..9: x;

constraint q(x);

predicate q(array[int] of var int: d) =
    if forall(i in index_set(d))(dom_size(d[i]) = 1) then true else true endif;

solve satisfy;
