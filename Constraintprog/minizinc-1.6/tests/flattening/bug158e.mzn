% RUNS ON mzn2fzn

% Regression test for the fifth part of bug #158.
% Attempting to use fix/1 on an unfixed value should cause
% flattening to abort.

array[1..5] of var 1..9: x;

constraint p(x);

predicate p(array[int] of var int: d) =
    if forall(i in index_set(d))(fix(d[i]) = 1) then
    true else true endif;

solve satisfy;
