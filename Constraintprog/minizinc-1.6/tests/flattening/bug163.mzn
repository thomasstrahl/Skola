% RUNS ON mzn2fzn

% Regression test for bug #163.  The FlatZinc optimiser in MiniZinc was substituting
% variables in nested annotations properly.  In the model below, the "c" in the
% int_search annotation below was being replaced by an internal variable.
% (Actually, the internal variable was being introduced by the main pass of mzn2fzn
% and the optimizer then eliminated and replaced it with "c" everywhere except in
% within the search annotation.

var 1..3: a;

var int: c;
constraint c = max(a, 2);

solve :: limit(time, 42, int_search([c], input_order, indomain_max, complete))
    maximize c;
