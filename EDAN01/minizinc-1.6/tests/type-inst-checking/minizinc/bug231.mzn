% RUNS ON mzn_tc

% Regression test for bug #231.  The following model caused an internal
% error in mzn2fzn instead of a proper error message being printed.
% (The cause was some overly-strict checks inside the type-checker.)

var int : max;
solve :: int_search([max], input_order, indomain, complete) satisfy;
