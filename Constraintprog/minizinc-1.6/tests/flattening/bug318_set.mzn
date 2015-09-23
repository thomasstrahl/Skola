% RUNS ON mzn2fzn

% Regression test for bug #318.

var int: var1;
var set of 1..10: var2;
var set of 1..10: var3;

constraint [(var2 union {1, 5})][var1] = var3;

solve satisfy;
