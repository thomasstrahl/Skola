% RUNS ON mzn2fzn.

% Regression test for bug #318.

var int: var1;
var int: var2;
var int: var3;

constraint [(var2 + 1)][var1] = var3;

solve minimize var3;
