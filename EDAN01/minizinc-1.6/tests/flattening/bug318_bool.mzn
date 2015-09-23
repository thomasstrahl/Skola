% RUNS ON mzn2fzn

% Regression test for bug #318.

var int: var1;
var bool: var2;
var bool: var3;

constraint [(var2 \/ false)][var1] = var3;

solve satisfy;
