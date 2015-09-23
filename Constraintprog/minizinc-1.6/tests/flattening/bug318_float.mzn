% RUNS ON mzn2fzn

% Regression test for bug #318.

var int: var1;
var float: var2;
var float: var3;

constraint [(var2 + 1.0)][var1] = var3;

solve minimize var3;
