% RUNS ON mzn2fzn

% A regression test for the bug fixed in r9313.
% mzn2fzn was incorrectly improving the bounds on y based upon
% the elements of x, rather than upon the cardinality of x.
% It should be 0..4, not 2..100000.
%
var set of {2, 3, 5, 100000}: x;
var -200000..200000: y;
constraint y = card(x);
solve maximize y;
output ["y = ", show(y), ";\n"];
