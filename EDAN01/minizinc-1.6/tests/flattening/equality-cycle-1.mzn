% RUNS ON mzn2fzn

% Test for a special case of bug 65.
include "inverse.mzn";
int: n = 1;
array [1..n] of var int: r;
constraint inverse(r, r);
solve satisfy;
