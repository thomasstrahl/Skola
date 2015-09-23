% RUNS ON mzn2fzn

% Test for the general case of bug 65.
include "inverse.mzn";
int: n = 2;
array [1..n] of var int: r;
constraint inverse(r, r);
solve satisfy;
