% RUNS ON mzn2fzn
% Regression test for the first part of bug #68.
% mzn2fzn should reject this since the use of index would result
% in a set of tuple(int, int).

int: N = 3;
array[1..N, 1..N] of var 1 .. N: a;
constraint forall([ a[i] = array2d(1..2, 1..2, [_, _ , _, _])[i] | i in index_set(a) ]);
solve satisfy;
