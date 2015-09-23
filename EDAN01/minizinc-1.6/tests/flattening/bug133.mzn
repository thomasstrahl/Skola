% RUNS ON mzn2fzn

% Prior to r12450, mzn2fzn was imposing the 3..10 domain constraint on
% the x[i]-x[i-1] expressions instead of on the introduced variable.
% This could result in less propagation when the flattened model is
% evaluated.

predicate p(array[int] of var int: a);

array[1..3] of var 0..10: x;

constraint let {
    array[2..length(x)] of var 3..10: d = array1d(2..length(x), [ x[i]-x[i-1] | i in 2..length(x) ])
    } in p(d);

solve satisfy;

