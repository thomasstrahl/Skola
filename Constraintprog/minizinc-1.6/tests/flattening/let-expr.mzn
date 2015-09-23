% RUNS ON mzn2fzn

% This should not introduce an intermediate variable.

array[1..3] of var int: x;

constraint let { var int: q = x[2] + x[3] } in x[1] + q < 0;

solve satisfy;

