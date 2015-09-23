% RUNS ON mzn2fzn

% Test abort/1.
%
set of int: s = abort("This assignment aborts!");
solve satisfy;
