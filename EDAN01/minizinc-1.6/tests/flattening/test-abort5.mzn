% RUNS ON mzn2fzn

% Test abort/1.
%
var bool: x = abort("This assignment aborts!");
solve satisfy;
