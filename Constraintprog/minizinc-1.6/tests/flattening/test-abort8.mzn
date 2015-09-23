% RUNS ON mzn2fzn

% Test abort/1.
%
var set of 1..10: x = abort("This assignment aborts!");
solve satisfy;
