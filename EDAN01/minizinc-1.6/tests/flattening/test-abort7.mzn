% RUNS ON mzn2fzn

% Test abort/1.
%
var float: x = abort("This assignment aborts!");
solve satisfy;
