% RUNS ON mzn2fzn

% Test abort/1.
%
float: f = abort("This assignment aborts!");
solve satisfy;
