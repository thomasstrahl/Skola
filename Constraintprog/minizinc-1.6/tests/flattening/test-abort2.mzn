% RUNS ON mzn2fzn

% Test abort/1.
%
bool: b = abort("This assignment aborts!");
solve satisfy;
