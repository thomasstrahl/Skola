% RUNS ON mzn2fzn

% Test abort/1.
%
string: s = abort("This assignment aborts!");
solve satisfy;
