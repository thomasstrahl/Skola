% RUNS ON mzn2fzn

% Regression test for bug #170.

% r13145 and before were incorrectly reporting that the assignment
% for n was out of range.  (This was occurring because mzn2fzn was
% internally setting the value of n to the empty string before
% making the assignment.)


par string: n;
n = "Hello";
solve satisfy;
