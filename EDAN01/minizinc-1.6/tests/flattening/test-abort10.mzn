% RUNS ON mzn2fzn

% Test abort/1 in predicates.
%
predicate foo(int: x) = abort("foo/1 aborted");

constraint foo(10);
solve satisfy;
