% RUNS ON mzn2fzn

% Check that we tighten the bounds on S induced by the constraint.

var set of 1..6: S;

constraint S subset {2, 3};
solve satisfy;

output [show(S)];
