% RUNS ON mzn_tc

% Regresion test for bug #208: we were generating a rather cryptic
% error messages involving cycle errors for the duplicate assignment
% below.

int: a;
a = 1;
a = 1;
solve satisfy;
