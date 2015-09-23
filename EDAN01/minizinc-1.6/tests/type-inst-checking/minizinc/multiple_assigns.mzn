% RUNS ON mzn_tc

% Multiple assignments to unitialized variable.
int: x;
x = 1;
x = 3;

% Multiple assignments to initialized variable.
int: y = 3;
y = 4;

% Mutiple assignments to undeclared variable.
% In this case we want to report that z is undeclared,
% not that is has multiple assignments -- however, for
% this test we won't report anything because symbol
% checking is not done until later.
%
z = 4;
z = 5;

solve satisfy;
