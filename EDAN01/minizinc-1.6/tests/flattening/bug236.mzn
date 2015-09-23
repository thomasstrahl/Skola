% RUNS ON mzn2fzn

% Regression test for bug #236.
%
% The following model was flattend into:
%
%  array [1..6] of int: A = B;
%  array [1..6] of int: B = [1, 2, 3, 4, 5, 6];
%
% but parameter assignments in FlatZinc must be literals.

array[1..2, 1..3] of int: A = array2d(1..2, 1..3, B);                                                                                                                   
array[1..6] of int: B = [ 1, 2, 3, 4, 5, 6 ]; 
solve satisfy;
