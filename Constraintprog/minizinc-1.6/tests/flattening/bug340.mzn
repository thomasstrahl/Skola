% RUNS ON mzn2fzn

% Regression test for bug 340.
%
% mzn2fzn was flattening xorall/1 and iffall/1 incorrectly when they appeared
% in a reified context with an array argument containing at least two variables
% and at least one literal "true".

var bool: x1;
var bool: x2;
var bool: x3;
var bool: x4;
constraint x1 <-> iffall([x2, true, x3, x4]);

var bool: x5;
var bool: x6;
var bool: x7;
array [1..5] of var bool: x8 = [x6, true, true, x7, true];
constraint x5 <-> xorall(x8);

solve satisfy;
