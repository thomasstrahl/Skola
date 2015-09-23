% RUNS ON mzn_tc
% Things that work in Zinc but not in MiniZinc.
var int:   vi;
par float: pf  = 3;                 % no int->float auto-coercion
var float: vf1 = 3;                 % no int->float auto-coercion
var float: vf2 = vi;                % no int->float auto-coercion

array[0..3] of int: api = {1,2,3};  % no set->array auto-coercion

int: x1 = length([[1,2,3], [4,5,6]]);   % no arrays-of-arrays
int: x2 = length([[i] | i in 1..5]);    % no arrays-of-arrays
int: x3 = card({{1}, {2}, {3,4}});      % no sets-of-sets
int: x4 = card({[1], [2], [3,4]});      % no sets-of-arrays
int: x5 = card({"one", "two"});         % no sets-of-strings
int: x6 = card({{} | i in 1..5});       % no sets-of-sets

% Zinc-only tuple expression, tuple access expression.
int: i1 = (1,2,3).1;

% Zinc-only record expression, record access expression.
int: i2 = (x:1, y:2).x;

% indexed array literal
array[1..3] of int: i5 = [1:1, 2:2, 3:3];

% indexed array comprehension
array[1..5] of int: i6 = [i:i | i in 1..5];

solve satisfy;
