% RUNS ON mzn_tc
% Scalars
var bool: x1 = true;
par int:  x2 = 3;
float:    x3 = 3.0;
1..3:     x4 = 3;
1.0..3.0: x5 = 3.0;
x3..x5:   x7 = 3.0;

% Sets
    set of 1..3: s1 = {};
var set of 1..3: s2 = {};
    set of float: s3 = {};
    set of bool:  s4 = {};

% Arrays
array[1..4] of     int:          a1 = [0,1,2,3];
array[1..4] of var int:          a2;
array[1..4] of par float:        a3 = [0.0,1.0,2.0,3.0];
array[1..4] of var float:        a4;
array[1..4] of     bool:         a5 = [true,true,true,true];
array[1..4] of var bool:         a6;
array[1..4] of     set of 1..3:  a7 = [{}, {}, {}, {}];
array[1..4] of var set of 1..3:  a8;
array[1..4] of     set of float: a9 = [{}, {}, {}, {}];
array[1..4] of     set of bool:  a10 = [{}, {}, {}, {}];
array[0..1, 0..1] of int:        a11 = array2d(0..1, 0..1, [1,2,3,4]);
array[1..4] of string:           a11b = ["", "", "", ""];

% In MiniZinc, we can have [int] array index only in predicate args.
test p(array[int] of int:a) = true;

% We can have array index ranges through a variable...
set of int: s8 = 1..10;
array[s8] of int: a12 = [1,2,3,4,5,6,7,8,9,10];

set of int: s9;
array[s9] of int: a13 = [1,2,3,4,5,6,7,8,9,10];
s9 = 1..10;

solve satisfy;

