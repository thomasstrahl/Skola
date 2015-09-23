array [11..13] of var 1..3: x1;
var int: x2;
var int: x3 = x1[x2];
% constraint x3 = x1[x2];
solve satisfy;
