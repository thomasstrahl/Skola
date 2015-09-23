% RUNS ON mzn2fzn
% RUNS ON mzn2fzn_cd
array[1..20] of var 1..10: X;
var set of 1..10: m = dom(X[1]);
solve satisfy;
output ["m = ", show(m), ";\n"];
