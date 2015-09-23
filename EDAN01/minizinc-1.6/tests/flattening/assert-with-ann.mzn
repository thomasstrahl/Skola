% RUNS ON mzn2fzn
% RUNS ON mzn2fzn_cd
annotation foo;

var 1..10: x;

constraint assert(true, "blah blah", x != 1) :: foo;

solve satisfy;

output ["x = ", show(x), ";\n"];
