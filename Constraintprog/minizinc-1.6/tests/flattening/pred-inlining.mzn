% RUNS ON mzn2fzn
% RUNS ON mzn2fzn_cd
predicate p(var int: x) = x > 3;

predicate q(var int: x) = p(x);

predicate r(var int: x);

var int: vi1;
var int: vi2;

constraint p(vi1);

constraint q(vi2);

constraint r(3 + 4);        % 'r' has no body -- call left untouched.

solve satisfy;
