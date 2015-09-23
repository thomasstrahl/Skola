% RUNS ON mzn2fzn

% mzn2fzn 1.1 was inferring incorrect bounds (-3..0) for the
% variable r.  The correct bounds are -3..3.

set of int: xs;
set of int: ys;

var xs: x;
var ys : y;
var int : d;
var int : r;

constraint
    x div y = d
    /\
    x mod y = r
;

solve satisfy;

output([ show(x), " div ", show(y)," = ", show(d), 
     "\n", show(x), " mod ", show(y), " = ", show(r), "\n" ]);

xs = {-7, 7};
ys = {-4, -2};
