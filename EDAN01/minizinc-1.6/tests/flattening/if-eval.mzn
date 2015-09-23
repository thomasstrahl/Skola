% RUNS ON mzn2fzn
% RUNS ON mzn2fzn_cd
int: three = 3;

int: x1 =
	if three == 3 then 0 else 1 endif;
int: x2 =
	if three == 4 then 1 else 0 endif;

var int: y1 =
	if x1 == 0 then 0  elseif x2 == 0 then 88 else 99 endif;
var int: y2 =
	if x1 == 1 then 88 elseif x2 == 0 then 0  else 99 endif;
var int: y3 =
	if x1 == 1 then 88 elseif x2 == 1 then 99 else  0 endif;

solve satisfy;

output [
    "y1 = ", show(y1), ";\n",
    "y2 = ", show(y2), ";\n",
    "y3 = ", show(y3), ";\n"
];
