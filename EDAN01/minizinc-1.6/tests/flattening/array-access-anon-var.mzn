var int: x;
var int: yt;
var int: zt;

constraint [x, y][_] = z;

solve :: int_search([x, y, z], input_order, indomain_min, complete) satisfy;

output [
   "x = ", show(x), ";\n",
   "y = ", show(y), ";\n",
   "z = ", show(z), ";\n"
];
