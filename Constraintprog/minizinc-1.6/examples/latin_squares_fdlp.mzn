% RUNS ON mzn_mer_fdlp

% vim: ft=zinc ts=4 sw=4 et tw=0
% Author: Peter Stuckey
% Edited: Ralph Becket
% 
% Latin squares hybrid.

int: size = 3;

set of int: range = 1..size;

array[range, range, range] of var 0.0..1.0: x_lp;
array[range, range, range] of var 0..1: x;

constraint
    forall (i, j, k in range) (
        channel_fd_int_to_lp_float(x[i, j, k], x_lp[i, j, k])
    );

constraint forall (i, j in range) ((sum (k in range) (x[i,j,k]) = 1));
constraint forall (i, k in range) ((sum (j in range) (x[i,j,k]) = 1));
constraint forall (j, k in range) ((sum (i in range) (x[i,j,k]) = 1));

constraint forall (i, j in range) ((sum (k in range) (x_lp[i,j,k]) = 1.0));
constraint forall (i, k in range) ((sum (j in range) (x_lp[i,j,k]) = 1.0));
constraint forall (j, k in range) ((sum (i in range) (x_lp[i,j,k]) = 1.0));

solve satisfy;

output
    ["x = array3d(1..", show(size), ", 1..", show(size), ", 1..", show(size), ", ", show(x), ");\n"] ++
    ["x_lp = array3d(1..", show(size), ", 1..", show(size), ", 1..", show(size), ", ", show(x_lp), ");\n"];
