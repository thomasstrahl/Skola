% RUNS ON mzn2fzn

% Regression test for bug #149.
% mzn2fzn 1.1.5 was inferring incorrect bounds for the elements of jts.
%-----------------------------------------------------------------------------%

array[1..3, 1..3] of var 0.. 7773: jts;
array[1..9] of var 0.. 7773: myarray = array1d(1..9, jts);

constraint jts[3, 2] + 69 <= jts[3, 3];

solve :: int_search(myarray, input_order, indomain_split, complete) satisfy;

output ["jts = ", show(jts), ";\n"];

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
