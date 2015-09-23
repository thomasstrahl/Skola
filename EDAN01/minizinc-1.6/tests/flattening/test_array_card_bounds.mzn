% RUNS ON mzn2fzn

array[1..2] of var set of 1..3: d;

constraint forall(i, j in 1..2 where i != j) (
	card(d[i] intersect d[j]) <= 1
);

solve satisfy;

output ["d = ", show(d), ";\n"];

