% RUNS ON mzn2fzn

array [1..2] of var set of 1..3: xs;

constraint forall (i in 1..2) (card(xs[i]) = 2);

solve satisfy;
