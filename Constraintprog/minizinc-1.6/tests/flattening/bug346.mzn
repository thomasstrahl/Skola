% RUNS ON mzn2fzn

% mzn2fzn 1.5.1 and before introduced unncessary reification variables
% when flattening this.

array[1..1] of var 0.0..1.0: x;
constraint forall(i in 1..1)
   (x[i] * x[i] = x[i]);
solve satisfy;
