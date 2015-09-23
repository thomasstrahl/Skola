% RUNS ON mzn_tc

constraint forall(i in 1..10)(i);		% Non-Boolean.
constraint forall(i in 10)(i > 0);		% Bad argument.
constraint forall(i in 1..10 where i) (i > 0);	% Non-Boolean where-clause.
constraint forall(i in 1..10) (i = "one");	% Type error in expression.

constraint exists(i in 1..10)(i);		% Non-Boolean.
constraint exists(i in 10)(i > 0);		% Bad argument.
constraint exists(i in 1..10 where i) (i > 0);	% Non-Boolean where-clause.
constraint exists(i in 1..10) (i = "one");	% Type error in expression.

solve satisfy;
